package PVE::HA::CRM;

# Cluster Resource Manager

use strict;
use warnings;

use List::Util qw(any);

use PVE::Tools;
use PVE::HA::Tools;

use PVE::HA::Manager;

# Server can have several state:

my $valid_states = {
    wait_for_quorum => "cluster is not quorate, waiting",
    master => "quorate, and we got the ha_manager lock",
    lost_manager_lock => "we lost the ha_manager lock (watchdog active)",
    slave => "quorate, but we do not own the ha_manager lock",
};

# The CRM takes ~10s per 'active' round, so if no services is available for >= 15 min we'd go in
# wait state giving up the watchdog and the CRM lock voluntary, ensuring the WD can do no harm and
# another CRM can be come active faster, if HA services get configured again.
# This is explictily 30 rounds (5 min) longer than LRM waits to avoid that LRM gets stuck due to no
# CRM being active.
my $max_active_idle_rounds = 90;

sub new {
    my ($this, $haenv) = @_;

    my $class = ref($this) || $this;

    my $self = bless {
        haenv => $haenv,
        manager => undef,
        status => { state => 'startup' },
        cluster_state_update => 0,
        active_idle_rounds => 0,
    }, $class;

    $self->set_local_status({ state => 'wait_for_quorum' });

    return $self;
}

sub shutdown_request {
    my ($self) = @_;

    $self->{haenv}->log('info', "server received shutdown request")
        if !$self->{shutdown_request};

    $self->{shutdown_request} = 1;
}

sub get_local_status {
    my ($self) = @_;

    return $self->{status};
}

sub set_local_status {
    my ($self, $new) = @_;

    die "invalid state '$new->{state}'" if !$valid_states->{ $new->{state} };

    my $haenv = $self->{haenv};

    my $old = $self->{status};

    # important: only update if if really changed
    return if $old->{state} eq $new->{state};

    $haenv->log('info', "status change $old->{state} => $new->{state}");

    $new->{state_change_time} = $haenv->get_time();

    $self->{status} = $new;

    # fixme: do not use extra class
    if ($new->{state} eq 'master') {
        $self->{manager} = PVE::HA::Manager->new($haenv);
    } else {
        if ($self->{manager}) {
            # fixme: what should we do here?
            $self->{manager}->cleanup();
            $self->{manager} = undef;
        }
    }
}

sub get_protected_ha_manager_lock {
    my ($self) = @_;

    my $haenv = $self->{haenv};

    my $count = 0;
    my $starttime = $haenv->get_time();

    for (;;) {

        if ($haenv->get_ha_manager_lock()) {
            if ($self->{ha_manager_wd}) {
                $haenv->watchdog_update($self->{ha_manager_wd});
            } else {
                my $wfh = $haenv->watchdog_open();
                $self->{ha_manager_wd} = $wfh;
            }
            return 1;
        }

        last if ++$count > 5; # try max 5 time

        my $delay = $haenv->get_time() - $starttime;
        last if $delay > 5; # for max 5 seconds

        $haenv->sleep(1);
    }

    return 0;
}

# NOTE: This is disabling the self-fence mechanism for the CRM, which has almost *no* safety or
# intergrity implications for the HA stack. The reason for this is that fencing is protecting
# against running the same services multiple times in a cluster, e.g. due to split brain, which then
# can cause corruption, especially of shared resources. But as the CRM itself does not run any
# resources, but only handles delegation and orchestration of them, disabling fencing in the CRM is
# fine as long as we do not touch the manager HA state anymore, which normaly means transition out
# of the "master" (= active manager) state in our FSM.
#
# If we would actually always fence we might reset a node that currently has no active HA service
# and thus an idle LRM that does not need fencing. This then could make a partial outage even worse,
# as one nodes less is available in such a situation.
my sub give_up_watchdog_protection {
    my ($self) = @_;

    if ($self->{ha_manager_wd}) {
        $self->{haenv}->watchdog_close($self->{ha_manager_wd});
        delete $self->{ha_manager_wd}; # only delete after close!
    }
}

my sub get_manager_status_guarded {
    my ($haenv) = @_;

    my $manager_status = eval { $haenv->read_manager_status() };
    $haenv->log('err', "could not read manager status: $@") if $@;

    return $manager_status;
}

sub is_cluster_and_ha_healthy {
    my ($self, $manager_status) = @_;

    my $haenv = $self->{haenv};

    return 0 if !$haenv->quorate();

    # we may not do any active work with an incosistent cluster state
    return 0 if !$self->{cluster_state_update};

    return 0 if !defined($manager_status);

    # there might be services to fence even if service config is completely empty
    return 0
        if PVE::HA::Tools::count_fenced_services($manager_status->{service_status},
            $haenv->nodename());

    return 1;
}

# checks quorum, for no active pending fence jobs and if services are configured
sub can_get_active {
    my ($self, $allow_no_service) = @_;

    my $haenv = $self->{haenv};

    my $manager_status = get_manager_status_guarded($haenv);
    return 0 if !$self->is_cluster_and_ha_healthy($manager_status);

    if (!$allow_no_service) {
        my $conf = eval { $haenv->read_service_config() };
        if (my $err = $@) {
            $haenv->log('err', "could not read service config: $err");
            return undef;
        }
        if (!scalar(%{$conf})) {
            my $active_master_node = $manager_status->{master_node};
            if (
                !$active_master_node
                || $manager_status->{node_status}->{$active_master_node} ne 'online'
                || $active_master_node eq $haenv->nodename()
            ) {
                my $ns = $manager_status->{node_status};
                for my $node (keys $ns->%*) {
                    next if $ns->{$node} ne 'maintenance';
                    if (
                        !$manager_status->{node_request}
                        || !$manager_status->{node_request}->{$node}
                        || !$manager_status->{node_request}->{$node}->{maintenance}
                    ) {
                        return 1;
                    }
                }
                if ($haenv->any_pending_crm_command()) {
                    return 1; # process pending CRM commands
                }
            }
            return 0; # no services, no node in maintenance mode, and no crm cmds -> can stay idle
        }
    }

    return 1;
}

# checks if there are safe conditions to get idle, like being quorate and having no active
# services. This method should be debounced over a few minutes to avoid flapping between active and
# idle.
#
# This is similar to 'can_get_active' but inverts the service count and check.
sub allowed_to_get_idle {
    my ($self) = @_;

    my $haenv = $self->{haenv};

    my $manager_status = get_manager_status_guarded($haenv);
    return 0 if !$self->is_cluster_and_ha_healthy($manager_status);

    my $conf = eval { $haenv->read_service_config() };
    if (my $err = $@) {
        $haenv->log('err', "could not read service config: $err");
        return undef;
    }
    return 1 if !scalar(%{$conf});

    # TODO: an exception might be if all services are requested to be ignored? Would need LRM
    # adaption too though.

    return 0;
}

sub do_one_iteration {
    my ($self) = @_;

    my $haenv = $self->{haenv};

    $haenv->loop_start_hook();

    $self->{cluster_state_update} = $haenv->cluster_state_update();

    my $res = $self->work();

    $haenv->loop_end_hook();

    return $res;
}

sub work {
    my ($self) = @_;

    my $haenv = $self->{haenv};

    my $status = $self->get_local_status();
    my $state = $status->{state};

    # do state changes first

    if ($state eq 'wait_for_quorum') {

        if ($self->can_get_active()) {
            if ($self->get_protected_ha_manager_lock()) {
                $self->set_local_status({ state => 'master' });
            } else {
                $self->set_local_status({ state => 'slave' });
            }
        }

    } elsif ($state eq 'slave') {

        if ($self->can_get_active()) {
            if ($self->get_protected_ha_manager_lock()) {
                $self->set_local_status({ state => 'master' });
            }
        } else {
            $self->set_local_status({ state => 'wait_for_quorum' });
        }

    } elsif ($state eq 'lost_manager_lock') {

        if ($self->can_get_active(1)) {
            if ($self->get_protected_ha_manager_lock()) {
                $self->set_local_status({ state => 'master' });
            }
        }

    } elsif ($state eq 'master') {

        if (!$self->get_protected_ha_manager_lock()) {
            $self->set_local_status({ state => 'lost_manager_lock' });
        } elsif ($self->allowed_to_get_idle()) {
            $self->{active_idle_rounds}++;
            if ($self->{active_idle_rounds} > $max_active_idle_rounds) {
                $self->{active_idle_rounds} = 0;
                $haenv->log(
                    'info',
                    "cluster had no service configured for $max_active_idle_rounds rounds, going idle.\n",
                );
                $haenv->release_ha_manager_lock();
                # safety: transition into wait_for_quorum and thus do not touch manager state anymore
                give_up_watchdog_protection($self);
                $self->set_local_status({ state => 'wait_for_quorum' });
            }
        } elsif ($self->{active_idle_rounds}) {
            $self->{active_idle_rounds} = 0;
        }
    }

    $status = $self->get_local_status();
    $state = $status->{state};

    # do work

    if ($state eq 'wait_for_quorum') {

        return 0 if $self->{shutdown_request};

        $haenv->sleep(5);

    } elsif ($state eq 'master') {

        my $manager = $self->{manager};

        die "no manager" if !defined($manager);

        my $startime = $haenv->get_time();

        my $max_time = 10;

        my $shutdown = 0;

        # do work (max_time seconds)
        eval {
            # fixme: set alert timer

            if ($self->{shutdown_request}) {

                # safety: service gets shutdown and thus won't change manager state again.
                give_up_watchdog_protection($self);

                # release the manager lock, so another CRM slave can get it
                # and continue to work without waiting for the lock timeout
                $haenv->log('info', "voluntary release CRM lock");
                if (!$haenv->release_ha_manager_lock()) {
                    $haenv->log('notice', "CRM lock release failed, let the lock timeout");
                }

                $shutdown = 1;

            } else {
                if (!$self->{cluster_state_update}) {
                    # update failed but we could still renew our lock (cfs restart?),
                    # safely skip manage and expect to update just fine next round
                    $haenv->log(
                        'notice',
                        "temporary inconsistent cluster state (cfs restart?), skip round",
                    );
                    return;
                }

                $manager->manage();
            }
        };
        if (my $err = $@) {
            $haenv->log('err', "got unexpected error - $err");
        }

        return 0 if $shutdown;

        $haenv->sleep_until($startime + $max_time);

    } elsif ($state eq 'lost_manager_lock') {

        # safety: not touching manager state in this or next FSM state
        give_up_watchdog_protection($self);

        return 0 if $self->{shutdown_request};

        $self->set_local_status({ state => 'wait_for_quorum' });

    } elsif ($state eq 'slave') {

        return 0 if $self->{shutdown_request};

        # wait until we get master

    } else {

        die "got unexpected status '$state'\n";
    }

    return 1;
}

1;
