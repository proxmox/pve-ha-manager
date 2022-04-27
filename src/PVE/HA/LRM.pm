package PVE::HA::LRM;

# Local Resource Manager

use strict;
use warnings;
use POSIX qw(:sys_wait_h);

use PVE::SafeSyslog;
use PVE::Tools;
use PVE::HA::Tools ':exit_codes';
use PVE::HA::Resources;

# Server can have several states:

my $valid_states = {
    wait_for_agent_lock => "waiting for agent lock",
    active => "got agent_lock",
    maintenance => "going into maintenance",
    lost_agent_lock => "lost agent_lock",
};

# we sleep ~10s per 'active' round, so if no services is available for >= 10 min we'd go in wait
# state giving up the watchdog and the LRM lock voluntary, ensuring the WD can do no harm
my $max_active_idle_rounds = 60;

sub new {
    my ($this, $haenv) = @_;

    my $class = ref($this) || $this;

    my $self = bless {
	haenv => $haenv,
	status => { state => 'startup' },
	workers => {},
	results => {},
	restart_tries => {},
	shutdown_request => 0,
	shutdown_errors => 0,
	# mode can be: active, reboot, shutdown, restart
	mode => 'active',
	cluster_state_update => 0,
	active_idle_rounds => 0,
    }, $class;

    $self->set_local_status({ state => 	'wait_for_agent_lock' });

    return $self;
}

sub shutdown_request {
    my ($self) = @_;

    return if $self->{shutdown_request}; # already in shutdown mode

    my $haenv = $self->{haenv};

    my $nodename = $haenv->nodename();

    my ($shutdown, $reboot) = $haenv->is_node_shutdown();

    my $dc_ha_cfg = $haenv->get_ha_settings();
    my $shutdown_policy = $dc_ha_cfg->{shutdown_policy} // 'conditional';

    if ($shutdown) { # don't log this on service restart, only on node shutdown
	$haenv->log('info', "got shutdown request with shutdown policy '$shutdown_policy'");
    }

    my $freeze_all;
    my $maintenance;
    if ($shutdown_policy eq 'conditional') {
	$freeze_all = $reboot;
    } elsif ($shutdown_policy eq 'freeze') {
	$freeze_all = 1;
    } elsif ($shutdown_policy eq 'failover') {
	$freeze_all = 0;
    } elsif ($shutdown_policy eq 'migrate') {
	$maintenance = 1;
    } else {
	$haenv->log('err', "unknown shutdown policy '$shutdown_policy', fall back to conditional");
	$freeze_all = $reboot;
    }

    if ($maintenance) {
	# we get marked as unaivalable by the manager, then all services will
	# be migrated away, we'll still have the same "can we exit" clause than
	# a normal shutdown -> no running service on this node
	# FIXME: after X minutes, add shutdown command for remaining services,
	# e.g., if they have no alternative node???
    } elsif ($shutdown) {
	# *always* queue stop jobs for all services if the node shuts down,
	# independent if it's a reboot or a poweroff, else we may corrupt
	# services or hinder node shutdown
	my $ss = $self->{service_status};

	foreach my $sid (keys %$ss) {
	    my $sd = $ss->{$sid};
	    next if !$sd->{node};
	    next if $sd->{node} ne $nodename;
	    # Note: use undef uid to mark shutdown/stop jobs
	    $self->queue_resource_command($sid, undef, 'request_stop');
	}
    }

    if ($shutdown) {
	my $shutdown_type = $reboot ? 'reboot' : 'shutdown';
	if ($maintenance) {
	    $haenv->log('info', "$shutdown_type LRM, doing maintenance, removing this node from active list");
	    $self->{mode} = 'maintenance';
	} elsif ($freeze_all) {
	    $haenv->log('info', "$shutdown_type LRM, stop and freeze all services");
	    $self->{mode} = 'restart';
	} else {
	    $haenv->log('info', "shutdown LRM, stop all services");
	    $self->{mode} = 'shutdown';
	}
    } else {
	$haenv->log('info', "restart LRM, freeze all services");
	$self->{mode} = 'restart';
    }

    $self->{shutdown_request} = $haenv->get_time();

    eval { $self->update_lrm_status() or die "not quorate?\n"; };
    if (my $err = $@) {
	$haenv->log('err', "unable to update lrm status file - $err");
    }
}

sub get_local_status {
    my ($self) = @_;

    return $self->{status};
}

sub set_local_status {
    my ($self, $new) = @_;

    die "invalid state '$new->{state}'" if !$valid_states->{$new->{state}};

    my $haenv = $self->{haenv};

    my $old = $self->{status};

    # important: only update if if really changed
    return if $old->{state} eq $new->{state};

    $haenv->log('info', "status change $old->{state} => $new->{state}");

    $new->{state_change_time} = $haenv->get_time();

    $self->{status} = $new;
}

sub update_lrm_status {
    my ($self) = @_;

    my $haenv = $self->{haenv};

    return 0 if !$haenv->quorate();

    my $lrm_status = {
	state => $self->{status}->{state},
	mode => $self->{mode},
	results => $self->{results},
	timestamp => $haenv->get_time(),
    };

    eval { $haenv->write_lrm_status($lrm_status); };
    if (my $err = $@) {
	$haenv->log('err', "unable to write lrm status file - $err");
	return 0;
    }

    return 1;
}

sub update_service_status {
    my ($self) = @_;

    my $haenv = $self->{haenv};

    my $ms = eval { $haenv->read_manager_status(); };
    if (my $err = $@) {
	$haenv->log('err', "updating service status from manager failed: $err");
	return undef;
    } else {
	$self->{service_status} = $ms->{service_status} || {};
	my $nodename = $haenv->nodename();
	$self->{node_status} = $ms->{node_status}->{$nodename} || 'unknown';
	return 1;
    }
}

sub get_protected_ha_agent_lock {
    my ($self) = @_;

    my $haenv = $self->{haenv};

    my $count = 0;
    my $starttime = $haenv->get_time();

    for (;;) {

	if ($haenv->get_ha_agent_lock()) {
	    if ($self->{ha_agent_wd}) {
		$haenv->watchdog_update($self->{ha_agent_wd});
	    } else {
		my $wfh = $haenv->watchdog_open();
		$self->{ha_agent_wd} = $wfh;
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

# only cares if any service has the local node as their node, independent of which req.state it is
sub has_configured_service_on_local_node {
    my ($self) = @_;

    my $haenv = $self->{haenv};
    my $nodename = $haenv->nodename();

    my $ss = $self->{service_status};
    foreach my $sid (keys %$ss) {
	my $sd = $ss->{$sid};
	next if !$sd->{node} || $sd->{node} ne $nodename;

	return 1;
    }
    return 0;
}

sub is_fence_requested {
    my ($self) = @_;

    my $haenv = $self->{haenv};

    my $nodename = $haenv->nodename();
    my $ss = $self->{service_status};

    my $fenced_services = PVE::HA::Tools::count_fenced_services($ss, $nodename);

    return $fenced_services || $self->{node_status} eq 'fence';
}

sub active_service_count {
    my ($self) = @_;

    my $haenv = $self->{haenv};
    my $nodename = $haenv->nodename();

    my $ss = $self->{service_status};

    my $count = 0;
    foreach my $sid (keys %$ss) {
	my $sd = $ss->{$sid};
	next if !$sd->{node};
	next if $sd->{node} ne $nodename;
	my $req_state = $sd->{state};
	next if !defined($req_state);
	next if $req_state eq 'stopped';
	# NOTE: 'ignored' ones are already dropped by the manager from service_status
	next if $req_state eq 'freeze';
	# erroneous services are not managed by HA, don't count them as active
	next if $req_state eq 'error';

	$count++;
    }

    return $count;
}

my $wrote_lrm_status_at_startup = 0;

sub do_one_iteration {
    my ($self) = @_;

    my $haenv = $self->{haenv};

    $haenv->loop_start_hook();

    $self->{cluster_state_update} = $haenv->cluster_state_update();

    my $res = $self->work();

    $haenv->loop_end_hook();

    return $res;
}

# NOTE: this is disabling the self-fence mechanism, so it must NOT be called with active services
# It's normally *only* OK on graceful shutdown (with no services, or all services frozen)
my sub give_up_watchdog_protection {
    my ($self) = @_;

    if ($self->{ha_agent_wd}) {
	$self->{haenv}->watchdog_close($self->{ha_agent_wd});
	delete $self->{ha_agent_wd}; # only delete after close!
    }
}

sub work {
    my ($self) = @_;

    my $haenv = $self->{haenv};

    if (!$wrote_lrm_status_at_startup) {
	if ($self->update_lrm_status()) {
	    $wrote_lrm_status_at_startup = 1;
	} else {
	    # do nothing
	    $haenv->sleep(5);
	    return $self->{shutdown_request} ? 0 : 1;
	}
    }

    my $status = $self->get_local_status();
    my $state = $status->{state};

    $self->update_service_status();

    my $fence_request = $self->is_fence_requested();

    # do state changes first

    my $ctime = $haenv->get_time();

    if ($state eq 'wait_for_agent_lock') {

	my $service_count = $self->active_service_count();

	if (!$fence_request && $service_count && $haenv->quorate()) {
	    if ($self->get_protected_ha_agent_lock()) {
		$self->set_local_status({ state => 'active' });
	    }
	}

    } elsif ($state eq 'lost_agent_lock') {

	if (!$fence_request && $haenv->quorate()) {
	    if ($self->get_protected_ha_agent_lock()) {
		$self->set_local_status({ state => 'active' });
	    }
	}

    } elsif ($state eq 'active') {

	if ($fence_request) {
	    $haenv->log('err', "node need to be fenced - releasing agent_lock\n");
	    $self->set_local_status({ state => 'lost_agent_lock'});
	} elsif (!$self->get_protected_ha_agent_lock()) {
	    $self->set_local_status({ state => 'lost_agent_lock'});
	} elsif ($self->{mode} eq 'maintenance') {
	    $self->set_local_status({ state => 'maintenance'});
	} else {
	    if (!$self->has_configured_service_on_local_node() && !$self->run_workers()) {
		# no active service configured for this node and all (old) workers are done
		$self->{active_idle_rounds}++;
		if ($self->{active_idle_rounds} > $max_active_idle_rounds) {
		    $haenv->log('info', "node had no service configured for $max_active_idle_rounds rounds, going idle.\n");
		    # safety: no active service & no running worker for quite some time -> OK
		    $haenv->release_ha_agent_lock();
		    give_up_watchdog_protection($self);
		    $self->set_local_status({ state => 'wait_for_agent_lock'});
		    $self->{active_idle_rounds} = 0;
		}
	    } elsif ($self->{active_idle_rounds}) {
		$self->{active_idle_rounds} = 0;
	    }
	}
    } elsif ($state eq 'maintenance') {

	if ($fence_request) {
	    $haenv->log('err', "node need to be fenced during maintenance mode - releasing agent_lock\n");
	    $self->set_local_status({ state => 'lost_agent_lock'});
	} elsif (!$self->get_protected_ha_agent_lock()) {
	    $self->set_local_status({ state => 'lost_agent_lock'});
	}
    }

    $status = $self->get_local_status();
    $state = $status->{state};

    # do work

    if ($state eq 'wait_for_agent_lock') {

	return 0 if $self->{shutdown_request};

	$self->update_lrm_status();

	$haenv->sleep(5);

    } elsif ($state eq 'active') {

	my $startime = $haenv->get_time();

	my $max_time = 10;

	my $shutdown = 0;

	# do work (max_time seconds)
	eval {
	    # fixme: set alert timer

	    # if we could not get the current service status there's no point
	    # in doing anything, try again next round.
	    return if !$self->update_service_status();

	    if ($self->{shutdown_request}) {

		if ($self->{mode} eq 'restart') {
		    # catch exited workers to update service state
		    my $workers = $self->run_workers();
		    my $service_count = $self->active_service_count();

		    if ($service_count == 0 && $workers == 0) {
			# safety: no active services or workers -> OK
			give_up_watchdog_protection($self);
			$shutdown = 1;

			# restart with no or freezed services, release the lock
			$haenv->release_ha_agent_lock();
		    }
		} else {

		    if ($self->run_workers() == 0) {
			if ($self->{shutdown_errors} == 0) {
			    # safety: no active services and LRM shutdown -> OK
			    give_up_watchdog_protection($self);

			    # shutdown with all services stopped thus release the lock
			    $haenv->release_ha_agent_lock();
			}

			$shutdown = 1;
		    }
		}
	    } else {
		if (!$self->{cluster_state_update}) {
		    # update failed but we could still renew our lock (cfs restart?),
		    # safely skip manage and expect to update just fine next round
		    $haenv->log('notice', "temporary inconsistent cluster state " .
		                "(cfs restart?), skip round");
		    return;
		}

		$self->manage_resources();

	    }
	};
	if (my $err = $@) {
	    $haenv->log('err', "got unexpected error - $err");
	}

	$self->update_lrm_status();

	return 0 if $shutdown;

	$haenv->sleep_until($startime + $max_time);

    } elsif ($state eq 'lost_agent_lock') {

	# NOTE: watchdog is active an will trigger soon!
	# so we hope to get the lock back soon!
	if ($self->{shutdown_request}) {

	    my $service_count = $self->active_service_count();

	    if ($service_count > 0) {
		$haenv->log('err', "get shutdown request in state 'lost_agent_lock' - " .
			    "detected $service_count running services");

		if ($self->{mode} eq 'restart') {
		    my $state_mt = $self->{status}->{state_change_time};

		    # watchdog should have already triggered, so either it's set
		    # set to noboot or it failed. As we are in restart mode, and
		    # have infinity stoptimeout -> exit now - we don't touch  services
		    # or change state, so this is save, relatively speaking
		    if (($haenv->get_time() - $state_mt) > 90) {
			$haenv->log('err', "lost agent lock and restart request for over 90 seconds - giving up!");
			return 0;
		    }
		}
	    } else {
		# safety: all services are stopped, so we can close the watchdog
		give_up_watchdog_protection($self);

		return 0;
	    }
	}

	$haenv->sleep(5);

    } elsif ($state eq 'maintenance') {

	my $startime = $haenv->get_time();
	return if !$self->update_service_status();

	# wait until all active services moved away
	my $service_count = $self->active_service_count();

	my $exit_lrm = 0;

	if ($self->{shutdown_request}) {
	    if ($service_count == 0 && $self->run_workers() == 0) {
		# safety: going into maintenance and all active services got moved -> OK
		give_up_watchdog_protection($self);

		$exit_lrm = 1;

		# restart with no or freezed services, release the lock
		$haenv->release_ha_agent_lock();
	    }
	}

	$self->manage_resources() if !$exit_lrm;

	$self->update_lrm_status();

	return 0 if $exit_lrm;

	$haenv->sleep_until($startime + 5);

    } else {

	die "got unexpected status '$state'\n";

    }

    return 1;
}

sub run_workers {
    my ($self) = @_;

    my $haenv = $self->{haenv};

    my $starttime = $haenv->get_time();

    # number of workers to start, if 0 we exec the command directly witouth forking
    my $max_workers = $haenv->get_max_workers();
    my $sc = $haenv->read_service_config();

    my $worker = $self->{workers};
    # we only got limited time but want to ensure that every queued worker is scheduled
    # eventually, so sort by the count a worker was seen here in this loop
    my $fair_sorter = sub {
	$worker->{$b}->{start_tries} <=> $worker->{$a}->{start_tries} || $a cmp $b
    };

    while (($haenv->get_time() - $starttime) <= 8) {
	my $count = $self->check_active_workers();

	for my $sid (sort $fair_sorter grep { !$worker->{$_}->{pid} } keys %$worker) {
	    my $w = $worker->{$sid};
	    # higher try-count means higher priority especially compared to newly queued jobs, so
	    # count every try to avoid starvation
	    $w->{start_tries}++;
	    next if $count >= $max_workers && $max_workers > 0;

	    # only fork if we may, else call exec_resource_agent directly (e.g. for tests)
	    if ($max_workers > 0) {
		my $pid = fork();
		if (!defined($pid)) {
		    $haenv->log('err', "forking worker failed - $!");
		    $count = 0; last; # abort, try later
		} elsif ($pid == 0) {
		    $haenv->after_fork(); # cleanup

		    # do work
		    my $res = -1;
		    eval {
			$res = $self->exec_resource_agent($sid, $sc->{$sid}, $w->{state}, $w->{params});
		    };
		    if (my $err = $@) {
			$haenv->log('err', $err);
			POSIX::_exit(-1);
		    }
		    POSIX::_exit($res);
		} else {
		    $count++;
		    $w->{pid} = $pid;
		}
	    } else {
		my $res = -1;
		eval {
		    $res = $self->exec_resource_agent($sid, $sc->{$sid}, $w->{state}, $w->{params});
		    $res = $res << 8 if $res > 0;
		};
		if (my $err = $@) {
		    $haenv->log('err', $err);
		}
		if (defined($w->{uid})) {
		    $self->resource_command_finished($sid, $w->{uid}, $res);
		} else {
		    $self->stop_command_finished($sid, $res);
		}
	    }
	}

	last if !$count;

	$haenv->sleep(1);
    }

    return scalar(keys %{$self->{workers}});
}

sub manage_resources {
    my ($self) = @_;

    my $haenv = $self->{haenv};

    my $nodename = $haenv->nodename();

    my $ss = $self->{service_status};

    foreach my $sid (keys %{$self->{restart_tries}}) {
	delete $self->{restart_tries}->{$sid} if !$ss->{$sid};
    }

    foreach my $sid (keys %$ss) {
	my $sd = $ss->{$sid};
	next if !$sd->{node} || !$sd->{uid};
	next if $sd->{node} ne $nodename;
	my $request_state = $sd->{state};
	next if !defined($request_state);
	# can only happen for restricted groups where the failed node itself needs to be the
	# reocvery target. Always let the master first do so, it will then marked as 'stopped' and
	# we can just continue normally. But we must NOT do anything with it while still in recovery
	next if $request_state eq 'recovery';
	next if $request_state eq 'freeze';

	$self->queue_resource_command($sid, $sd->{uid}, $request_state, {
	    'target' => $sd->{target},
	    'timeout' => $sd->{timeout},
	});
    }

    return $self->run_workers();
}

sub queue_resource_command {
    my ($self, $sid, $uid, $state, $params) = @_;

    # do not queue the exact same command twice as this may lead to an inconsistent HA state when
    # the first command fails but the CRM does not process its failure right away and the LRM starts
    # a second try, without the CRM knowing of it (race condition) The 'stopped' command is an
    # exception as we do not process its result in the CRM and we want to execute it always (even
    # with no active CRM)
    return if $state ne 'stopped' && $uid && defined($self->{results}->{$uid});

    if (my $w = $self->{workers}->{$sid}) {
	return if $w->{pid}; # already started
	# else, delete and overwrite queue entry with new command
	delete $self->{workers}->{$sid};
    }

    $self->{workers}->{$sid} = {
	sid => $sid,
	uid => $uid,
	state => $state,
	start_tries => 0,
    };

    $self->{workers}->{$sid}->{params} = $params if $params;
}

sub check_active_workers {
    my ($self) = @_;

    # finish/count workers
    my $count = 0;
    foreach my $sid (keys %{$self->{workers}}) {
	my $w = $self->{workers}->{$sid};
	my $pid = $w->{pid} || next;

	my $waitpid = waitpid($pid, WNOHANG); # check status
	if (defined($waitpid) && ($waitpid == $pid)) {
	    if (defined($w->{uid})) {
		$self->resource_command_finished($sid, $w->{uid}, $?);
	    } else {
		$self->stop_command_finished($sid, $?);
	    }
	} else {
	    $count++; # still active
	}
    }

    return $count;
}

sub stop_command_finished {
    my ($self, $sid, $status) = @_;

    my $haenv = $self->{haenv};

    my $w = delete $self->{workers}->{$sid};
    return if !$w; # should not happen

    my $exit_code = -1;

    if ($status == -1) {
	$haenv->log('err', "resource agent $sid finished - failed to execute");
    }  elsif (my $sig = ($status & 127)) {
	$haenv->log('err', "resource agent $sid finished - got signal $sig");
    } else {
	$exit_code = ($status >> 8);
    }

    if ($exit_code != 0) {
	$self->{shutdown_errors}++;
    }
}

sub resource_command_finished {
    my ($self, $sid, $uid, $status) = @_;

    my $haenv = $self->{haenv};

    my $w = delete $self->{workers}->{$sid};
    return if !$w; # should not happen

    my $exit_code = -1;

    if ($status == -1) {
	$haenv->log('err', "resource agent $sid finished - failed to execute");
    }  elsif (my $sig = ($status & 127)) {
	$haenv->log('err', "resource agent $sid finished - got signal $sig");
    } else {
	$exit_code = ($status >> 8);
    }

    $exit_code = $self->handle_service_exitcode($sid, $w->{state}, $exit_code);

    return if $exit_code == ETRY_AGAIN; # tell nobody, simply retry

    $self->{results}->{$uid} = {
	sid => $w->{sid},
	state => $w->{state},
	exit_code => $exit_code,
    };

    my $ss = $self->{service_status};

    # compute hash of valid/existing uids
    my $valid_uids = {};
    foreach my $sid (keys %$ss) {
	my $sd = $ss->{$sid};
	next if !$sd->{uid};
	$valid_uids->{$sd->{uid}} = 1;
    }

    my $results = {};
    foreach my $id (keys %{$self->{results}}) {
	next if !$valid_uids->{$id};
	$results->{$id} = $self->{results}->{$id};
    }
    $self->{results} = $results;
}

# processes the exit code from a finished resource agent, so that the CRM knows
# if the LRM wants to retry an action based on the current recovery policies for
# the failed service, or the CRM itself must try to recover from the failure.
sub handle_service_exitcode {
    my ($self, $sid, $cmd, $exit_code) = @_;

    my $haenv = $self->{haenv};
    my $tries = $self->{restart_tries};

    my $sc = $haenv->read_service_config();

    my $max_restart = 0;

    if (my $cd = $sc->{$sid}) {
	$max_restart = $cd->{max_restart};
    }

    if ($cmd eq 'started') {

	if ($exit_code == SUCCESS) {

	    $tries->{$sid} = 0;

	    return $exit_code;

	} elsif ($exit_code == ERROR) {

	    $tries->{$sid} = 0 if !defined($tries->{$sid});

	    if ($tries->{$sid} >= $max_restart) {
		$haenv->log('err', "unable to start service $sid on local node".
			   " after $tries->{$sid} retries");
		$tries->{$sid} = 0;
		return ERROR;
	    }

	    $tries->{$sid}++;

	    $haenv->log('warning', "restart policy: retry number $tries->{$sid}" .
			" for service '$sid'");
	    # tell CRM that we retry the start
	    return ETRY_AGAIN;
	}
    }

    return $exit_code;

}

sub exec_resource_agent {
    my ($self, $sid, $service_config, $cmd, $params) = @_;

    # setup execution environment

    $ENV{'PATH'} = '/sbin:/bin:/usr/sbin:/usr/bin';

    my $haenv = $self->{haenv};

    my $nodename = $haenv->nodename();

    my (undef, $service_type, $service_name) = $haenv->parse_sid($sid);

    my $plugin = PVE::HA::Resources->lookup($service_type);
    if (!$plugin) {
	$haenv->log('err', "service type '$service_type' not implemented");
	return EUNKNOWN_SERVICE_TYPE;
    }

    if (!$service_config) {
	$haenv->log('err', "missing resource configuration for '$sid'");
	return EUNKNOWN_SERVICE;
    }

    # process error state early
    if ($cmd eq 'error') {
	$haenv->log('err', "service $sid is in an error state and needs manual " .
		    "intervention. Look up 'ERROR RECOVERY' in the documentation.");

	return SUCCESS; # error always succeeds
    }

    if ($service_config->{node} ne $nodename) {
	$haenv->log('err', "service '$sid' not on this node");
	return EWRONG_NODE;
    }

    my $id = $service_name;

    my $running = $plugin->check_running($haenv, $id);

    if ($cmd eq 'started') {

	return SUCCESS if $running;

	$haenv->log("info", "starting service $sid");

	$plugin->start($haenv, $id);

	$running = $plugin->check_running($haenv, $id);

	if ($running) {
	    $haenv->log("info", "service status $sid started");
	    return SUCCESS;
	} else {
	    $haenv->log("warning", "unable to start service $sid");
	    return ERROR;
	}

    } elsif ($cmd eq 'request_stop' || $cmd eq 'stopped') {

	return SUCCESS if !$running;

	if (defined($params->{timeout})) {
	    $haenv->log("info", "stopping service $sid (timeout=$params->{timeout})");
	} else {
	    $haenv->log("info", "stopping service $sid");
	}

	$plugin->shutdown($haenv, $id, $params->{timeout});

	$running = $plugin->check_running($haenv, $id);

	if (!$running) {
	    $haenv->log("info", "service status $sid stopped");
	    return SUCCESS;
	} else {
	    $haenv->log("info", "unable to stop stop service $sid (still running)");
	    return ERROR;
	}

    } elsif ($cmd eq 'migrate' || $cmd eq 'relocate') {

	my $target = $params->{target};
	if (!defined($target)) {
	    die "$cmd '$sid' failed - missing target\n" if !defined($target);
	    return EINVALID_PARAMETER;
	}

	if ($service_config->{node} eq $target) {
	    # already there
	    return SUCCESS;
	}

	my $online = ($cmd eq 'migrate') ? 1 : 0;

	my $res = $plugin->migrate($haenv, $id, $target, $online);

	# something went wrong if service is still on this node
	if (!$res) {
	    $haenv->log("err", "service $sid not moved (migration error)");
	    return ERROR;
	}

	return SUCCESS;

    }

    $haenv->log("err", "implement me (cmd '$cmd')");
    return EUNKNOWN_COMMAND;
}


1;
