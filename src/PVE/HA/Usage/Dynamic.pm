package PVE::HA::Usage::Dynamic;

use strict;
use warnings;

use PVE::HA::Resources;
use PVE::RS::ResourceScheduling::Dynamic;

use base qw(PVE::HA::Usage);

sub new {
    my ($class, $haenv, $service_stats) = @_;

    my $node_stats = eval { $haenv->get_dynamic_node_stats() };
    die "did not get dynamic node usage information - $@" if $@;

    my $scheduler = eval { PVE::RS::ResourceScheduling::Dynamic->new() };
    die "unable to initialize dynamic scheduling - $@" if $@;

    return bless {
        'node-stats' => $node_stats,
        'service-stats' => $service_stats,
        haenv => $haenv,
        scheduler => $scheduler,
    }, $class;
}

sub add_node {
    my ($self, $nodename) = @_;

    my $stats = $self->{'node-stats'}->{$nodename}
        or die "did not get dynamic node usage information for '$nodename'\n";
    die "dynamic node usage information for '$nodename' missing cpu count\n"
        if !defined($stats->{maxcpu});
    die "dynamic node usage information for '$nodename' missing memory\n"
        if !defined($stats->{maxmem});

    eval { $self->{scheduler}->add_node($nodename, $stats); };
    die "initializing dynamic node usage for '$nodename' failed - $@" if $@;
}

sub remove_node {
    my ($self, $nodename) = @_;

    $self->{scheduler}->remove_node($nodename);
}

sub list_nodes {
    my ($self) = @_;

    return $self->{scheduler}->list_nodes()->@*;
}

sub contains_node {
    my ($self, $nodename) = @_;

    return $self->{scheduler}->contains_node($nodename);
}

my sub get_service_usage {
    my ($self, $sid) = @_;

    my $service_stats = $self->{'service-stats'}->{$sid}->{usage}
        or die "did not get dynamic service usage information for '$sid'\n";

    return $service_stats;
}

sub add_service {
    my ($self, $sid, $current_node, $target_node, $running) = @_;

    # do not add service, which does not put any usage on the nodes
    return if !defined($current_node) && !defined($target_node);

    # PVE::RS::ResourceScheduling::Dynamic::add_resource() expects $current_node
    # to be set, so consider $target_node as $current_node for unset $current_node;
    #
    # currently, this happens for the request_start_balance service state and if
    # node maintenance causes services to migrate to other nodes
    if (!defined($current_node)) {
        $current_node = $target_node;
        undef $target_node;
    }

    eval {
        my $service_usage = get_service_usage($self, $sid);

        my $service = {
            stats => $service_usage,
            running => $running,
            'current-node' => $current_node,
            'target-node' => $target_node,
        };

        $self->{scheduler}->add_resource($sid, $service);
    };
    $self->{haenv}->log('warning', "unable to add service '$sid' - $@") if $@;
}

sub remove_service_usage {
    my ($self, $sid) = @_;

    eval { $self->{scheduler}->remove_resource($sid) };
    $self->{haenv}->log('warning', "unable to remove service '$sid' usage - $@") if $@;
}

sub calculate_node_imbalance {
    my ($self) = @_;

    my $node_imbalance = eval { $self->{scheduler}->calculate_node_imbalance() };
    $self->{haenv}->log('warning', "unable to calculate dynamic node imbalance - $@") if $@;

    return $node_imbalance // 0.0;
}

sub score_best_balancing_migrations {
    my ($self, $migration_candidates, $limit) = @_;

    my $migrations = eval {
        $self->{scheduler}
            ->score_best_balancing_migration_candidates($migration_candidates, $limit);
    };
    $self->{haenv}->log('warning', "unable to score best balancing migration - $@") if $@;

    return $migrations;
}

sub score_best_balancing_migrations_topsis {
    my ($self, $migration_candidates, $limit) = @_;

    my $migrations = eval {
        $self->{scheduler}
            ->score_best_balancing_migration_candidates_topsis($migration_candidates, $limit);
    };
    $self->{haenv}->log('warning', "unable to score best balancing migration - $@") if $@;

    return $migrations;
}

sub score_nodes_to_start_service {
    my ($self, $sid) = @_;

    my $score_list = eval {
        my $service_usage = get_service_usage($self, $sid);
        $self->{scheduler}->score_nodes_to_start_resource($service_usage);
    };
    $self->{haenv}
        ->log('err', "unable to score nodes according to dynamic usage for service '$sid' - $@")
        if $@;

    # Take minus the value, so that a lower score is better, which our caller(s) expect(s).
    return { map { $_->[0] => -$_->[1] } $score_list->@* };
}

1;
