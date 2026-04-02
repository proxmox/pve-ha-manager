package PVE::HA::Usage;

use strict;
use warnings;

sub new {
    my ($class, $haenv, $service_stats) = @_;

    die "implement in subclass";
}

sub add_node {
    my ($self, $nodename) = @_;

    die "implement in subclass";
}

sub remove_node {
    my ($self, $nodename) = @_;

    die "implement in subclass";
}

sub list_nodes {
    my ($self) = @_;

    die "implement in subclass";
}

sub contains_node {
    my ($self, $nodename) = @_;

    die "implement in subclass";
}

sub add_service {
    my ($self, $sid, $current_node, $target_node, $running) = @_;

    die "implement in subclass";
}

# Adds service $sid's usage to the online nodes according to their service data $sd.
sub add_service_usage {
    my ($self, $sid, $sd) = @_;

    my $online_nodes = { map { $_ => 1 } $self->list_nodes() };
    my ($current_node, $target_node) = get_used_service_nodes($online_nodes, $sd);

    # some usage implementations need to discern whether a service is truly running;
    # a service does only have the 'running' flag in 'started' state
    my $running = ($sd->{state} eq 'started' && $sd->{running})
        || ($sd->{state} ne 'started' && defined($current_node));

    $self->add_service($sid, $current_node, $target_node, $running);
}

sub remove_service_usage {
    my ($self, $sid) = @_;

    die "implement in subclass";
}

sub calculate_node_imbalance {
    my ($self) = @_;

    die "implement in subclass";
}

sub score_best_balancing_migrations {
    my ($self, $migration_candidates, $limit) = @_;

    die "implement in subclass";
}

sub select_best_balancing_migration {
    my ($self, $migration_candidates) = @_;

    my $migrations = $self->score_best_balancing_migrations($migration_candidates, 1);

    return $migrations->[0];
}

sub score_best_balancing_migrations_topsis {
    my ($self, $migration_candidates, $limit) = @_;

    die "implement in subclass";
}

sub select_best_balancing_migration_topsis {
    my ($self, $migration_candidates) = @_;

    my $migrations = $self->score_best_balancing_migrations_topsis($migration_candidates, 1);

    return $migrations->[0];
}

# Returns a hash with $nodename => $score pairs. A lower $score is better.
sub score_nodes_to_start_service {
    my ($self, $sid) = @_;

    die "implement in subclass";
}

# Returns a two-element array of the nodes a service puts load on
# (current and target), given $online_nodes and service data $sd.
sub get_used_service_nodes {
    my ($online_nodes, $sd) = @_;

    my ($state, $node, $target) = $sd->@{qw(state node target)};

    return (undef, undef) if $state eq 'stopped' || $state eq 'request_start';

    my ($current_node, $target_node);

    if (
        $state eq 'started'
        || $state eq 'request_stop'
        || $state eq 'fence'
        || $state eq 'freeze'
        || $state eq 'error'
        || $state eq 'recovery'
        || $state eq 'migrate'
        || $state eq 'relocate'
    ) {
        $current_node = $node if $online_nodes->{$node};
    }

    if ($state eq 'migrate' || $state eq 'relocate' || $state eq 'request_start_balance') {
        $target_node = $target if defined($target) && $online_nodes->{$target};
    }

    return ($current_node, $target_node);
}

1;
