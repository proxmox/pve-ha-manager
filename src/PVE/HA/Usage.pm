package PVE::HA::Usage;

use strict;
use warnings;

sub new {
    my ($class, $haenv) = @_;

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

# Logs a warning to $haenv upon failure, but does not die.
sub add_service_usage_to_node {
    my ($self, $nodename, $sid, $service_node, $migration_target) = @_;

    die "implement in subclass";
}

# Returns a hash with $nodename => $score pairs. A lower $score is better.
sub score_nodes_to_start_service {
    my ($self, $sid, $service_node) = @_;

    die "implement in subclass";
}

# Returns the current and target node as a two-element array, that a service
# puts load on according to the $online_nodes and the service's $state, $node
# and $target.
sub get_used_service_nodes {
    my ($online_nodes, $state, $node, $target) = @_;

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
