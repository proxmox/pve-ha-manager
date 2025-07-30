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

sub get_service_nodes {
    my ($self, $sid) = @_;

    die "implement in subclass";
}

sub set_service_node {
    my ($self, $sid, $nodename) = @_;

    die "implement in subclass";
}

sub add_service_node {
    my ($self, $sid, $nodename) = @_;

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

1;
