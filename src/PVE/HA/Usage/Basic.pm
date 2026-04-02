package PVE::HA::Usage::Basic;

use strict;
use warnings;

use base qw(PVE::HA::Usage);

sub new {
    my ($class, $haenv, $service_stats) = @_;

    return bless {
        nodes => {},
        haenv => $haenv,
    }, $class;
}

sub add_node {
    my ($self, $nodename) = @_;

    $self->{nodes}->{$nodename} = {};
}

sub remove_node {
    my ($self, $nodename) = @_;

    delete $self->{nodes}->{$nodename};
}

sub list_nodes {
    my ($self) = @_;

    return keys $self->{nodes}->%*;
}

sub contains_node {
    my ($self, $nodename) = @_;

    return defined($self->{nodes}->{$nodename});
}

my sub add_service_usage_to_node {
    my ($self, $nodename, $sid) = @_;

    if ($self->contains_node($nodename)) {
        $self->{nodes}->{$nodename}->{$sid} = 1;
    } else {
        $self->{haenv}->log(
            'warning',
            "unable to add service '$sid' usage to node '$nodename' - node not in usage hash",
        );
    }
}

sub add_service {
    my ($self, $sid, $current_node, $target_node, $running) = @_;

    add_service_usage_to_node($self, $current_node, $sid) if defined($current_node);
    add_service_usage_to_node($self, $target_node, $sid) if defined($target_node);
}

sub remove_service_usage {
    my ($self, $sid) = @_;

    for my $node ($self->list_nodes()) {
        delete $self->{nodes}->{$node}->{$sid};
    }
}

sub calculate_node_imbalance {
    my ($self) = @_;

    return 0.0;
}

sub score_best_balancing_migrations {
    my ($self, $migration_candidates, $limit) = @_;

    return [];
}

sub score_best_balancing_migrations_topsis {
    my ($self, $migration_candidates, $limit) = @_;

    return [];
}

sub score_nodes_to_start_service {
    my ($self, $sid) = @_;

    my $nodes = $self->{nodes};

    return { map { $_ => scalar(keys $nodes->{$_}->%*) } keys $nodes->%* };
}

1;
