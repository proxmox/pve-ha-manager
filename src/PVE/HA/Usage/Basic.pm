package PVE::HA::Usage::Basic;

use strict;
use warnings;

use base qw(PVE::HA::Usage);

sub new {
    my ($class, $haenv) = @_;

    return bless {
        nodes => {},
        haenv => $haenv,
        'service-nodes' => {},
    }, $class;
}

sub add_node {
    my ($self, $nodename) = @_;

    $self->{nodes}->{$nodename} = 0;
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

sub get_service_nodes {
    my ($self, $sid) = @_;

    return $self->{'service-nodes'}->{$sid};
}

sub set_service_node {
    my ($self, $sid, $nodename) = @_;

    $self->{'service-nodes'}->{$sid} = [$nodename];
}

sub add_service_node {
    my ($self, $sid, $nodename) = @_;

    push @{ $self->{'service-nodes'}->{$sid} }, $nodename;
}

sub add_service_usage_to_node {
    my ($self, $nodename, $sid, $service_node, $migration_target) = @_;

    if ($self->contains_node($nodename)) {
        $self->{nodes}->{$nodename}++;
    } else {
        $self->{haenv}->log(
            'warning',
            "unable to add service '$sid' usage to node '$nodename' - node not in usage hash",
        );
    }
}

sub score_nodes_to_start_service {
    my ($self, $sid, $service_node) = @_;

    return $self->{nodes};
}

1;
