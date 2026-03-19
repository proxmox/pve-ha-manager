package PVE::HA::Usage::Static;

use strict;
use warnings;

use PVE::HA::Resources;
use PVE::RS::ResourceScheduling::Static;

use base qw(PVE::HA::Usage);

sub new {
    my ($class, $haenv) = @_;

    my $node_stats = eval { $haenv->get_static_node_stats() };
    die "did not get static node usage information - $@" if $@;

    my $service_stats = eval { $haenv->get_static_service_stats() };
    die "did not get static service usage information - $@" if $@;

    my $scheduler = eval { PVE::RS::ResourceScheduling::Static->new(); };
    die "unable to initialize static scheduling - $@" if $@;

    return bless {
        'node-stats' => $node_stats,
        'service-stats' => $service_stats,
        haenv => $haenv,
        scheduler => $scheduler,
        'node-services' => {}, # Services on each node. Fallback if scoring calculation fails.
    }, $class;
}

sub add_node {
    my ($self, $nodename) = @_;

    $self->{'node-services'}->{$nodename} = {};

    my $stats = $self->{'node-stats'}->{$nodename}
        or die "did not get static node usage information for '$nodename'\n";
    die "static node usage information for '$nodename' missing cpu count\n" if !defined($stats->{maxcpu});
    die "static node usage information for '$nodename' missing memory\n" if !defined($stats->{maxmem});

    eval { $self->{scheduler}->add_node($nodename, int($stats->{maxcpu}), int($stats->{maxmem})); };
    die "initializing static node usage for '$nodename' failed - $@" if $@;
}

sub remove_node {
    my ($self, $nodename) = @_;

    delete $self->{'node-services'}->{$nodename};

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
        or die "did not get static service usage information for '$sid'\n";

    return $service_stats;
}

sub add_service_usage_to_node {
    my ($self, $nodename, $sid) = @_;

    $self->{'node-services'}->{$nodename}->{$sid} = 1;

    eval {
        my $service_usage = get_service_usage($self, $sid);
        $self->{scheduler}->add_service_usage_to_node($nodename, $sid, $service_usage);
    };
    $self->{haenv}->log('warning', "unable to add service '$sid' usage to node '$nodename' - $@")
        if $@;
}

sub remove_service_usage {
    my ($self, $sid) = @_;

    delete($self->{'node-services'}->{$_}->{$sid}) for $self->list_nodes();

    eval { $self->{scheduler}->remove_service_usage($sid) };
    $self->{haenv}->log('warning', "unable to remove service '$sid' usage - $@") if $@;
}

sub score_nodes_to_start_service {
    my ($self, $sid) = @_;

    my $score_list = eval {
        my $service_usage = get_service_usage($self, $sid);
        $self->{scheduler}->score_nodes_to_start_service($service_usage);
    };
    if (my $err = $@) {
        $self->{haenv}->log(
            'err',
            "unable to score nodes according to static usage for service '$sid' - $err",
        );
        my $node_services = $self->{'node-services'};
        return { map { $_ => scalar(keys $node_services->{$_}->%*) } keys $node_services->%* };
    }

    # Take minus the value, so that a lower score is better, which our caller(s) expect(s).
    return { map { $_->[0] => -$_->[1] } $score_list->@* };
}

1;
