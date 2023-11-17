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

    my $scheduler = eval { PVE::RS::ResourceScheduling::Static->new(); };
    die "unable to initialize static scheduling - $@" if $@;

    return bless {
	'node-stats' => $node_stats,
	'service-stats' => {},
	haenv => $haenv,
	scheduler => $scheduler,
	'service-counts' => {}, # Service count on each node. Fallback if scoring calculation fails.
    }, $class;
}

sub add_node {
    my ($self, $nodename) = @_;

    $self->{'service-counts'}->{$nodename} = 0;

    my $stats = $self->{'node-stats'}->{$nodename}
	or die "did not get static node usage information for '$nodename'\n";
    die "static node usage information for '$nodename' missing cpu count\n" if !$stats->{cpus};
    die "static node usage information for '$nodename' missing memory\n" if !$stats->{memory};

    eval { $self->{scheduler}->add_node($nodename, int($stats->{cpus}), int($stats->{memory})); };
    die "initializing static node usage for '$nodename' failed - $@" if $@;
}

sub remove_node {
    my ($self, $nodename) = @_;

    delete $self->{'service-counts'}->{$nodename};

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
    my ($self, $sid, $service_node, $migration_target) = @_;

    return $self->{'service-stats'}->{$sid} if $self->{'service-stats'}->{$sid};

    my (undef, $type, $id) = $self->{haenv}->parse_sid($sid);
    my $plugin = PVE::HA::Resources->lookup($type);

    my $stats = eval { $plugin->get_static_stats($self->{haenv}, $id, $service_node) };
    if (my $err = $@) {
	# config might've already moved during a migration
	$stats = eval { $plugin->get_static_stats($self->{haenv}, $id, $migration_target); } if $migration_target;
	die "did not get static service usage information for '$sid' - $err\n" if !$stats;
    }

    my $service_stats = {
	maxcpu => $stats->{maxcpu} + 0.0, # containers allow non-integer cpulimit
	maxmem => int($stats->{maxmem}),
    };

    $self->{'service-stats'}->{$sid} = $service_stats;

    return $service_stats;
}

sub add_service_usage_to_node {
    my ($self, $nodename, $sid, $service_node, $migration_target) = @_;

    $self->{'service-counts'}->{$nodename}++;

    eval {
	my $service_usage = get_service_usage($self, $sid, $service_node, $migration_target);
	$self->{scheduler}->add_service_usage_to_node($nodename, $service_usage);
    };
    $self->{haenv}->log('warning', "unable to add service '$sid' usage to node '$nodename' - $@")
	if $@;
}

sub score_nodes_to_start_service {
    my ($self, $sid, $service_node) = @_;

    my $score_list = eval {
	my $service_usage = get_service_usage($self, $sid, $service_node);
	$self->{scheduler}->score_nodes_to_start_service($service_usage);
    };
    if (my $err = $@) {
	$self->{haenv}->log(
	    'err',
	    "unable to score nodes according to static usage for service '$sid' - $err",
	);
	return $self->{'service-counts'};
    }

    # Take minus the value, so that a lower score is better, which our caller(s) expect(s).
    return { map { $_->[0] => -$_->[1] } $score_list->@* };
}

1;
