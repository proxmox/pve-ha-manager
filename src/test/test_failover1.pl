#!/usr/bin/perl

use strict;
use warnings;

use lib '..';
use PVE::HA::Manager;
use PVE::HA::Usage::Basic;

use PVE::HA::Rules;
use PVE::HA::Rules::NodeAffinity;

PVE::HA::Rules::NodeAffinity->register();

PVE::HA::Rules->init(property_isolation => 1);

my $sid = 'vm:111';
my $rules = PVE::HA::Rules->parse_config("rules.tmp", <<EOD);
node-affinity: prefer_node1
	resources $sid
	nodes node1
EOD

# Relies on the fact that the basic plugin doesn't use the haenv.
my $online_node_usage = PVE::HA::Usage::Basic->new();
$online_node_usage->add_node("node1");
$online_node_usage->add_node("node2");
$online_node_usage->add_node("node3");

my $service_conf = {
    node => 'node1',
    failback => 1,
};

my $ss = {
    "$sid" => {
        node => $service_conf->{node},
        failed_nodes => undef,
        maintenance_node => undef,
    },
};

sub test {
    my ($expected_node, $try_next) = @_;

    my $select_node_preference = $try_next ? 'try-next' : 'none';

    my $node = PVE::HA::Manager::select_service_node(
        $rules, $online_node_usage, "$sid", $service_conf, $ss, $select_node_preference,
    );

    my (undef, undef, $line) = caller();
    die "unexpected result: $node != ${expected_node} at line $line\n"
        if $node ne $expected_node;

    $ss->{$sid}->{node} = $node;
}

test('node1');
test('node1', 1);

$online_node_usage->remove_node("node1"); # poweroff

test('node2');
test('node3', 1);
test('node2', 1);

$online_node_usage->remove_node("node2"); # poweroff

test('node3');
test('node3', 1);

$online_node_usage->add_node("node1"); # poweron

test('node1');

$online_node_usage->add_node("node2"); # poweron

test('node1');
test('node1', 1);
