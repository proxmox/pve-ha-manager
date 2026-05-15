#!/usr/bin/perl

use v5.36;

use lib qw(..);

use Test::More;

use PVE::HA::Manager;

my $get_active_stationary_movable_resource_bundle_tests = [
    {
        description => "trivial resource bundles",
        services => {
            'vm:101' => {
                state => 'started',
                node => 'node1',
            },
            'vm:102' => {
                state => 'started',
                node => 'node1',
            },
        },
        service_config => {
            'vm:101' => { 'auto-rebalance' => 1 },
            'vm:102' => { 'auto-rebalance' => 1 },
        },
        resource_affinity => {
            positive => {},
            negative => {},
        },
        resource_bundles => {
            'vm:101' => [
                'vm:101',
            ],
            'vm:102' => [
                'vm:102',
            ],
        },
    },
    {
        description => "simple resource bundle",
        services => {
            'vm:101' => {
                state => 'started',
                node => 'node1',
            },
            'vm:102' => {
                state => 'started',
                node => 'node1',
            },
        },
        service_config => {
            'vm:101' => { 'auto-rebalance' => 1 },
            'vm:102' => { 'auto-rebalance' => 1 },
        },
        resource_affinity => {
            positive => {
                'vm:101' => {
                    'vm:102' => 1,
                },
                'vm:102' => {
                    'vm:101' => 1,
                },
            },
            negative => {},
        },
        resource_bundles => {
            'vm:101' => [
                'vm:101', 'vm:102',
            ],
        },
    },
    {
        description => "resource bundle with first resource stopped",
        services => {
            'vm:101' => {
                state => 'stopped',
                node => 'node1',
            },
            'vm:102' => {
                state => 'started',
                node => 'node1',
            },
            'vm:103' => {
                state => 'started',
                node => 'node1',
            },
        },
        service_config => {
            'vm:101' => { 'auto-rebalance' => 1 },
            'vm:102' => { 'auto-rebalance' => 1 },
            'vm:103' => { 'auto-rebalance' => 1 },
        },
        resource_affinity => {
            positive => {
                'vm:101' => {
                    'vm:102' => 1,
                    'vm:103' => 1,
                },
                'vm:102' => {
                    'vm:101' => 1,
                    'vm:103' => 1,
                },
                'vm:103' => {
                    'vm:101' => 1,
                    'vm:102' => 1,
                },
            },
            negative => {},
        },
        resource_bundles => {
            'vm:102' => [
                'vm:102', 'vm:103',
            ],
        },
    },
    {
        description => "resource bundle with some stopped resources",
        services => {
            'vm:101' => {
                state => 'started',
                node => 'node1',
            },
            'vm:102' => {
                state => 'stopped',
                node => 'node1',
            },
            'vm:103' => {
                state => 'started',
                node => 'node1',
            },
        },
        service_config => {
            'vm:101' => { 'auto-rebalance' => 1 },
            'vm:102' => { 'auto-rebalance' => 1 },
            'vm:103' => { 'auto-rebalance' => 1 },
        },
        resource_affinity => {
            positive => {
                'vm:101' => {
                    'vm:102' => 1,
                    'vm:103' => 1,
                },
                'vm:102' => {
                    'vm:101' => 1,
                    'vm:103' => 1,
                },
                'vm:103' => {
                    'vm:101' => 1,
                    'vm:102' => 1,
                },
            },
            negative => {},
        },
        resource_bundles => {
            'vm:101' => [
                'vm:101', 'vm:103',
            ],
        },
    },
    {
        description => "resource bundle with moving resources",
        services => {
            'vm:101' => {
                state => 'started',
                node => 'node1',
            },
            'vm:102' => {
                state => 'migrate',
                node => 'node2',
                target => 'node1',
            },
            'vm:103' => {
                state => 'relocate',
                node => 'node3',
                target => 'node1',
            },
        },
        service_config => {
            'vm:101' => { 'auto-rebalance' => 1 },
            'vm:102' => { 'auto-rebalance' => 1 },
            'vm:103' => { 'auto-rebalance' => 1 },
        },
        resource_affinity => {
            positive => {
                'vm:101' => {
                    'vm:102' => 1,
                    'vm:103' => 1,
                },
                'vm:102' => {
                    'vm:101' => 1,
                    'vm:103' => 1,
                },
                'vm:103' => {
                    'vm:101' => 1,
                    'vm:102' => 1,
                },
            },
            negative => {},
        },
        resource_bundles => {},
    },
    # might happen if the resource bundle is generated even before the HA Manager
    # puts the HA resources in migrate/relocate to make them adhere to the HA rules
    {
        description => "resource bundle with resources on different nodes",
        services => {
            'vm:101' => {
                state => 'started',
                node => 'node1',
            },
            'vm:102' => {
                state => 'started',
                node => 'node2',
            },
            'vm:103' => {
                state => 'started',
                node => 'node3',
            },
        },
        service_config => {
            'vm:101' => { 'auto-rebalance' => 1 },
            'vm:102' => { 'auto-rebalance' => 1 },
            'vm:103' => { 'auto-rebalance' => 1 },
        },
        resource_affinity => {
            positive => {
                'vm:101' => {
                    'vm:102' => 1,
                    'vm:103' => 1,
                },
                'vm:102' => {
                    'vm:101' => 1,
                    'vm:103' => 1,
                },
                'vm:103' => {
                    'vm:101' => 1,
                    'vm:102' => 1,
                },
            },
            negative => {},
        },
        resource_bundles => {},
    },
    {
        description => "singleton resource bundle with disabled auto-rebalance",
        services => {
            'vm:101' => {
                state => 'started',
                node => 'node1',
            },
            'vm:102' => {
                state => 'started',
                node => 'node1',
            },
        },
        service_config => {
            'vm:101' => { 'auto-rebalance' => 0 },
            'vm:102' => { 'auto-rebalance' => 1 },
        },
        resource_affinity => {
            positive => {},
            negative => {},
        },
        resource_bundles => {
            'vm:102' => [
                'vm:102',
            ],
        },
    },
    {
        description => "resource bundle leader with disabled auto-rebalance",
        services => {
            'vm:101' => {
                state => 'started',
                node => 'node1',
            },
            'vm:102' => {
                state => 'started',
                node => 'node1',
            },
            'ct:103' => {
                state => 'started',
                node => 'node2',
            },
        },
        service_config => {
            'vm:101' => { 'auto-rebalance' => 0 },
            'vm:102' => { 'auto-rebalance' => 1 },
            'ct:103' => { 'auto-rebalance' => 1 },
        },
        resource_affinity => {
            positive => {
                'vm:101' => {
                    'vm:102' => 1,
                },
                'vm:102' => {
                    'vm:101' => 1,
                },
            },
            negative => {},
        },
        resource_bundles => {
            'ct:103' => [
                'ct:103',
            ],
        },
    },
    {
        description => "some member of resource bundle with disabled auto-rebalance",
        services => {
            'vm:101' => {
                state => 'started',
                node => 'node1',
            },
            'vm:102' => {
                state => 'started',
                node => 'node1',
            },
            'ct:103' => {
                state => 'started',
                node => 'node2',
            },
        },
        service_config => {
            'vm:101' => { 'auto-rebalance' => 1 },
            'vm:102' => { 'auto-rebalance' => 0 },
            'ct:103' => { 'auto-rebalance' => 1 },
        },
        resource_affinity => {
            positive => {
                'vm:101' => {
                    'vm:102' => 1,
                },
                'vm:102' => {
                    'vm:101' => 1,
                },
            },
            negative => {},
        },
        resource_bundles => {
            'ct:103' => [
                'ct:103',
            ],
        },
    },
];

my $tests = [
    @$get_active_stationary_movable_resource_bundle_tests,
];

plan(tests => scalar($tests->@*));

for my $case ($get_active_stationary_movable_resource_bundle_tests->@*) {
    my ($ss, $sc, $resource_affinity) = $case->@{qw(services service_config resource_affinity)};

    my $result = PVE::HA::Manager::get_active_stationary_movable_resource_bundles(
        $ss, $sc, $resource_affinity,
    );

    is_deeply($result, $case->{resource_bundles}, $case->{description});
}

done_testing();
