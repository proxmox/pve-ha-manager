#!/usr/bin/perl

use v5.36;

use lib qw(..);

use Test::More;

use PVE::HA::Manager;

my $get_active_stationary_resource_bundle_tests = [
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
];

my $tests = [
    @$get_active_stationary_resource_bundle_tests,
];

plan(tests => scalar($tests->@*));

for my $case ($get_active_stationary_resource_bundle_tests->@*) {
    my ($ss, $resource_affinity) = $case->@{qw(services resource_affinity)};

    my $result = PVE::HA::Manager::get_active_stationary_resource_bundles($ss, $resource_affinity);

    is_deeply($result, $case->{resource_bundles}, $case->{description});
}

done_testing();
