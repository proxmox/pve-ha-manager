package PVE::HA::Groups;

use strict;
use warnings;

use PVE::JSONSchema qw(get_standard_option);
use PVE::SectionConfig;
use PVE::HA::Tools;

use base qw(PVE::SectionConfig);

my $defaultData = {
    propertyList => {
        type => {
            description => "Group type.",
            optional => 1,
        },
        group => get_standard_option(
            'pve-ha-group-id',
            { completion => \&PVE::HA::Tools::complete_group },
        ),
        nodes => get_standard_option('pve-ha-group-node-list', { optional => 1 }),
        restricted => {
            description =>
                "Resources bound to restricted groups may only run on nodes defined by the group.",
            verbose_description =>
                "Resources bound to restricted groups may only run on nodes defined by the group. The resource will be placed in the stopped state if no group node member is online. Resources on unrestricted groups may run on any cluster node if all group members are offline, but they will migrate back as soon as a group member comes online. One can implement a 'preferred node' behavior using an unrestricted group with only one member.",
            type => 'boolean',
            optional => 1,
            default => 0,
        },
        nofailback => {
            description =>
                "The CRM tries to run services on the node with the highest priority. If a node with higher priority comes online, the CRM migrates the service to that node. Enabling nofailback prevents that behavior.",
            type => 'boolean',
            optional => 1,
            default => 0,
        },
        comment => {
            description => "Description.",
            type => 'string',
            optional => 1,
            maxLength => 4096,
        },
    },
};

sub type {
    return 'group';
}

sub options {
    return {
        nodes => { optional => 0 },
        comment => { optional => 1 },
        nofailback => { optional => 1 },
        restricted => { optional => 1 },
    };
}

sub private {
    return $defaultData;
}

sub decode_value {
    my ($class, $type, $key, $value) = @_;

    if ($key eq 'nodes') {
        my $res = {};

        foreach my $node (PVE::Tools::split_list($value)) {
            if (PVE::HA::Tools::pve_verify_ha_group_node($node)) {
                $res->{$node} = 1;
            }
        }

        return $res;
    }

    return $value;
}

sub encode_value {
    my ($class, $type, $key, $value) = @_;

    if ($key eq 'nodes') {
        return join(',', keys(%$value));
    }

    return $value;
}

sub parse_section_header {
    my ($class, $line) = @_;

    if ($line =~ m/^(\S+):\s*(\S+)\s*$/) {
        my ($type, $group) = (lc($1), $2);
        my $errmsg = undef; # set if you want to skip whole section
        eval { PVE::JSONSchema::pve_verify_configid($group); };
        $errmsg = $@ if $@;
        my $config = {}; # to return additional attributes
        return ($type, $group, $errmsg, $config);
    }
    return undef;
}

__PACKAGE__->register();
__PACKAGE__->init();

1;
