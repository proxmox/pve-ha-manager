package PVE::API2::HA::Rules;

use strict;
use warnings;

use HTTP::Status qw(:constants);

use Storable qw(dclone);

use PVE::Cluster qw(cfs_read_file);
use PVE::Exception;
use PVE::INotify;
use PVE::JSONSchema qw(get_standard_option);
use PVE::Tools qw(extract_param);

use PVE::HA::Config;
use PVE::HA::Groups;
use PVE::HA::Rules;

use base qw(PVE::RESTHandler);

my $get_api_ha_rule = sub {
    my ($rules, $ruleid, $rule_errors) = @_;

    die "no such ha rule '$ruleid'\n" if !$rules->{ids}->{$ruleid};

    my $cfg = dclone($rules->{ids}->{$ruleid});
    my ($type, $resources, $nodes) = $cfg->@{qw(type resources nodes)};

    $cfg->{rule} = $ruleid;
    $cfg->{digest} = $rules->{digest};
    $cfg->{order} = $rules->{order}->{$ruleid};

    # set optional rule parameter's default values
    PVE::HA::Rules->set_rule_defaults($cfg);

    $cfg->{resources} = PVE::HA::Rules->encode_value($type, 'resources', $resources)
        if $resources;
    $cfg->{nodes} = PVE::HA::Rules->encode_value($type, 'nodes', $nodes) if $nodes;
    $cfg->{errors} = $rule_errors if $rule_errors;

    return $cfg;
};

my $assert_resources_are_configured = sub {
    my ($resources) = @_;

    my $unconfigured_resources = [];

    for my $resource (sort keys %$resources) {
        push @$unconfigured_resources, $resource
            if !PVE::HA::Config::service_is_configured($resource);
    }

    die "cannot use unmanaged resource(s) " . join(', ', @$unconfigured_resources) . ".\n"
        if @$unconfigured_resources;
};

my $assert_nodes_do_exist = sub {
    my ($nodes) = @_;

    my $nonexistant_nodes = [];
    my $localnode = PVE::INotify::nodename();

    for my $node (sort keys %$nodes) {
        # check_node_exists(...) does not account for single-node setups
        next if $node eq $localnode;

        push @$nonexistant_nodes, $node
            if !PVE::Cluster::check_node_exists($node, 1);
    }

    die "cannot use non-existant node(s) " . join(', ', @$nonexistant_nodes) . ".\n"
        if @$nonexistant_nodes;
};

my $get_full_rules_config = sub {
    my ($rules) = @_;

    # set optional rule parameter's default values
    for my $rule (values %{ $rules->{ids} }) {
        PVE::HA::Rules->set_rule_defaults($rule);
    }

    # TODO PVE 10: Remove group migration when HA groups have been fully migrated to location rules
    my $groups = PVE::HA::Config::read_group_config();
    my $resources = PVE::HA::Config::read_and_check_resources_config();

    PVE::HA::Groups::migrate_groups_to_rules($rules, $groups, $resources);

    return $rules;
};

my $check_feasibility = sub {
    my ($rules) = @_;

    $rules = dclone($rules);

    $rules = $get_full_rules_config->($rules);

    return PVE::HA::Rules->check_feasibility($rules);
};

my $assert_feasibility = sub {
    my ($rules, $ruleid) = @_;

    my $global_errors = $check_feasibility->($rules);
    my $rule_errors = $global_errors->{$ruleid};

    return if !$rule_errors;

    # stringify error messages
    for my $opt (keys %$rule_errors) {
        $rule_errors->{$opt} = join(', ', @{ $rule_errors->{$opt} });
    }

    my $param = {
        code => HTTP_BAD_REQUEST,
        errors => $rule_errors,
    };

    my $exc = PVE::Exception->new("Rule '$ruleid' is invalid.\n", %$param);

    my ($pkg, $filename, $line) = caller;

    $exc->{filename} = $filename;
    $exc->{line} = $line;

    die $exc;
};

__PACKAGE__->register_method({
    name => 'index',
    path => '',
    method => 'GET',
    description => "Get HA rules.",
    permissions => {
        check => ['perm', '/', ['Sys.Audit']],
    },
    parameters => {
        additionalProperties => 0,
        properties => {
            type => {
                type => 'string',
                description => "Limit the returned list to the specified rule type.",
                enum => PVE::HA::Rules->lookup_types(),
                optional => 1,
            },
            resource => {
                type => 'string',
                description =>
                    "Limit the returned list to rules affecting the specified resource.",
                completion => \&PVE::HA::Tools::complete_sid,
                optional => 1,
            },
        },
    },
    returns => {
        type => 'array',
        items => {
            type => 'object',
            properties => {
                rule => { type => 'string' },
            },
            links => [{ rel => 'child', href => '{rule}' }],
        },
    },
    code => sub {
        my ($param) = @_;

        my $type = extract_param($param, 'type');
        my $state = extract_param($param, 'state');
        my $resource = extract_param($param, 'resource');

        my $rules = PVE::HA::Config::read_rules_config();
        $rules = $get_full_rules_config->($rules);

        my $global_errors = $check_feasibility->($rules);

        my $res = [];

        PVE::HA::Rules::foreach_rule(
            $rules,
            sub {
                my ($rule, $ruleid) = @_;

                my $rule_errors = $global_errors->{$ruleid};
                my $cfg = $get_api_ha_rule->($rules, $ruleid, $rule_errors);

                push @$res, $cfg;
            },
            {
                type => $type,
                sid => $resource,
            },
        );

        return $res;
    },
});

__PACKAGE__->register_method({
    name => 'read_rule',
    method => 'GET',
    path => '{rule}',
    description => "Read HA rule.",
    permissions => {
        check => ['perm', '/', ['Sys.Audit']],
    },
    parameters => {
        additionalProperties => 0,
        properties => {
            rule => get_standard_option(
                'pve-ha-rule-id',
                { completion => \&PVE::HA::Tools::complete_rule },
            ),
        },
    },
    returns => {
        type => 'object',
        properties => {
            rule => get_standard_option('pve-ha-rule-id'),
            type => {
                type => 'string',
            },
        },
    },
    code => sub {
        my ($param) = @_;

        my $ruleid = extract_param($param, 'rule');

        my $rules = PVE::HA::Config::read_rules_config();
        $rules = $get_full_rules_config->($rules);

        my $global_errors = $check_feasibility->($rules);
        my $rule_errors = $global_errors->{$ruleid};

        return $get_api_ha_rule->($rules, $ruleid, $rule_errors);
    },
});

__PACKAGE__->register_method({
    name => 'create_rule',
    method => 'POST',
    path => '',
    description => "Create HA rule.",
    permissions => {
        check => ['perm', '/', ['Sys.Console']],
    },
    protected => 1,
    parameters => PVE::HA::Rules->createSchema(),
    returns => {
        type => 'null',
    },
    code => sub {
        my ($param) = @_;

        PVE::Cluster::check_cfs_quorum();
        mkdir("/etc/pve/ha");

        die "cannot create ha rule: ha groups have not been migrated yet\n"
            if !PVE::HA::Config::have_groups_been_migrated();

        my $type = extract_param($param, 'type');
        my $ruleid = extract_param($param, 'rule');

        my $plugin = PVE::HA::Rules->lookup($type);

        my $opts = $plugin->check_config($ruleid, $param, 1, 1);

        PVE::HA::Config::lock_ha_domain(
            sub {
                my $rules = PVE::HA::Config::read_rules_config();

                die "HA rule '$ruleid' already defined\n" if $rules->{ids}->{$ruleid};

                $assert_resources_are_configured->($opts->{resources});
                $assert_nodes_do_exist->($opts->{nodes}) if $opts->{nodes};

                $rules->{order}->{$ruleid} = PVE::HA::Rules::get_next_ordinal($rules);
                $rules->{ids}->{$ruleid} = $opts;

                $assert_feasibility->($rules, $ruleid);

                PVE::HA::Config::write_rules_config($rules);
            },
            "create ha rule failed",
        );

        return undef;
    },
});

__PACKAGE__->register_method({
    name => 'update_rule',
    method => 'PUT',
    path => '{rule}',
    description => "Update HA rule.",
    permissions => {
        check => ['perm', '/', ['Sys.Console']],
    },
    protected => 1,
    parameters => PVE::HA::Rules->updateSchema(),
    returns => {
        type => 'null',
    },
    code => sub {
        my ($param) = @_;

        die "cannot update ha rule: ha groups have not been migrated yet\n"
            if !PVE::HA::Config::have_groups_been_migrated();

        my $ruleid = extract_param($param, 'rule');
        my $digest = extract_param($param, 'digest');
        my $delete = extract_param($param, 'delete');

        if ($delete) {
            $delete = [PVE::Tools::split_list($delete)];
        }

        PVE::HA::Config::lock_ha_domain(
            sub {
                my $rules = PVE::HA::Config::read_rules_config();

                PVE::SectionConfig::assert_if_modified($rules, $digest);

                my $rule = $rules->{ids}->{$ruleid} || die "HA rule '$ruleid' does not exist\n";

                my $type = $rule->{type};
                my $plugin = PVE::HA::Rules->lookup($type);
                my $opts = $plugin->check_config($ruleid, $param, 0, 1);

                $assert_resources_are_configured->($opts->{resources});
                $assert_nodes_do_exist->($opts->{nodes}) if $opts->{nodes};

                my $options = $plugin->private()->{options}->{$type};
                PVE::SectionConfig::delete_from_config($rule, $options, $opts, $delete);

                $rule->{$_} = $opts->{$_} for keys $opts->%*;

                $assert_feasibility->($rules, $ruleid);

                PVE::HA::Config::write_rules_config($rules);
            },
            "update HA rules failed",
        );

        return undef;
    },
});

__PACKAGE__->register_method({
    name => 'delete_rule',
    method => 'DELETE',
    path => '{rule}',
    description => "Delete HA rule.",
    permissions => {
        check => ['perm', '/', ['Sys.Console']],
    },
    protected => 1,
    parameters => {
        additionalProperties => 0,
        properties => {
            rule => get_standard_option(
                'pve-ha-rule-id',
                { completion => \&PVE::HA::Tools::complete_rule },
            ),
        },
    },
    returns => {
        type => 'null',
    },
    code => sub {
        my ($param) = @_;

        my $ruleid = extract_param($param, 'rule');

        PVE::HA::Config::lock_ha_domain(
            sub {
                my $rules = PVE::HA::Config::read_rules_config();

                delete $rules->{ids}->{$ruleid};

                PVE::HA::Config::write_rules_config($rules);
            },
            "delete ha rule failed",
        );

        return undef;
    },
});

1;
