package PVE::CLI::ha_manager;

use strict;
use warnings;

use PVE::INotify;
use JSON;

use PVE::JSONSchema qw(get_standard_option);
use PVE::CLIHandler;
use PVE::Cluster;
use PVE::Tools qw(extract_param);
use PVE::RPCEnvironment;

use PVE::HA::Config; # needed for bash completion in PVE::HA::Tools!
use PVE::HA::Env::PVE2;
use PVE::HA::Tools;
use PVE::API2::HA::Resources;
use PVE::API2::HA::Groups;
use PVE::API2::HA::Rules;
use PVE::API2::HA::Status;

use base qw(PVE::CLIHandler);

my $nodename = PVE::INotify::nodename();

my $timestamp_to_status = sub {
    my ($ctime, $timestamp) = @_;

    my $tdiff = $ctime - $timestamp;
    if ($tdiff > 30) {
        return "old timestamp - dead?";
    } elsif ($tdiff < -2) {
        return "detected time drift!";
    } else {
        return "active";
    }
};

sub setup_environment {
    PVE::RPCEnvironment->setup_default_cli_env();
}

__PACKAGE__->register_method({
    name => 'status',
    path => 'status',
    method => 'GET',
    description => "Display HA manger status.",
    parameters => {
        additionalProperties => 0,
        properties => {
            verbose => {
                description => "Verbose output. Include complete CRM and LRM status (JSON).",
                type => 'boolean',
                default => 0,
                optional => 1,
            },
        },
    },
    returns => { type => 'null' },
    code => sub {
        my ($param) = @_;

        my $res = PVE::API2::HA::Status->status({});
        foreach my $e (@$res) {
            print "$e->{type} $e->{status}\n";
        }

        if ($param->{verbose}) {
            print "full cluster state:\n";
            my $data = PVE::API2::HA::Status->manager_status({});
            print to_json($data, { pretty => 1, canonical => 1 });
        }

        return undef;
    },
});

__PACKAGE__->register_method({
    name => 'stop',
    path => 'stop',
    method => 'POST',
    description => "Request the service to be stopped.",
    permissions => {
        check => ['perm', '/', ['Sys.Console']],
    },
    parameters => {
        additionalProperties => 0,
        properties => {
            sid => get_standard_option(
                'pve-ha-resource-or-vm-id',
                {
                    completion => \&PVE::HA::Tools::complete_sid,
                },
            ),
            timeout => {
                description => "Timeout in seconds. If set to 0 a hard stop will be performed.",
                type => 'integer',
                minimum => 0,
            },
        },
    },
    returns => { type => 'null' },
    code => sub {
        my ($param) = @_;

        my $sid = PVE::HA::Config::parse_sid(extract_param($param, 'sid'));

        PVE::HA::Config::service_is_ha_managed($sid);

        PVE::API2::HA::Resources::check_service_state($sid);

        PVE::HA::Config::queue_crm_commands("stop $sid $param->{timeout}");

        return undef;
    },
});

__PACKAGE__->register_method({
    name => 'node-maintenance-set',
    path => 'node-maintenance-set',
    method => 'POST',
    description => "Change the node-maintenance request state.",
    permissions => {
        check => ['perm', '/', ['Sys.Console']],
    },
    parameters => {
        additionalProperties => 0,
        properties => {
            node => get_standard_option('pve-node'),
            disable => {
                description => "Requests disabling or enabling maintenance-mode.",
                type => 'boolean',
            },
        },
    },
    returns => { type => 'null' },
    code => sub {
        my ($param) = @_;

        PVE::Cluster::check_node_exists($param->{node});

        my $cmd = $param->{disable} ? 'disable-node-maintenance' : 'enable-node-maintenance';
        PVE::HA::Config::queue_crm_commands("$cmd $param->{node}");

        return undef;
    },
});

my $print_resource_motion_output = sub {
    my ($cmd) = @_;

    return sub {
        my ($data) = @_;

        my $sid = $data->{sid};
        my $req_node = $data->{'requested-node'};

        if (my $blocking_resources = $data->{'blocking-resources'}) {
            my $err_msg = "cannot $cmd resource '$sid' to node '$req_node':\n\n";

            for my $blocking_resource (@$blocking_resources) {
                my ($csid, $cause) = $blocking_resource->@{qw(sid cause)};

                $err_msg .= "- resource '$csid' on target node '$req_node'";

                if ($cause eq 'resource-affinity') {
                    $err_msg .= " in negative affinity with resource '$sid'";
                }

                $err_msg .= "\n";
            }

            die $err_msg;
        }

        if ($data->{'comigrated-resources'}) {
            for my $csid ($data->{'comigrated-resources'}->@*) {
                print "also $cmd resource '$csid' in positive affinity with"
                    . " resource '$sid' to target node '$req_node'\n";
            }
        }
    };
};

our $cmddef = {
    status => [__PACKAGE__, 'status'],
    config => [
        'PVE::API2::HA::Resources',
        'index',
        [],
        {},
        sub {
            my $res = shift;
            foreach my $rec (sort { $a->{sid} cmp $b->{sid} } @$res) {
                my ($type, $name) = split(':', $rec->{sid}, 2);
                print "$type:$name\n";
                foreach my $k (sort keys %$rec) {
                    next
                        if $k eq 'digest'
                        || $k eq 'sid'
                        || $k eq 'type'
                        || $k eq 'errors';
                    print "\t$k $rec->{$k}\n";
                }
                if (my $errors = $rec->{errors}) {
                    foreach my $p (keys %$errors) {
                        warn "error: property '$p' - $errors->{$p}\n";
                    }
                }
                print "\n";
            }
        },
    ],
    groupconfig => [
        'PVE::API2::HA::Groups',
        'index',
        [],
        {},
        sub {
            my $res = shift;
            foreach my $rec (sort { $a->{group} cmp $b->{group} } @$res) {
                print "group: $rec->{group}\n";
                foreach my $k (sort keys %$rec) {
                    next
                        if $k eq 'digest'
                        || $k eq 'group'
                        || $k eq 'type';
                    print "\t$k $rec->{$k}\n";
                }
                print "\n";
            }
        },
    ],
    groupadd => ["PVE::API2::HA::Groups", 'create', ['group']],
    groupremove => ["PVE::API2::HA::Groups", 'delete', ['group']],
    groupset => ["PVE::API2::HA::Groups", 'update', ['group']],

    rules => {
        list => [
            'PVE::API2::HA::Rules',
            'index',
            [],
            {},
            sub {
                my ($data, $schema, $options) = @_;
                PVE::CLIFormatter::print_api_result($data, $schema, undef, $options);
            },
            $PVE::RESTHandler::standard_output_options,
        ],
        config => [
            'PVE::API2::HA::Rules',
            'index',
            [],
            {},
            sub {
                my ($data, $schema, $options) = @_;
                my $props_to_print = [
                    'enabled', 'state', 'rule', 'type', 'resources', 'comment',
                ];
                for my $rule (@$data) {
                    $rule->{enabled} = int(!exists($rule->{disable}));
                    $rule->{state} = $rule->{errors} ? 'ignored (conflicts)' : 'in use';
                }
                PVE::CLIFormatter::print_api_result($data, $schema, $props_to_print, $options);
            },
            $PVE::RESTHandler::standard_output_options,
        ],
        add => ['PVE::API2::HA::Rules', 'create_rule', ['type', 'rule']],
        remove => ['PVE::API2::HA::Rules', 'delete_rule', ['rule']],
        set => ['PVE::API2::HA::Rules', 'update_rule', ['type', 'rule']],
    },

    add => ["PVE::API2::HA::Resources", 'create', ['sid']],
    remove => ["PVE::API2::HA::Resources", 'delete', ['sid']],
    set => ["PVE::API2::HA::Resources", 'update', ['sid']],

    migrate => { alias => 'crm-command migrate' },
    relocate => { alias => 'crm-command relocate' },

    'crm-command' => {
        migrate => [
            "PVE::API2::HA::Resources",
            'migrate',
            ['sid', 'node'],
            {},
            $print_resource_motion_output->('migrate'),
        ],
        relocate => [
            "PVE::API2::HA::Resources",
            'relocate',
            ['sid', 'node'],
            {},
            $print_resource_motion_output->('relocate'),
        ],
        stop => [__PACKAGE__, 'stop', ['sid', 'timeout']],
        'node-maintenance' => {
            enable => [__PACKAGE__, 'node-maintenance-set', ['node'], { disable => 0 }],
            disable => [__PACKAGE__, 'node-maintenance-set', ['node'], { disable => 1 }],
        },
    },

};

1;
