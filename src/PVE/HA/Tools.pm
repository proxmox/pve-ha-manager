package PVE::HA::Tools;

use strict;
use warnings;
use JSON;

use PVE::JSONSchema;
use PVE::Tools;
use PVE::ProcFSTools;

# return codes used in the ha environment
# mainly by the resource agents
use constant {
    SUCCESS => 0, # action finished as expected
    ERROR => 1, # action was erroneous
    ETRY_AGAIN => 2, # action was erroneous and needs to be repeated
    EWRONG_NODE => 3, # needs to fixup the service location
    EUNKNOWN_SERVICE_TYPE => 4, # no plugin for this type service found
    EUNKNOWN_COMMAND => 5,
    EINVALID_PARAMETER => 6,
    EUNKNOWN_SERVICE => 7, # service not found
    IGNORED => 8, # action was ignored for some good reason
};

# get constants out of package in a somewhat easy way
use base 'Exporter';
our @EXPORT_OK = qw(SUCCESS ERROR EWRONG_NODE EUNKNOWN_SERVICE_TYPE
    EUNKNOWN_COMMAND EINVALID_PARAMETER ETRY_AGAIN EUNKNOWN_SERVICE IGNORED);
our %EXPORT_TAGS = ('exit_codes' => [@EXPORT_OK]);

PVE::JSONSchema::register_format('pve-ha-resource-id', \&pve_verify_ha_resource_id);

sub pve_verify_ha_resource_id {
    my ($sid, $noerr) = @_;

    if ($sid !~ m/^[a-z]+:\S+$/) {
        return undef if $noerr;
        die "value does not look like a valid ha resource id\n";
    }
    return $sid;
}

PVE::JSONSchema::register_standard_option(
    'pve-ha-resource-id',
    {
        description => "HA resource ID. This consists of a resource type followed by a resource"
            . " specific name, separated with colon (example: vm:100 / ct:100).",
        typetext => "<type>:<name>",
        type => 'string',
        format => 'pve-ha-resource-id',
    },
);

PVE::JSONSchema::register_format('pve-ha-resource-or-vm-id', \&pve_verify_ha_resource_or_vm_id);

sub pve_verify_ha_resource_or_vm_id {
    my ($sid, $noerr) = @_;

    if ($sid !~ m/^([a-z]+:\S+|\d+)$/) {
        return undef if $noerr;
        die "value does not look like a valid ha resource id\n";
    }
    return $sid;
}

PVE::JSONSchema::register_standard_option(
    'pve-ha-resource-or-vm-id',
    {
        description => "HA resource ID. This consists of a resource type followed by a resource"
            . " specific name, separated with colon (example: vm:100 / ct:100). For virtual machines and"
            . " containers, you can simply use the VM or CT id as a shortcut (example: 100).",
        typetext => "<type>:<name>",
        type => 'string',
        format => 'pve-ha-resource-or-vm-id',
    },
);

PVE::JSONSchema::register_format('pve-ha-group-node', \&pve_verify_ha_group_node);

sub pve_verify_ha_group_node {
    my ($node, $noerr) = @_;

    if ($node !~ m/^([a-zA-Z0-9]([a-zA-Z0-9\-]*[a-zA-Z0-9])?)(:\d+)?$/) {
        return undef if $noerr;
        die "value does not look like a valid ha group node\n";
    }
    return $node;
}

PVE::JSONSchema::register_standard_option(
    'pve-ha-group-node-list',
    {
        description => "List of cluster node names with optional priority.",
        verbose_description =>
            "List of cluster node members, where a priority can be given to each"
            . " node. A resource bound to a group will run on the available nodes with the highest"
            . " priority. If there are more nodes in the highest priority class, the services will get"
            . " distributed to those nodes. The priorities have a relative meaning only. The higher the"
            . " number, the higher the priority.",
        type => 'string',
        format => 'pve-ha-group-node-list',
        typetext => '<node>[:<pri>]{,<node>[:<pri>]}*',
    },
);

PVE::JSONSchema::register_standard_option(
    'pve-ha-group-id',
    {
        description => "The HA group identifier.",
        type => 'string',
        format => 'pve-configid',
    },
);

sub read_json_from_file {
    my ($filename, $default) = @_;

    my $data;

    if (defined($default) && (!-f $filename)) {
        $data = $default;
    } else {
        my $raw = PVE::Tools::file_get_contents($filename);
        $data = decode_json($raw);
    }

    return $data;
}

sub write_json_to_file {
    my ($filename, $data) = @_;

    my $raw = encode_json($data);

    PVE::Tools::file_set_contents($filename, $raw);
}

sub count_fenced_services {
    my ($ss, $node) = @_;

    my $count = 0;

    foreach my $sid (keys %$ss) {
        my $sd = $ss->{$sid};
        next if !$sd->{node};
        next if $sd->{node} ne $node;
        my $req_state = $sd->{state};
        next if !defined($req_state);
        if ($req_state eq 'fence') {
            $count++;
            next;
        }
    }

    return $count;
}

sub get_verbose_service_state {
    my ($service_state, $service_conf) = @_;

    return 'deleting' if !$service_conf;

    my $req = $service_conf->{state} // 'ignored';
    return 'ignored' if $req eq 'ignored';

    return 'not found' if !defined($service_conf->{node});

    # service not yet processed by manager
    return 'queued' if !defined($service_state);
    my $cur = $service_state->{state};

    # give fast feedback to the user
    my $state = $cur;
    if (!defined($cur)) {
        $state = 'queued';
    } elsif ($cur eq 'stopped') {
        if ($req eq 'started') {
            $state = 'starting';
        } elsif ($req eq 'disabled') {
            $state = 'disabled';
        }
    } elsif ($cur eq 'started') {
        if ($req eq 'stopped' || $req eq 'disabled') {
            $state = 'stopping';
        }
        $state = 'starting' if !$service_state->{running};
    } elsif ($cur eq 'error') {
        if ($req eq 'disabled') {
            $state = 'clearing error flag';
        }
    }

    return $state;
}

sub upid_wait {
    my ($upid, $haenv) = @_;

    my $waitfunc = sub {
        my $task = PVE::Tools::upid_encode(shift);
        $haenv->log('info', "Task '$task' still active, waiting");
    };

    PVE::ProcFSTools::upid_wait($upid, $waitfunc, 5);
}

# bash auto completion helper

# NOTE: we use PVE::HA::Config here without declaring an 'use' clause above as
# an hack. It uses the PVE::Cluster module from pve-cluster, which we do not
# have nor want as dependency in the simulator - where the completion helpers
# are never called. The PVE::CLI::ha_manager package pulls it in for us.

sub complete_sid {
    my ($cmd, $pname, $cur) = @_;

    my $cfg = PVE::HA::Config::read_resources_config();

    my $res = [];

    if ($cmd eq 'add') {

        my $vmlist = PVE::Cluster::get_vmlist();

        while (my ($vmid, $info) = each %{ $vmlist->{ids} }) {

            my $sid;

            if ($info->{type} eq 'lxc') {
                $sid = "ct:$vmid";
            } elsif ($info->{type} eq 'qemu') {
                $sid = "vm:$vmid";
            } else {
                next; # should not happen
            }

            next if $cfg->{ids}->{$sid};

            push @$res, $sid;
        }

    } else {

        foreach my $sid (keys %{ $cfg->{ids} }) {
            push @$res, $sid;
        }
    }

    return $res;
}

sub complete_enabled_sid {
    my $cfg = PVE::HA::Config::read_resources_config();

    my $res = [];
    foreach my $sid (keys %{ $cfg->{ids} }) {
        my $state = $cfg->{ids}->{$sid}->{state} // 'started';
        next if $state ne 'started';
        push @$res, $sid;
    }

    return $res;
}

sub complete_disabled_sid {
    my $cfg = PVE::HA::Config::read_resources_config();

    my $res = [];
    foreach my $sid (keys %{ $cfg->{ids} }) {
        my $state = $cfg->{ids}->{$sid}->{state} // 'started';
        next if $state eq 'started';
        push @$res, $sid;
    }

    return $res;
}

sub complete_group {
    my ($cmd, $pname, $cur) = @_;

    my $cfg = PVE::HA::Config::read_group_config();

    my $res = [];
    if ($cmd ne 'groupadd') {

        foreach my $group (keys %{ $cfg->{ids} }) {
            push @$res, $group;
        }

    }

    return $res;
}

1;
