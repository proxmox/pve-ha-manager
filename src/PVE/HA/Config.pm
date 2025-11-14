package PVE::HA::Config;

use strict;
use warnings;

use JSON;

use PVE::HA::Tools;
use PVE::HA::Groups;
use PVE::HA::Rules;
use PVE::HA::Rules::ResourceAffinity qw(get_affinitive_resources);
use PVE::Cluster qw(cfs_register_file cfs_read_file cfs_write_file cfs_lock_file);
use PVE::HA::Resources;

my $manager_status_filename = "ha/manager_status";
my $ha_groups_config = "ha/groups.cfg";
my $ha_resources_config = "ha/resources.cfg";
my $ha_rules_config = "ha/rules.cfg";
my $crm_commands_filename = "ha/crm_commands";
my $ha_fence_config = "ha/fence.cfg";

cfs_register_file(
    $crm_commands_filename,
    sub { my ($fn, $raw) = @_; return defined($raw) ? $raw : ''; },
    sub { my ($fn, $raw) = @_; return $raw; },
);
cfs_register_file(
    $ha_groups_config,
    sub { PVE::HA::Groups->parse_config(@_); },
    sub { PVE::HA::Groups->write_config(@_); },
);
cfs_register_file(
    $ha_resources_config,
    sub { PVE::HA::Resources->parse_config(@_); },
    sub { PVE::HA::Resources->write_config(@_); },
);
cfs_register_file(
    $ha_rules_config,
    sub { PVE::HA::Rules->parse_config(@_); },
    sub { PVE::HA::Rules->write_config(@_); },
);
cfs_register_file($manager_status_filename, \&json_reader, \&json_writer);
cfs_register_file(
    $ha_fence_config,
    \&PVE::HA::FenceConfig::parse_config,
    \&PVE::HA::FenceConfig::write_config,
);

sub json_reader {
    my ($filename, $data) = @_;

    return defined($data) && length($data) > 0 ? decode_json($data) : {};
}

sub json_writer {
    my ($filename, $data) = @_;

    return encode_json($data);
}

sub read_lrm_status {
    my ($node) = @_;

    die "undefined node" if !defined($node);

    my $cfs_path = "nodes/$node/lrm_status";

    my $raw = PVE::Cluster::get_config($cfs_path);
    if (!defined($raw)) {
        # ENOENT -> possible deleted node, don't die here as it breaks our node
        # 'gone' logic
        warn "unable to read file '/etc/pve/$cfs_path'\n";
        # unkown mode set explicitly as 'active' is assumed as default..
        return { mode => 'unknown' } if !-e "/etc/pve/$cfs_path";
    }

    return json_reader(undef, $raw);
}

sub write_lrm_status {
    my ($node, $status_obj) = @_;

    die "undefined node" if !defined($node);

    my $filename = "/etc/pve/nodes/$node/lrm_status";

    PVE::HA::Tools::write_json_to_file($filename, $status_obj);
}

sub parse_groups_config {
    my ($filename, $raw) = @_;

    return PVE::HA::Groups->parse_config($filename, $raw);
}

sub parse_resources_config {
    my ($filename, $raw) = @_;

    return PVE::HA::Resources->parse_config($filename, $raw);
}

sub read_resources_config {

    return cfs_read_file($ha_resources_config);
}

# checks if resource exists and sets defaults for unset values
sub read_and_check_resources_config {

    my $res = cfs_read_file($ha_resources_config);

    my $vmlist = PVE::Cluster::get_vmlist();
    my $conf = {};

    foreach my $sid (keys %{ $res->{ids} }) {
        my $d = $res->{ids}->{$sid};
        my (undef, undef, $name) = parse_sid($sid);
        $d->{state} = 'started' if !defined($d->{state});
        $d->{state} = 'started' if $d->{state} eq 'enabled'; # backward compatibility
        $d->{failback} = 1 if !defined($d->{failback});
        $d->{max_restart} = 1 if !defined($d->{max_restart});
        $d->{max_relocate} = 1 if !defined($d->{max_relocate});
        if (PVE::HA::Resources->lookup($d->{type})) {
            if (my $vmd = $vmlist->{ids}->{$name}) {
                $d->{node} = $vmd->{node};
                $conf->{$sid} = $d;
            } else {
                # undef $d->{node} is handled in get_verbose_service_state and
                # status API, don't spam logs or ignore it; allow to delete it!
                $conf->{$sid} = $d;
            }
        }
    }

    # TODO PVE 10: Remove digest when HA groups have been fully migrated to rules
    return wantarray ? ($conf, $res->{digest}) : $conf;
}

sub update_resources_config {
    my ($sid, $param, $delete, $digest) = @_;

    lock_ha_domain(
        sub {
            my $cfg = read_resources_config();
            ($sid, my $type, my $name) = parse_sid($sid);

            PVE::SectionConfig::assert_if_modified($cfg, $digest);

            my $scfg = $cfg->{ids}->{$sid}
                || die "no such resource '$sid'\n";

            my $plugin = PVE::HA::Resources->lookup($scfg->{type});
            my $opts = $plugin->check_config($sid, $param, 0, 1);

            foreach my $k (%$opts) {
                $scfg->{$k} = $opts->{$k};
            }

            if ($delete) {
                my $options = $plugin->private()->{options}->{$type};
                foreach my $k (PVE::Tools::split_list($delete)) {
                    my $d = $options->{$k}
                        || die "no such option '$k'\n";
                    die "unable to delete required option '$k'\n"
                        if !$d->{optional};
                    die "unable to delete fixed option '$k'\n"
                        if $d->{fixed};
                    delete $scfg->{$k};
                }
            }

            write_resources_config($cfg);
        },
        "update resources config failed",
    );
}

sub parse_sid {
    my ($sid) = @_;

    my ($type, $name);

    if ($sid =~ m/^(\d+)$/) {
        $name = $1;

        my $vmlist = PVE::Cluster::get_vmlist();
        if (defined($vmlist->{ids}->{$name})) {
            my $vm_type = $vmlist->{ids}->{$name}->{type};
            if ($vm_type eq 'lxc') {
                $type = 'ct';
            } elsif ($vm_type eq 'qemu') {
                $type = 'vm';
            } else {
                die "internal error";
            }
            $sid = "$type:$name";
        } else {
            die "unable do detect SID from VMID - VM/CT $1 does not exist\n";
        }
    } elsif ($sid =~ m/^(\S+):(\S+)$/) {
        $name = $2;
        $type = $1;
    } else {
        die "unable to parse service id '$sid'\n";
    }

    return wantarray ? ($sid, $type, $name) : $sid;
}

sub read_rules_config {

    return cfs_read_file($ha_rules_config);
}

sub read_and_check_rules_config {

    my $rules = cfs_read_file($ha_rules_config);

    # set optional rule parameter's default values
    for my $rule (values %{ $rules->{ids} }) {
        PVE::HA::Rules->set_rule_defaults($rule);
    }

    return $rules;
}

sub read_and_check_effective_rules_config {

    my $rules = read_and_check_rules_config();

    my $manager_status = read_manager_status();
    my $nodes = [keys $manager_status->{node_status}->%*];

    # TODO PVE 10: Remove group migration when HA groups have been fully migrated to location rules
    my $groups = read_group_config();
    my $resources = read_and_check_resources_config();

    PVE::HA::Groups::migrate_groups_to_rules($rules, $groups, $resources);

    PVE::HA::Rules->transform($rules, $nodes);

    return $rules;
}

sub write_rules_config {
    my ($cfg) = @_;

    cfs_write_file($ha_rules_config, $cfg);
}

sub read_group_config {

    return cfs_read_file($ha_groups_config);
}

sub have_groups_been_migrated {
    my ($groups) = @_;

    $groups = read_group_config() if !$groups;

    return 1 if !$groups;
    return keys $groups->{ids}->%* < 1;
}

sub delete_group_config {

    unlink "/etc/pve/$ha_groups_config" or die "failed to remove group config: $!\n";
}

sub write_group_config {
    my ($cfg) = @_;

    cfs_write_file($ha_groups_config, $cfg);
}

sub write_resources_config {
    my ($cfg) = @_;

    cfs_write_file($ha_resources_config, $cfg);
}

sub read_manager_status {
    my () = @_;

    return cfs_read_file($manager_status_filename);
}

sub write_manager_status {
    my ($status_obj) = @_;

    cfs_write_file($manager_status_filename, $status_obj);
}

sub read_fence_config {
    my () = @_;

    cfs_read_file($ha_fence_config);
}

sub write_fence_config {
    my ($cfg) = @_;

    cfs_write_file($ha_fence_config, $cfg);
}

sub lock_ha_domain {
    my ($code, $errmsg) = @_;

    my $res = PVE::Cluster::cfs_lock_domain("ha", undef, $code);
    my $err = $@;
    if ($err) {
        $errmsg ? die "$errmsg: $err" : die $err;
    }
    return $res;
}

sub queue_crm_commands {
    my ($cmd) = @_;

    chomp $cmd;

    my $code = sub {
        my $data = cfs_read_file($crm_commands_filename);
        $data .= "$cmd\n";
        cfs_write_file($crm_commands_filename, $data);
    };

    return lock_ha_domain($code);
}

sub any_pending_crm_command {
    my $data = cfs_read_file($crm_commands_filename);
    return defined($data) && length $data;
}

sub read_crm_commands {

    my $code = sub {
        my $data = cfs_read_file($crm_commands_filename);
        cfs_write_file($crm_commands_filename, '');
        return $data;
    };

    return lock_ha_domain($code);
}

my $service_check_ha_state = sub {
    my ($conf, $sid, $has_state) = @_;

    if (my $d = $conf->{ids}->{$sid}) {
        if (!defined($has_state)) {
            # ignored service behave as if they were not managed by HA
            return 0 if defined($d->{state}) && $d->{state} eq 'ignored';
            return 1;
        }

        # backward compatibility
        $has_state = 'started' if $has_state eq 'enabled';

        $d->{state} = 'started'
            if !defined($d->{state})
            || ($d->{state} eq 'enabled');

        return 1 if $d->{state} eq $has_state;
    }

    return undef;
};

# cannot use service_is_ha_managed as it skips 'ignored' services, see bug #1602
sub service_is_configured {
    my ($sid) = @_;

    my $conf = read_resources_config();
    if (defined($conf->{ids}) && defined($conf->{ids}->{$sid})) {
        return 1;
    }
    return 0;
}

sub get_resource_motion_info {
    my ($sid) = @_;

    my $resources = read_resources_config();

    my $dependent_resources = [];
    my $blocking_resources_by_node = {};

    if (&$service_check_ha_state($resources, $sid)) {
        my $manager_status = read_manager_status();
        my $ss = $manager_status->{service_status};
        my $ns = $manager_status->{node_status};

        my $rules = read_and_check_effective_rules_config();
        my ($together, $separate) = get_affinitive_resources($rules, $sid);

        for my $csid (sort keys %$together) {
            next if !defined($ss->{$csid});
            next if $ss->{$csid}->{state} eq 'ignored';

            push @$dependent_resources, $csid;
        }

        for my $node (keys %$ns) {
            next if $ns->{$node} ne 'online';

            for my $csid (sort keys %$separate) {
                next if !defined($ss->{$csid});
                next if $ss->{$csid}->{state} eq 'ignored';
                next if $ss->{$csid}->{node} && $ss->{$csid}->{node} ne $node;
                next if $ss->{$csid}->{target} && $ss->{$csid}->{target} ne $node;

                push $blocking_resources_by_node->{$node}->@*,
                    {
                        sid => $csid,
                        cause => 'resource-affinity',
                    };
            }
        }
    }

    return ($dependent_resources, $blocking_resources_by_node);
}

# graceful, as long as locking + cfs_write works
sub delete_service_from_config {
    my ($sid) = @_;

    return 1 if !service_is_configured($sid);

    my $res;
    PVE::HA::Config::lock_ha_domain(
        sub {
            my $conf = read_resources_config();
            $res = delete $conf->{ids}->{$sid};
            write_resources_config($conf);

        },
        "delete resource failed",
    );

    return !!$res;
}

sub vm_is_ha_managed {
    my ($vmid, $has_state) = @_;

    my $conf = cfs_read_file($ha_resources_config);

    my $types = PVE::HA::Resources->lookup_types();
    foreach my $type ('vm', 'ct') {
        return 1 if &$service_check_ha_state($conf, "$type:$vmid", $has_state);
    }

    return undef;
}

sub service_is_ha_managed {
    my ($sid, $has_state, $noerr) = @_;

    my $conf = cfs_read_file($ha_resources_config);

    return 1 if &$service_check_ha_state($conf, $sid, $has_state);

    die "resource '$sid' is not HA managed\n" if !$noerr;

    return undef;
}

sub get_service_status {
    my ($sid) = @_;

    my $status = { managed => 0 };

    my $conf = cfs_read_file($ha_resources_config);

    if (&$service_check_ha_state($conf, $sid)) {
        my $manager_status = cfs_read_file($manager_status_filename);

        $status->{managed} = 1;
        $status->{group} = $conf->{ids}->{$sid}->{group};
        $status->{state} = $manager_status->{service_status}->{$sid}->{state};
    }

    return $status;
}

1;
