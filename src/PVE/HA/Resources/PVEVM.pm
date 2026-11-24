package PVE::HA::Resources::PVEVM;

use strict;
use warnings;

use PVE::Cluster;

use PVE::HA::Tools;

BEGIN {
    if (!$ENV{PVE_GENERATING_DOCS}) {
        require PVE::GuestIdentity;
        import PVE::GuestIdentity;
        require PVE::QemuConfig;
        import PVE::QemuConfig;
        require PVE::QemuServer;
        import PVE::QemuServer;
        require PVE::QemuServer::Monitor;
        import PVE::QemuServer::Monitor;
        require PVE::API2::Qemu;
        import PVE::API2::Qemu;
    }
}

use base qw(PVE::HA::Resources);

my $vm_migrate_json_properties;
# FIXME: the extra properties are missing from the docs, since the PVE::API2::Qemu module can't be
# loaded while generating the docs, because of cyclic dependencies.
my sub init_migrate_json_properties {
    my $combined = { $PVE::HA::Resources::common_migrate_json_properties->%* };
    my $extra_migrate_props = eval { PVE::API2::Qemu::ha_migrate_json_properties() } // {};
    die $@ if $@ && !$ENV{PVE_GENERATING_DOCS};
    $combined->{$_} = $extra_migrate_props->{$_} for keys $extra_migrate_props->%*;
    $vm_migrate_json_properties = $combined;
}

sub type {
    return 'vm';
}

sub verify_name {
    my ($class, $vmid) = @_;

    die "invalid VMID\n" if $vmid !~ m/^[1-9][0-9]+$/;
}

sub options {
    return {
        state => { optional => 1 },
        group => { optional => 1 },
        comment => { optional => 1 },
        failback => { optional => 1 },
        'auto-rebalance' => { optional => 1 },
        max_restart => { optional => 1 },
        max_relocate => { optional => 1 },
    };
}

sub config_file {
    my ($class, $vmid, $nodename) = @_;

    return PVE::QemuConfig->config_file($vmid, $nodename);
}

sub exists {
    my ($class, $vmid, $noerr) = @_;

    my $vmlist = PVE::Cluster::get_vmlist();
    my $entry = $vmlist->{ids}->{$vmid};

    if (!defined($entry)) {
        die "resource 'vm:$vmid' does not exist in cluster\n" if !$noerr;
        return undef;
    }

    if ($entry->{type} ne 'qemu') {
        die "resource 'vm:$vmid' - guest with ID $vmid is not a VM\n" if !$noerr;
        return undef;
    }

    return 1;
}

sub start {
    my ($class, $haenv, $id) = @_;

    my $nodename = $haenv->nodename();

    my $params = {
        node => $nodename,
        vmid => $id,
    };

    my $upid = PVE::API2::Qemu->vm_start($params);
    PVE::HA::Tools::upid_wait($upid, $haenv);
}

sub shutdown {
    my ($class, $haenv, $id, $timeout) = @_;

    my $nodename = $haenv->nodename();
    my $shutdown_timeout = $timeout // 60;

    my $upid;
    my $params = {
        node => $nodename,
        vmid => $id,
    };

    if ($shutdown_timeout) {
        $params->{timeout} = $shutdown_timeout;
        $params->{forceStop} = 1;
        $upid = PVE::API2::Qemu->vm_shutdown($params);
    } else {
        $upid = PVE::API2::Qemu->vm_stop($params);
    }

    PVE::HA::Tools::upid_wait($upid, $haenv);
}

sub migrate {
    my ($class, $haenv, $id, $target, $online, $migrate_options) = @_;

    my $nodename = $haenv->nodename();

    my $params = {
        node => $nodename,
        vmid => $id,
        # bug #2241 forces is for local resource only, people can ensure that
        # different host have the same hardware, so this can be fine, and qemu
        # knows when not, so can only win here
        force => 1,
        'with-local-disks' => 1,
        target => $target,
        online => $online,
    };

    $migrate_options //= {};
    $params->{$_} = $migrate_options->{$_} for keys $migrate_options->%*;

    # explicitly shutdown if $online isn't true (relocate)
    if (!$online && $class->check_running($haenv, $id)) {
        $class->shutdown($haenv, $id);
    }

    my $oldconfig = $class->config_file($id, $nodename);

    my $upid = PVE::API2::Qemu->migrate_vm($params);
    PVE::HA::Tools::upid_wait($upid, $haenv);

    # check if vm really moved
    return !(-f $oldconfig);
}

# Whether a relocation of the running VM writes its config before the 'migrate' lock gets set: the
# shutdown applies pending changes, and preparing the migration removes left-over fleecing images.
# FIXME: remove with PVE 10, as all nodes then keep identities
my sub relocation_writes_config {
    my ($vmid) = @_;

    my $conf = PVE::QemuConfig->load_config($vmid);
    return 1 if scalar(keys(($conf->{pending} // {})->%*));
    return defined($conf->{'special-sections'}->{fleecing});
}

sub assert_migration_target {
    my ($class, $haenv, $id, $target, $online) = @_;

    # FIXME: remove with PVE 10, as all nodes then keep identities
    # Like the migrate API, only refuse a node that would drop the identity the VM has, but before a
    # relocation stops the VM. A config write of the relocation also assigns an identity to a VM
    # without one.
    my $gets_identity =
        !$online && $class->check_running($haenv, $id) && relocation_writes_config($id);
    return if !$gets_identity && !PVE::QemuConfig->has_stored_identity($id);

    PVE::GuestIdentity::assert_node_supports_identity($target);
}

sub check_running {
    my ($class, $haenv, $vmid) = @_;

    my $nodename = $haenv->nodename();

    if (PVE::QemuServer::check_running($vmid, 1, $nodename)) {
        # do not count VMs which are suspended for a backup job as running
        my $conf = PVE::QemuConfig->load_config($vmid, $nodename);
        if (defined($conf->{lock}) && $conf->{lock} eq 'backup') {
            my $qmpstatus = eval { PVE::QemuServer::Monitor::mon_cmd($vmid, 'query-status') };
            $haenv->log('warning', "$@") if $@;

            return 0 if defined($qmpstatus) && $qmpstatus->{status} eq 'prelaunch';
        }

        return 1;
    } else {
        return 0;
    }
}

sub remove_locks {
    my ($self, $haenv, $id, $locks, $service_node) = @_;

    $service_node = $service_node || $haenv->nodename();

    my $conf = PVE::QemuConfig->load_config($id, $service_node);

    return undef if !defined($conf->{lock});

    foreach my $lock (@$locks) {
        if ($conf->{lock} eq $lock) {
            delete $conf->{lock};

            my $cfspath = PVE::QemuConfig->cfs_config_path($id, $service_node);
            PVE::Cluster::cfs_write_file($cfspath, $conf);

            return $lock;
        }
    }

    return undef;
}

sub get_static_stats_from_config {
    my ($class, $conf) = @_;

    # Ensure types are compatible with what Rust expects: f64 for maxcpu and usize for maxmem.
    return {
        maxcpu => 0.0 + PVE::QemuConfig->get_derived_property($conf, 'max-cpu'),
        maxmem => int(PVE::QemuConfig->get_derived_property($conf, 'max-memory')),
    };
}

sub migrate_json_properties {
    my ($class) = @_;
    init_migrate_json_properties() if !defined($vm_migrate_json_properties);
    return $vm_migrate_json_properties;
}

1;
