package PVE::HA::Resources::PVEVM;

use strict;
use warnings;

use PVE::Cluster;

use PVE::HA::Tools;

BEGIN {
    if (!$ENV{PVE_GENERATING_DOCS}) {
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

sub type {
    return 'vm';
}

sub verify_name {
    my ($class, $name) = @_;

    die "invalid VMID\n" if $name !~ m/^[1-9][0-9]+$/;
}

sub options {
    return {
        state => { optional => 1 },
        group => { optional => 1 },
        comment => { optional => 1 },
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

    if (!defined($vmlist->{ids}->{$vmid})) {
        die "resource 'vm:$vmid' does not exist in cluster\n" if !$noerr;
        return undef;
    } else {
        return 1;
    }
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
    my ($class, $haenv, $id, $target, $online) = @_;

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

sub check_running {
    my ($class, $haenv, $vmid) = @_;

    my $nodename = $haenv->nodename();

    if (PVE::QemuServer::check_running($vmid, 1, $nodename)) {
        # do not count VMs which are suspended for a backup job as running
        my $conf = PVE::QemuConfig->load_config($vmid, $nodename);
        if (defined($conf->{lock}) && $conf->{lock} eq 'backup') {
            my $qmpstatus = eval { PVE::QemuServer::Monitor::mon_cmd($vmid, 'query-status') };
            warn "$@\n" if $@;

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

sub get_static_stats {
    my ($class, $haenv, $id, $service_node) = @_;

    my $conf = PVE::QemuConfig->load_config($id, $service_node);

    return {
        maxcpu => PVE::QemuConfig->get_derived_property($conf, 'max-cpu'),
        maxmem => PVE::QemuConfig->get_derived_property($conf, 'max-memory'),
    };
}

1;
