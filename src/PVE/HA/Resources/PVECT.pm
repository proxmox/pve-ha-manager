package PVE::HA::Resources::PVECT;

use strict;
use warnings;

use PVE::Cluster;

use PVE::HA::Tools;

BEGIN {
    if (!$ENV{PVE_GENERATING_DOCS}) {
        require PVE::GuestIdentity;
        import PVE::GuestIdentity;
        require PVE::LXC;
        import PVE::LXC;
        require PVE::LXC::Config;
        import PVE::LXC::Config;
        require PVE::API2::LXC;
        import PVE::API2::LXC;
        require PVE::API2::LXC::Status;
        import PVE::API2::LXC::Status;
    }
}

use base qw(PVE::HA::Resources);

my $ct_migrate_json_properties;
# FIXME: the extra properties are missing from the docs, since the PVE::API2::LXC module can't be
# loaded while generating the docs, because of cyclic dependencies.
my sub init_migrate_json_properties {
    my $combined = { $PVE::HA::Resources::common_migrate_json_properties->%* };
    my $extra_migrate_props = eval { PVE::API2::LXC::ha_migrate_json_properties() } // {};
    die $@ if $@ && !$ENV{PVE_GENERATING_DOCS};
    $combined->{$_} = $extra_migrate_props->{$_} for keys $extra_migrate_props->%*;
    $ct_migrate_json_properties = $combined;
}

sub type {
    return 'ct';
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

    return PVE::LXC::Config->config_file($vmid, $nodename);
}

sub exists {
    my ($class, $vmid, $noerr) = @_;

    my $vmlist = PVE::Cluster::get_vmlist();
    my $entry = $vmlist->{ids}->{$vmid};

    if (!defined($entry)) {
        die "resource 'ct:$vmid' does not exist in cluster\n" if !$noerr;
        return undef;
    }

    if ($entry->{type} ne 'lxc') {
        die "resource 'ct:$vmid' - guest with ID $vmid is not a container\n" if !$noerr;
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

    my $upid = PVE::API2::LXC::Status->vm_start($params);
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
        $upid = PVE::API2::LXC::Status->vm_shutdown($params);
    } else {
        $upid = PVE::API2::LXC::Status->vm_stop($params);
    }

    PVE::HA::Tools::upid_wait($upid, $haenv);
}

sub migrate {
    my ($class, $haenv, $id, $target, $online, $migrate_options) = @_;

    my $nodename = $haenv->nodename();

    my $params = {
        node => $nodename,
        vmid => $id,
        target => $target,
        online => 0, # we cannot migrate CT (yet) online, only relocate
    };

    $migrate_options //= {};
    $params->{$_} = $migrate_options->{$_} for keys $migrate_options->%*;

    # always relocate container for now
    if ($class->check_running($haenv, $id)) {
        $class->shutdown($haenv, $id);
    }

    my $oldconfig = $class->config_file($id, $nodename);

    my $upid = PVE::API2::LXC->migrate_vm($params);
    PVE::HA::Tools::upid_wait($upid, $haenv);

    # check if vm really moved
    return !(-f $oldconfig);
}

sub assert_migration_target {
    my ($class, $haenv, $id, $target) = @_;

    # the migrate API refuses such a node as well, but only after the container got stopped
    PVE::GuestIdentity::assert_node_supports_identity($target);
}

sub check_running {
    my ($class, $haenv, $vmid) = @_;

    return PVE::LXC::check_running($vmid);
}

sub remove_locks {
    my ($self, $haenv, $id, $locks, $service_node) = @_;

    $service_node = $service_node || $haenv->nodename();

    my $conf = PVE::LXC::Config->load_config($id, $service_node);

    return undef if !defined($conf->{lock});

    foreach my $lock (@$locks) {
        if ($conf->{lock} eq $lock) {
            delete $conf->{lock};

            my $cfspath = PVE::LXC::Config->cfs_config_path($id, $service_node);
            PVE::Cluster::cfs_write_file($cfspath, $conf);

            return $lock;
        }
    }

    return undef;
}

sub get_static_stats_from_config {
    my ($class, $conf) = @_;

    # Ensure types are compatible with what Rust expects: f64 for maxcpu and usize for maxmem.
    # Note that there might be a non-integer 'cpulimit'.
    return {
        maxcpu => 0.0 + PVE::LXC::Config->get_derived_property($conf, 'max-cpu'),
        maxmem => int(PVE::LXC::Config->get_derived_property($conf, 'max-memory')),
    };
}

sub migrate_json_properties {
    my ($class) = @_;
    init_migrate_json_properties() if !defined($ct_migrate_json_properties);
    return $ct_migrate_json_properties;
}

1;
