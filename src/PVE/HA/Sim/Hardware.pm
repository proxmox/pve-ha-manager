package PVE::HA::Sim::Hardware;

# Simulate Hardware resources

# power supply for nodes: on/off
# network connection to nodes: on/off
# watchdog devices for nodes

use strict;
use warnings;
use POSIX qw(strftime EINTR);
use JSON;
use IO::File;
use Fcntl qw(:DEFAULT :flock);
use File::Copy;
use File::Path qw(make_path remove_tree);
use PVE::HA::FenceConfig;
use PVE::HA::Groups;

my $watchdog_timeout = 60;


# Status directory layout
#
# configuration
#
# $testdir/cmdlist                    Command list for simulation
# $testdir/hardware_status            Hardware description (number of nodes, ...)
# $testdir/manager_status             CRM status (start with {})
# $testdir/service_config             Service configuration
# $testdir/groups                     HA groups configuration
# $testdir/service_status_<node>      Service status
# $testdir/datacenter.cfg             Datacenter wide HA configuration

#
# runtime status for simulation system
#
# $testdir/status/cluster_locks        Cluster locks
# $testdir/status/hardware_status      Hardware status (power/network on/off)
# $testdir/status/watchdog_status      Watchdog status
#
# runtime status
#
# $testdir/status/lrm_status_<node>           LRM status
# $testdir/status/manager_status              CRM status
# $testdir/status/crm_commands                CRM command queue
# $testdir/status/service_config              Service configuration
# $testdir/status/service_status_<node>       Service status
# $testdir/status/groups                      HA groups configuration

sub read_lrm_status {
    my ($self, $node) = @_;

    my $filename = "$self->{statusdir}/lrm_status_$node";

    return PVE::HA::Tools::read_json_from_file($filename, {});
}

sub write_lrm_status {
    my ($self, $node, $status_obj) = @_;

    my $filename = "$self->{statusdir}/lrm_status_$node";

    PVE::HA::Tools::write_json_to_file($filename, $status_obj);
}

sub read_hardware_status_nolock {
    my ($self) = @_;

    my $filename = "$self->{statusdir}/hardware_status";

    my $raw = PVE::Tools::file_get_contents($filename);
    my $cstatus = decode_json($raw);

    return $cstatus;
}

sub write_hardware_status_nolock {
    my ($self, $cstatus) = @_;

    my $filename = "$self->{statusdir}/hardware_status";

    PVE::Tools::file_set_contents($filename, encode_json($cstatus));
};

sub read_service_config {
    my ($self) = @_;

    my $filename = "$self->{statusdir}/service_config";
    my $conf = PVE::HA::Tools::read_json_from_file($filename);

    foreach my $sid (keys %$conf) {
	my $d = $conf->{$sid};

	die "service '$sid' without assigned node!" if !$d->{node};

	if ($sid =~ m/^(vm|ct|fa):(\d+)$/) {
	    $d->{type} = $1;
	    $d->{name} = $2;
	} else {
	    die "implement me";
	}
	$d->{state} = 'disabled' if !$d->{state};
	$d->{state} = 'started' if $d->{state} eq 'enabled'; # backward compatibility
	$d->{max_restart} = 1 if !defined($d->{max_restart});
	$d->{max_relocate} = 1 if !defined($d->{max_relocate});
    }

    return $conf;
}

sub update_service_config {
    my ($self, $sid, $param) = @_;

    my $conf = $self->read_service_config();

    my $sconf = $conf->{$sid} || die "no such resource '$sid'\n";

    foreach my $k (%$param) {
	$sconf->{$k} = $param->{$k};
    }

    $self->write_service_config($conf);
}

sub write_service_config {
    my ($self, $conf) = @_;

    $self->{service_config} = $conf;

    my $filename = "$self->{statusdir}/service_config";
    return PVE::HA::Tools::write_json_to_file($filename, $conf);
}

sub read_fence_config {
    my ($self) = @_;

    my $raw = undef;

    my $filename = "$self->{statusdir}/fence.cfg";
    if (-e $filename) {
	$raw = PVE::Tools::file_get_contents($filename);
    }

    return PVE::HA::FenceConfig::parse_config($filename, $raw);
}

sub exec_fence_agent {
    my ($self, $agent, $node, @param) = @_;

    # let all agent succeed and behave the same for now
    $self->sim_hardware_cmd("power $node off", $agent);

    return 0; # EXIT_SUCCESS
}

sub set_service_state {
    my ($self, $sid, $state) = @_;

    my $conf = $self->read_service_config();
    die "no such service '$sid'" if !$conf->{$sid};

    $conf->{$sid}->{state} = $state;

    $self->write_service_config($conf);

    return $conf;
}

sub add_service {
    my ($self, $sid, $opts) = @_;

    my $conf = $self->read_service_config();
    die "resource ID '$sid' already defined\n" if $conf->{$sid};

    $conf->{$sid} = $opts;

    $self->write_service_config($conf);

    return $conf;
}

sub delete_service {
    my ($self, $sid) = @_;

    my $conf = $self->read_service_config();

    die "no such service '$sid'" if !$conf->{$sid};

    delete $conf->{$sid};

    $self->write_service_config($conf);

    return $conf;
}

sub change_service_location {
    my ($self, $sid, $current_node, $new_node) = @_;

    my $conf = $self->read_service_config();

    die "no such service '$sid'\n" if !$conf->{$sid};

    die "current_node for '$sid' does not match ($current_node != $conf->{$sid}->{node})\n"
	if $current_node ne $conf->{$sid}->{node};

    $conf->{$sid}->{node} = $new_node;

    $self->write_service_config($conf);
}

sub service_has_lock {
    my ($self, $sid) = @_;

    my $conf = $self->read_service_config();

    die "no such service '$sid'\n" if !$conf->{$sid};

    return $conf->{$sid}->{lock};
}

sub lock_service {
    my ($self, $sid, $lock) = @_;

    my $conf = $self->read_service_config();

    die "no such service '$sid'\n" if !$conf->{$sid};

    $conf->{$sid}->{lock} = $lock || 'backup';

    $self->write_service_config($conf);

    return $conf;
}

sub unlock_service {
    my ($self, $sid, $lock) = @_;

    my $conf = $self->read_service_config();

    die "no such service '$sid'\n" if !$conf->{$sid};

    if (!defined($conf->{$sid}->{lock})) {
	return undef;
    }

    if (defined($lock) && $conf->{$sid}->{lock} ne $lock) {
	warn "found lock '$conf->{$sid}->{lock}' trying to remove '$lock' lock\n";
	return undef;
    }

    my $removed_lock = delete $conf->{$sid}->{lock};

    $self->write_service_config($conf);

    return $removed_lock;
}

sub queue_crm_commands_nolock {
    my ($self, $cmd) = @_;

    chomp $cmd;

    my $data = '';
    my $filename = "$self->{statusdir}/crm_commands";
    if (-f $filename) {
	$data = PVE::Tools::file_get_contents($filename);
    }
    $data .= "$cmd\n";
    PVE::Tools::file_set_contents($filename, $data);

    return undef;
}

sub queue_crm_commands {
    my ($self, $cmd) = @_;

    my $code = sub { $self->queue_crm_commands_nolock($cmd); };

    $self->global_lock($code);

    return undef;
}

sub read_crm_commands {
    my ($self) = @_;

    my $code = sub {
	my $data = '';

 	my $filename = "$self->{statusdir}/crm_commands";
	if (-f $filename) {
	    $data = PVE::Tools::file_get_contents($filename);
	}
	PVE::Tools::file_set_contents($filename, '');

	return $data;
    };

    return $self->global_lock($code);
}

sub read_group_config {
    my ($self) = @_;

    my $filename = "$self->{statusdir}/groups";
    my $raw = '';
    $raw = PVE::Tools::file_get_contents($filename) if -f $filename;

    return PVE::HA::Groups->parse_config($filename, $raw);
}

sub read_service_status {
    my ($self, $node) = @_;

    my $filename = "$self->{statusdir}/service_status_$node";
    return PVE::HA::Tools::read_json_from_file($filename);
}

sub write_service_status {
    my ($self, $node, $data) = @_;

    my $filename = "$self->{statusdir}/service_status_$node";
    my $res = PVE::HA::Tools::write_json_to_file($filename, $data);

    # fixme: add test if a service runs on two nodes!!!

    return $res;
}

my $default_group_config = <<__EOD;
group: prefer_node1
    nodes node1
    nofailback 1

group: prefer_node2
    nodes node2
    nofailback 1

group: prefer_node3
    nodes node3
    nofailback 1
__EOD

sub new {
    my ($this, $testdir) = @_;

    die "missing testdir" if !$testdir;

    die "testdir '$testdir' does not exist or is not a directory!\n"
        if !-d $testdir;

    my $class = ref($this) || $this;

    my $self = bless {}, $class;

    my $statusdir = $self->{statusdir} = "$testdir/status";

    remove_tree($statusdir);
    mkdir $statusdir;

    # copy initial configuartion
    copy("$testdir/manager_status", "$statusdir/manager_status"); # optional

    if (-f "$testdir/groups") {
	copy("$testdir/groups", "$statusdir/groups");
    } else {
	PVE::Tools::file_set_contents("$statusdir/groups", $default_group_config);
    }

    if (-f "$testdir/service_config") {
	copy("$testdir/service_config", "$statusdir/service_config");
    } else {
	my $conf = {
	    'vm:101' => { node => 'node1', group => 'prefer_node1' },
	    'vm:102' => { node => 'node2', group => 'prefer_node2' },
	    'vm:103' => { node => 'node3', group => 'prefer_node3' },
	    'vm:104' => { node => 'node1', group => 'prefer_node1' },
	    'vm:105' => { node => 'node2', group => 'prefer_node2' },
	    'vm:106' => { node => 'node3', group => 'prefer_node3' },
	};
	$self->write_service_config($conf);
    }

    if (-f "$testdir/hardware_status") {
	copy("$testdir/hardware_status", "$statusdir/hardware_status") ||
	    die "Copy failed: $!\n";
    } else {
	my $cstatus = {
	    node1 => { power => 'off', network => 'off' },
	    node2 => { power => 'off', network => 'off' },
	    node3 => { power => 'off', network => 'off' },
	};
	$self->write_hardware_status_nolock($cstatus);
    }

    if (-f "$testdir/fence.cfg") {
	copy("$testdir/fence.cfg", "$statusdir/fence.cfg");
    }

    if (-f "$testdir/datacenter.cfg") {
	copy("$testdir/datacenter.cfg", "$statusdir/datacenter.cfg");
    }

    my $cstatus = $self->read_hardware_status_nolock();

    foreach my $node (sort keys %$cstatus) {
	$self->{nodes}->{$node} = {};

	if (-f "$testdir/service_status_$node") {
	    copy("$testdir/service_status_$node", "$statusdir/service_status_$node");
	} else {
	    $self->write_service_status($node, {});
	}
    }

    $self->{service_config} = $self->read_service_config();

    return $self;
}

sub get_time {
    my ($self) = @_;

    die "implement in subclass";
}

sub log {
    my ($self, $level, $msg, $id) = @_;

    chomp $msg;

    my $time = $self->get_time();

    $id = 'hardware' if !$id;

    printf("%-5s %5d %12s: $msg\n", $level, $time, $id);
}

sub statusdir {
    my ($self, $node) = @_;

    return $self->{statusdir};
}

sub read_datacenter_conf {
    my ($self, $node) = @_;

    my $filename = "$self->{statusdir}/datacenter.cfg";
    return PVE::HA::Tools::read_json_from_file($filename, {});
}

sub global_lock {
    my ($self, $code, @param) = @_;

    my $lockfile = "$self->{statusdir}/hardware.lck";
    my $fh = IO::File->new(">>$lockfile") ||
	die "unable to open '$lockfile'\n";

    my $success;
    for (;;) {
	$success = flock($fh, LOCK_EX);
	if ($success || ($! != EINTR)) {
	    last;
	}
	if (!$success) {
	    close($fh);
	    die "can't acquire lock '$lockfile' - $!\n";
	}
    }

    my $res;

    eval { $res = &$code($fh, @param) };
    my $err = $@;

    close($fh);

    die $err if $err;

    return $res;
}

my $compute_node_info = sub {
    my ($self, $cstatus) = @_;

    my $node_info = {};

    my $node_count = 0;
    my $online_count = 0;

    foreach my $node (keys %$cstatus) {
	my $d = $cstatus->{$node};

	my $online = ($d->{power} eq 'on' && $d->{network} eq 'on') ? 1 : 0;
	$node_info->{$node}->{online} = $online;

	$node_count++;
	$online_count++ if $online;
    }

    my $quorate = ($online_count > int($node_count/2)) ? 1 : 0;

    if (!$quorate) {
	foreach my $node (keys %$cstatus) {
	    my $d = $cstatus->{$node};
	    $node_info->{$node}->{online} = 0;
	}
    }

    return ($node_info, $quorate);
};

sub get_node_info {
    my ($self) = @_;

    my $cstatus = $self->read_hardware_status_nolock();
    my ($node_info, $quorate) = &$compute_node_info($self, $cstatus);

    return ($node_info, $quorate);
}

# helper for Sim/ only
sub get_cfs_state {
    my ($self, $node, $state) = @_;

    # TODO: ensure nolock is OK when adding this to RTSim
    my $cstatus = $self->read_hardware_status_nolock();
    my $res = $cstatus->{$node}->{cfs}->{$state};

    # we assume default true if not defined
    return !defined($res) || $res;
}

# simulate hardware commands
# power <node> <on|off>
# network <node> <on|off>
# cfs <node> <rw|update> <work|fail>
# reboot <node>
# shutdown <node>
# restart-lrm <node>
# service <sid> <started|disabled|stopped|ignored>
# service <sid> <migrate|relocate> <target>
# service <sid> lock/unlock [lockname]

sub sim_hardware_cmd {
    my ($self, $cmdstr, $logid) = @_;

    my $code = sub {
	my ($lock_fh) = @_;

	my $cstatus = $self->read_hardware_status_nolock();

	my ($cmd, $objid, $action, $param) = split(/\s+/, $cmdstr);

	die "sim_hardware_cmd: no node or service for command specified"
	    if !$objid;

	my ($node, $sid, $d);

	if ($cmd eq 'service') {
	    $sid = PVE::HA::Tools::pve_verify_ha_resource_id($objid);
	} else {
	    $node = $objid;
	    $d = $self->{nodes}->{$node} ||
		die "sim_hardware_cmd: no such node '$node'\n";
	}

	$self->log('info', "execute $cmdstr", $logid);

	if ($cmd eq 'power') {
	    die "sim_hardware_cmd: unknown action '$action'\n"
		if $action !~ m/^(on|off)$/;

	    if ($cstatus->{$node}->{power} ne $action) {
		if ($action eq 'on') {

		    $d->{crm} = $self->crm_control('start', $d, $lock_fh) if !defined($d->{crm});
		    $d->{lrm} = $self->lrm_control('start', $d, $lock_fh) if !defined($d->{lrm});
		    $d->{lrm_restart} = undef;
		    $cstatus->{$node}->{cfs} = {};

		} else {

		    if ($d->{crm}) {
			$d->{crm_env}->log('info', "killed by poweroff");
			$self->crm_control('stop', $d, $lock_fh);
			$d->{crm} = undef;
		    }
		    if ($d->{lrm}) {
			$d->{lrm_env}->log('info', "killed by poweroff");
			$self->lrm_control('stop', $d, $lock_fh);
			$d->{lrm} = undef;
			$d->{lrm_restart} = undef;
		    }

		    $self->watchdog_reset_nolock($node);
		    $self->write_service_status($node, {});
		}
	    }

	    $cstatus->{$node}->{power} = $action;
	    $cstatus->{$node}->{network} = $action;
	    $cstatus->{$node}->{shutdown} = undef;

	    $self->write_hardware_status_nolock($cstatus);

	} elsif ($cmd eq 'network') {
	    die "sim_hardware_cmd: unknown network action '$action'"
		if $action !~ m/^(on|off)$/;
	    $cstatus->{$node}->{network} = $action;

	    $self->write_hardware_status_nolock($cstatus);

	} elsif ($cmd eq 'cfs') {
	    die "sim_hardware_cmd: unknown cfs action '$action' for node '$node'"
		if $action !~ m/^(rw|update)$/;
	    die "sim_hardware_cmd: unknown cfs command '$param' for '$action' on node '$node'"
		if $param !~ m/^(work|fail)$/;

	    $cstatus->{$node}->{cfs}->{$action} = $param eq 'work';
	    $self->write_hardware_status_nolock($cstatus);

	} elsif ($cmd eq 'reboot' || $cmd eq 'shutdown') {
	    $cstatus->{$node}->{shutdown} = $cmd;

	    $self->write_hardware_status_nolock($cstatus);

	    $self->lrm_control('shutdown', $d, $lock_fh) if defined($d->{lrm});
	} elsif ($cmd eq 'restart-lrm') {
	    if ($d->{lrm}) {
		$d->{lrm_restart} = 1;
		$self->lrm_control('shutdown', $d, $lock_fh);
	    }
	} elsif ($cmd eq 'crm') {

	    if ($action eq 'stop') {
		if ($d->{crm}) {
		    $d->{crm_stop} = 1;
		    $self->crm_control('shutdown', $d, $lock_fh);
		}
	    } elsif ($action eq 'start') {
		$d->{crm} = $self->crm_control('start', $d, $lock_fh) if !defined($d->{crm});
	    } else {
		die "sim_hardware_cmd: unknown action '$action'";
	    }

	} elsif ($cmd eq 'service') {
	    if ($action eq 'started' || $action eq 'disabled' ||
		$action eq 'stopped' || $action eq 'ignored') {

		$self->set_service_state($sid, $action);

	    } elsif ($action eq 'migrate' || $action eq 'relocate') {

		die "sim_hardware_cmd: missing target node for '$action' command"
		    if !$param;

		$self->queue_crm_commands_nolock("$action $sid $param");

	    } elsif ($action eq 'add') {

		$self->add_service($sid, {state => 'started', node => $param});

	    } elsif ($action eq 'delete') {

		$self->delete_service($sid);

	    } elsif ($action eq 'lock') {

		$self->lock_service($sid, $param);

	    } elsif ($action eq 'unlock') {

		$self->unlock_service($sid, $param);

	    } else {
		die "sim_hardware_cmd: unknown service action '$action' " .
		    "- not implemented\n"
	    }
	} else {
	    die "sim_hardware_cmd: unknown command '$cmdstr'\n";
	}

	return $cstatus;
    };

    return $self->global_lock($code);
}

# for controlling the resource manager services
sub crm_control {
    my ($self, $action, $data, $lock_fh) = @_;

    die "implement in subclass";
}

sub lrm_control {
    my ($self, $action, $data, $lock_fh) = @_;

    die "implement in subclass";
}

sub run {
    my ($self) = @_;

    die "implement in subclass";
}

my $modify_watchog = sub {
    my ($self, $code) = @_;

    my $update_cmd = sub {

	my $filename = "$self->{statusdir}/watchdog_status";

	my ($res, $wdstatus);

	if (-f $filename) {
	    my $raw = PVE::Tools::file_get_contents($filename);
	    $wdstatus = decode_json($raw);
	} else {
	    $wdstatus = {};
	}

	($wdstatus, $res) = &$code($wdstatus);

	PVE::Tools::file_set_contents($filename, encode_json($wdstatus));

	return $res;
    };

    return $self->global_lock($update_cmd);
};

sub watchdog_reset_nolock {
    my ($self, $node) = @_;

    my $filename = "$self->{statusdir}/watchdog_status";

    if (-f $filename) {
 	my $raw = PVE::Tools::file_get_contents($filename);
	my $wdstatus = decode_json($raw);

	foreach my $id (keys %$wdstatus) {
	    delete $wdstatus->{$id} if $wdstatus->{$id}->{node} eq $node;
	}

	PVE::Tools::file_set_contents($filename, encode_json($wdstatus));
    }
}

sub watchdog_check {
    my ($self, $node) = @_;

    my $code = sub {
	my ($wdstatus) = @_;

	my $res = 1;

	foreach my $wfh (keys %$wdstatus) {
	    my $wd = $wdstatus->{$wfh};
	    next if $wd->{node} ne $node;

	    my $ctime = $self->get_time();
	    my $tdiff = $ctime - $wd->{update_time};

	    if ($tdiff > $watchdog_timeout) { # expired
		$res = 0;
		delete $wdstatus->{$wfh};
	    }
	}

	return ($wdstatus, $res);
    };

    return &$modify_watchog($self, $code);
}

my $wdcounter = 0;

sub watchdog_open {
    my ($self, $node) = @_;

    my $code = sub {
	my ($wdstatus) = @_;

	++$wdcounter;

	my $id = "WD:$node:$$:$wdcounter";

	die "internal error" if defined($wdstatus->{$id});

	$wdstatus->{$id} = {
	    node => $node,
	    update_time => $self->get_time(),
	};

	return ($wdstatus, $id);
    };

    return &$modify_watchog($self, $code);
}

sub watchdog_close {
    my ($self, $wfh) = @_;

    my $code = sub {
	my ($wdstatus) = @_;

	my $wd = $wdstatus->{$wfh};
	die "no such watchdog handle '$wfh'\n" if !defined($wd);

	my $tdiff = $self->get_time() - $wd->{update_time};
	die "watchdog expired" if $tdiff > $watchdog_timeout;

	delete $wdstatus->{$wfh};

	return ($wdstatus);
    };

    return &$modify_watchog($self, $code);
}

sub watchdog_update {
    my ($self, $wfh) = @_;

    my $code = sub {
	my ($wdstatus) = @_;

	my $wd = $wdstatus->{$wfh};

	die "no such watchdog handle '$wfh'\n" if !defined($wd);

	my $ctime = $self->get_time();
	my $tdiff = $ctime - $wd->{update_time};

	die "watchdog expired" if $tdiff > $watchdog_timeout;

	$wd->{update_time} = $ctime;

	return ($wdstatus);
    };

    return &$modify_watchog($self, $code);
}

1;
