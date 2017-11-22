package PVE::HA::Sim::TestHardware;

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

use PVE::HA::CRM;
use PVE::HA::LRM;

use PVE::HA::Sim::TestEnv;
use base qw(PVE::HA::Sim::Hardware);

my $max_sim_time = 10000;

sub new {
    my ($this, $testdir) = @_;

    my $class = ref($this) || $this;

    my $self = $class->SUPER::new($testdir);

    my $raw = PVE::Tools::file_get_contents("$testdir/cmdlist");
    $self->{cmdlist} = decode_json($raw);

    $self->{loop_count} = 0;
    $self->{cur_time} = 0;

    my $statusdir = $self->statusdir();
    my $logfile = "$statusdir/log";
    $self->{logfh} = IO::File->new(">>$logfile") ||
	die "unable to open '$logfile' - $!";

    foreach my $node (sort keys %{$self->{nodes}}) {

	my $d = $self->{nodes}->{$node};

	$d->{crm_env} = 
	    PVE::HA::Env->new('PVE::HA::Sim::TestEnv', $node, $self, 'crm');

	$d->{lrm_env} = 
	    PVE::HA::Env->new('PVE::HA::Sim::TestEnv', $node, $self, 'lrm');

	$d->{crm} = undef; # create on power on
	$d->{lrm} = undef; # create on power on
    }

    return $self;
}

sub get_time {
    my ($self) = @_;

    return $self->{cur_time};
}

sub log {
    my ($self, $level, $msg, $id) = @_;

    chomp $msg;

    my $time = $self->get_time();

    $id = 'hardware' if !$id;

    my $line = sprintf("%-5s %5d %12s: $msg\n", $level, $time, $id);
    print $line;

    $self->{logfh}->print($line);
    $self->{logfh}->flush();
}

# for controlling the resource manager services (CRM and LRM)
sub crm_control {
    my ($self, $action, $data, $lock_fh) = @_;

    if ($action eq 'start') {
	return PVE::HA::CRM->new($data->{crm_env});
    } elsif ($action eq 'stop') {
	# nothing todo sim_hardware_cmd sets us to undef, thats enough
    } elsif ($action eq 'shutdown') {
	$data->{crm}->shutdown_request();
    } else {
	die "unknown CRM control action: '$action'\n";
    }
}

sub lrm_control {
    my ($self, $action, $data, $lock_fh) = @_;

    if ($action eq 'start') {
	return PVE::HA::LRM->new($data->{lrm_env});
    } elsif ($action eq 'stop') {
	# nothing todo sim_hardware_cmd sets us to undef, thats enough
    } elsif ($action eq 'shutdown') {
	$data->{lrm}->shutdown_request();
    } else {
	die "unknown LRM control action: '$action'\n";
    }

}

sub run {
    my ($self) = @_;

    my $last_command_time = 0;
    my $next_cmd_at = 0;
	
    for (;;) {

	my $starttime = $self->get_time();

	my @nodes = sort keys %{$self->{nodes}};

	my $nodecount = scalar(@nodes);

	my $looptime = $nodecount*2;
	$looptime = 20 if $looptime < 20;

	die "unable to simulate so many nodes. You need to increate watchdog/lock timeouts.\n"
	    if $looptime >= 60;

	foreach my $node (@nodes) {

	    my $d = $self->{nodes}->{$node};
	    
	    if (my $crm = $d->{crm}) {

		my $exit_crm = !$crm->do_one_iteration();

		my $nodetime = $d->{crm_env}->get_time();
		$self->{cur_time} = $nodetime if $nodetime > $self->{cur_time};

		if ($exit_crm) {
		    $d->{crm_env}->log('info', "exit (loop end)");
		    $d->{crm} = undef;

		    my $cstatus = $self->read_hardware_status_nolock();
		    my $nstatus = $cstatus->{$node} || die "no node status for node '$node'";
		    my $shutdown = $nstatus->{shutdown} || '';
		    if ($shutdown eq 'reboot') {
			$self->sim_hardware_cmd("power $node off", 'reboot');
			$self->sim_hardware_cmd("power $node on", 'reboot');
		    } elsif ($shutdown eq 'shutdown') {
			$self->sim_hardware_cmd("power $node off", 'shutdown');
		    } elsif (!$d->{crm_stop}) {
			die "unexpected CRM exit - not implemented"
		    }
		    $d->{crm_stop} = undef;
		}
	    }

	    if (my $lrm = $d->{lrm}) {

		my $exit_lrm = !$lrm->do_one_iteration();

		my $nodetime = $d->{lrm_env}->get_time();
		$self->{cur_time} = $nodetime if $nodetime > $self->{cur_time};

		if ($exit_lrm) {
		    $d->{lrm_env}->log('info', "exit (loop end)");
		    $d->{lrm} = undef;
		    my $cstatus = $self->read_hardware_status_nolock();
		    my $nstatus = $cstatus->{$node} || die "no node status for node '$node'";
		    my $shutdown = $nstatus->{shutdown} || '';
		    if ($d->{lrm_restart}) {
			die "lrm restart during shutdown - not implemented" if $shutdown;
			$d->{lrm_restart} = undef;
			$d->{lrm} = PVE::HA::LRM->new($d->{lrm_env});
		    } elsif ($shutdown eq 'reboot' || $shutdown eq 'shutdown') {
			# exit the LRM before the CRM to reflect real world behaviour
			$self->sim_hardware_cmd("crm $node stop", $shutdown);
		    } else {
			die "unexpected LRM exit - not implemented"
		    }
		}
	    }

	    foreach my $n (@nodes) {
		if (!$self->watchdog_check($n)) {
		    $self->sim_hardware_cmd("power $n off", 'watchdog');
		    $self->log('info', "server '$n' stopped by poweroff (watchdog)");
		    $self->{nodes}->{$n}->{crm} = undef;
		    $self->{nodes}->{$n}->{lrm} = undef;
		}
	    }
	}

	
	$self->{cur_time} = $starttime + $looptime 
	    if ($self->{cur_time} - $starttime) < $looptime;

	die "simulation end\n" if $self->{cur_time} > $max_sim_time;

	foreach my $node (@nodes) {
	    my $d = $self->{nodes}->{$node};
	    # forced time update
	    $d->{lrm_env}->loop_start_hook();
	    $d->{crm_env}->loop_start_hook();
	}

	next if $self->{cur_time} < $next_cmd_at;
 
	# apply new comand after 5 loop iterations

	if (($self->{loop_count} % 5) == 0) {
	    my $list = shift @{$self->{cmdlist}};
	    if (!$list) {
		# end sumulation (500 seconds after last command)
		return if (($self->{cur_time} - $last_command_time) > 500);
	    }

	    foreach my $cmd (@$list) {
		$last_command_time = $self->{cur_time};

		if ($cmd =~ m/^delay\s+(\d+)\s*$/) {
		    $next_cmd_at = $self->{cur_time} + $1;
		} else {
		    $self->sim_hardware_cmd($cmd, 'cmdlist');
		}
	    }
	}

	++$self->{loop_count};
    }
}

1;
