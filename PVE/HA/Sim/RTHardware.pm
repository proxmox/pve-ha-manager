package PVE::HA::Sim::RTHardware;

# Simulate Hardware resources in Realtime by
# running CRM and LRM in sparate processes

use strict;
use warnings;
use POSIX qw(strftime EINTR);
use Data::Dumper;
use JSON; 
use IO::File;
use IO::Select;
use Fcntl qw(:DEFAULT :flock);
use File::Copy;
use File::Path qw(make_path remove_tree);

use Glib;

use Gtk3 '-init';

use PVE::HA::CRM;
use PVE::HA::LRM;

use PVE::HA::Sim::RTEnv;
use base qw(PVE::HA::Sim::Hardware);

sub new {
    my ($this, $testdir) = @_;

    my $class = ref($this) || $this;

    my $self = $class->SUPER::new($testdir);

    foreach my $node (sort keys %{$self->{nodes}}) {
	my $d = $self->{nodes}->{$node};

	$d->{crm} = undef; # create on power on
	$d->{lrm} = undef; # create on power on
    }

    $self->create_main_window();

    return $self;
}

sub get_time {
    my ($self) = @_;

    return time();
}

sub log {
    my ($self, $level, $msg, $id) = @_;

    chomp $msg;

    my $time = $self->get_time();

    $id = 'hardware' if !$id;

    my $text = sprintf("%-5s %10s %12s: $msg\n", $level, 
		       strftime("%H:%M:%S", localtime($time)), $id);

    $self->append_text($text);
}

# fixme: duplicate code in Env?
sub read_manager_status {
    my ($self) = @_;
    
    my $filename = "$self->{statusdir}/manager_status";

    return PVE::HA::Tools::read_json_from_file($filename, {});  
}

sub fork_daemon {
    my ($self, $lockfh, $type, $node) = @_;

    my @psync = POSIX::pipe();

    my $pid = fork();
    die "fork failed" if ! defined($pid);

    if ($pid == 0) { 

	close($lockfh) if defined($lockfh); # unlock global lock
	
	POSIX::close($psync[0]);

	my $outfh = $psync[1];

	my $fd = fileno (STDIN);
	close STDIN;
	POSIX::close(0) if $fd != 0;

	die "unable to redirect STDIN - $!" 
	    if !open(STDIN, "</dev/null");

	# redirect STDOUT
	$fd = fileno(STDOUT);
	close STDOUT;
	POSIX::close (1) if $fd != 1;

	die "unable to redirect STDOUT - $!" 
	    if !open(STDOUT, ">&", $outfh);

	STDOUT->autoflush (1);

	#  redirect STDERR to STDOUT
	$fd = fileno(STDERR);
	close STDERR;
	POSIX::close(2) if $fd != 2;

	die "unable to redirect STDERR - $!" 
	    if !open(STDERR, ">&1");
	
	STDERR->autoflush(1);

	if ($type eq 'crm') {

	    my $haenv = PVE::HA::Env->new('PVE::HA::Sim::RTEnv', $node, $self, 'crm');

	    my $crm = PVE::HA::CRM->new($haenv);

	    for (;;) {
		$haenv->loop_start_hook();

		if (!$crm->do_one_iteration()) {
		    $haenv->log("info", "daemon stopped");
		    exit (0);
		}

		$haenv->loop_end_hook();
	    }

	} else {

	    my $haenv = PVE::HA::Env->new('PVE::HA::Sim::RTEnv', $node, $self, 'lrm');

	    my $lrm = PVE::HA::LRM->new($haenv);

	    for (;;) {
		$haenv->loop_start_hook();

		if (!$lrm->do_one_iteration()) {
		    $haenv->log("info", "daemon stopped");
		    exit (0);
		}

		$haenv->loop_end_hook();
	    }
	}

	exit(-1);
    }

    # parent

    POSIX::close ($psync[1]);

    Glib::IO->add_watch($psync[0], ['in', 'hup'], sub {
	my ($fd, $cond) = @_;
	if ($cond eq 'in') {
	    my $readbuf;
	    if (my $count = POSIX::read($fd, $readbuf, 8192)) {
		$self->append_text($readbuf);
	    }
	    return 1;
	} else {
	    POSIX::close($fd);	
	    return 0;
	}
    });
    	
    return $pid;
}

# simulate hardware commands
# power <node> <on|off>
# network <node> <on|off>

sub sim_hardware_cmd {
    my ($self, $cmdstr, $logid) = @_;

    my $cstatus;

    # note: do not fork when we own the lock!
    my $code = sub {
	my ($lockfh) = @_;

	$cstatus = $self->read_hardware_status_nolock();

	my ($cmd, $node, $action) = split(/\s+/, $cmdstr);

	die "sim_hardware_cmd: no node specified" if !$node;
	die "sim_hardware_cmd: unknown action '$action'" if $action !~ m/^(on|off)$/;

	my $d = $self->{nodes}->{$node};
	die "sim_hardware_cmd: no such node '$node'\n" if !$d;

	$self->log('info', "execute $cmdstr", $logid);
	
	if ($cmd eq 'power') {
	    if ($cstatus->{$node}->{power} ne $action) {
		if ($action eq 'on') {	      
		    $d->{crm} = $self->fork_daemon($lockfh, 'crm', $node) if !$d->{crm};
		    $d->{lrm} = $self->fork_daemon($lockfh, 'lrm', $node) if !$d->{lrm};
		} else {
		    if ($d->{crm}) {
			$self->log('info', "crm on node '$node' killed by poweroff");
			kill(9, $d->{crm});
			$d->{crm} = undef;
		    }
		    if ($d->{lrm}) {
			$self->log('info', "lrm on node '$node' killed by poweroff");
			kill(9, $d->{lrm});
			$d->{lrm} = undef;
		    }
		}
	    }

	    $cstatus->{$node}->{power} = $action;
	    $cstatus->{$node}->{network} = $action;

	} elsif ($cmd eq 'network') {
	    $cstatus->{$node}->{network} = $action;
	} else {
	    die "sim_hardware_cmd: unknown command '$cmd'\n";
	}

	$self->write_hardware_status_nolock($cstatus);
    };

    my $res = $self->global_lock($code);

    # update GUI outside lock

    foreach my $node (keys %$cstatus) {
	my $d = $self->{nodes}->{$node};
	$d->{network_btn}->set_active($cstatus->{$node}->{network} eq 'on');
	$d->{power_btn}->set_active($cstatus->{$node}->{power} eq 'on');
    }

    return $res;
}

sub cleanup {
    my ($self) = @_;

    my @nodes = sort keys %{$self->{nodes}};
    foreach my $node (@nodes) {
	my $d = $self->{nodes}->{$node};

	if ($d->{crm}) {
	    kill 9, $d->{crm};
	    delete $d->{crm};
	}
	if ($d->{lrm}) {
	    kill 9, $d->{lrm};
	    delete $d->{lrm};
	}
    }
}

sub append_text {
    my ($self, $text) = @_;
    
    my $logview = $self->{gui}->{text_view} || die "GUI not ready";
    my $textbuf = $logview->get_buffer();

    $textbuf->insert_at_cursor($text, -1);
    my $lines = $textbuf->get_line_count();

    my $history = 102;
    
    if ($lines > $history) {
	my $start = $textbuf->get_iter_at_line(0);
	my $end =  $textbuf->get_iter_at_line($lines - $history);
	$textbuf->delete($start, $end);
    }

    $logview->scroll_to_mark($textbuf->get_insert(), 0.0, 1, 0.0, 1.0);
}

sub set_power_state {
    my ($self, $node) = @_;

    my $d = $self->{nodes}->{$node} || die "no such node '$node'";

    my $action = $d->{power_btn}->get_active() ? 'on' : 'off';
    
    $self->sim_hardware_cmd("power $node $action"); 
}

sub set_network_state {
    my ($self, $node) = @_;

    my $d = $self->{nodes}->{$node} || die "no such node '$node'";

    my $action = $d->{network_btn}->get_active() ? 'on' : 'off';
    
    $self->sim_hardware_cmd("network $node $action"); 
}

sub create_main_window {
    my ($self) = @_;

    my $w;

    my $window = Gtk3::Window->new();
    $window->set_title("Proxmox HA Simulator");

    $window->signal_connect( destroy => sub { Gtk3::main_quit(); });

    # create logview

    my $vbox = Gtk3::VBox->new(0, 0);
    my $f1 = Gtk3::Frame->new('Cluster Log');
    $vbox->pack_start($f1, 1, 1, 0);

    my $logview = Gtk3::TextView->new();
    $logview->set_editable(0);
    $logview->set_cursor_visible(0);

    $self->{gui}->{text_view} = $logview;

    my $swindow = Gtk3::ScrolledWindow->new();
    $swindow->set_size_request(800, 400);
    $swindow->add($logview);

    $f1->add($swindow);

    # create node control

    my $hbox = Gtk3::HBox->new(0, 10);
    $vbox->pack_start($hbox, 0, 0, 0);
    
    my $ngrid = Gtk3::Grid->new(); 
    $hbox->add($ngrid);

    $w = Gtk3::Label->new('Node');
    $ngrid->attach($w, 0, 0, 1, 1);
    $w = Gtk3::Label->new('Power');
    $ngrid->attach($w, 1, 0, 1, 1);
    $w = Gtk3::Label->new('Network');
    $ngrid->attach($w, 2, 0, 1, 1);
    $w = Gtk3::Label->new('Status');
    $ngrid->attach($w, 3, 0, 1, 1);
   
    my $row = 1;
    my @nodes = sort keys %{$self->{nodes}};
    foreach my $node (@nodes) {
	my $d = $self->{nodes}->{$node};

	$w = Gtk3::Label->new($node);
	$ngrid->attach($w, 0, $row, 1, 1);
	$w = Gtk3::Switch->new();
	$ngrid->attach($w, 1, $row, 1, 1);
	$d->{power_btn} = $w;
	$w->signal_connect('notify::active' => sub {
	    $self->set_power_state($node);
	}),

	$w = Gtk3::Switch->new();
	$ngrid->attach($w, 2, $row, 1, 1);
	$d->{network_btn} = $w;
	$w->signal_connect('notify::active' => sub {
	    $self->set_network_state($node);
	}),

	$w = Gtk3::Label->new('-');
	$ngrid->attach($w, 3, $row, 1, 1);
	$d->{node_status_label} = $w;

	$row++;
    }


    $window->add ($vbox);

    $window->show_all;
    $window->realize ();
}

sub run {
    my ($self) = @_;

    Glib::Timeout->add(1000, sub {

	# check all watchdogs
	my @nodes = sort keys %{$self->{nodes}};
	foreach my $node (@nodes) {
	    if (!$self->watchdog_check($node)) {
		$self->sim_hardware_cmd("power $node off", 'watchdog');
		$self->log('info', "server '$node' stopped by poweroff (watchdog)");
	    }
	}

	my $mstatus = $self->read_manager_status();
	my $node_status = $mstatus->{node_status} || {};

	foreach my $node (@nodes) {
	    my $ns = $node_status->{$node};
	    next if !$ns;
	    my $d = $self->{nodes}->{$node};
	    next if !$d;
	    my $sl = $d->{node_status_label};
	    next if !$sl;

	    if ($mstatus->{master_node} && ($mstatus->{master_node} eq $node)) {
		$sl->set_text(uc($ns));
	    } else {
		$sl->set_text($ns);
	    }
	}

	print Dumper($mstatus);

	#$d->{node_status_label}
    });

    Gtk3->main;

    $self->cleanup();
}

1;
