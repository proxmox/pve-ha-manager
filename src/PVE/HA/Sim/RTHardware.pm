package PVE::HA::Sim::RTHardware;

# Simulate Hardware resources in Realtime by
# running CRM and LRM in separate processes

use strict;
use warnings;

use POSIX qw(strftime EINTR);
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

my $DEFAULT_MAXMEM = 4096;
my $DEFAULT_MAXCPU = 4;

sub new {
    my ($this, $testdir) = @_;

    my $class = ref($this) || $this;

    my $self = $class->SUPER::new($testdir);

    my $logfile = "$testdir/log";
    $self->{logfh} = IO::File->new(">$logfile")
        || die "unable to open '$logfile' - $!";

    foreach my $node (sort keys %{ $self->{nodes} }) {
        my $d = $self->{nodes}->{$node};

        $d->{crm} = undef; # create on power on
        $d->{crm_env} = PVE::HA::Env->new('PVE::HA::Sim::RTEnv', $node, $self, 'crm');

        $d->{lrm} = undef; # create on power on
        $d->{lrm_env} = PVE::HA::Env->new('PVE::HA::Sim::RTEnv', $node, $self, 'lrm');
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

    my $text =
        sprintf("%-5s %10s %12s: $msg\n", $level, strftime("%H:%M:%S", localtime($time)), $id);

    $self->append_text($text);
}

# fixme: duplicate code in Env?
sub read_manager_status {
    my ($self) = @_;

    my $filename = "$self->{statusdir}/manager_status";

    return PVE::HA::Tools::read_json_from_file($filename, {});
}

sub fork_daemon {
    my ($self, $lockfh, $type, $haenv) = @_;

    my @psync = POSIX::pipe();

    my $pid = fork();
    die "fork failed" if !defined($pid);

    if ($pid == 0) {

        close($lockfh) if defined($lockfh); # unlock global lock

        POSIX::close($psync[0]);

        my $outfh = $psync[1];

        my $fd = fileno(STDIN);
        close STDIN;
        POSIX::close(0) if $fd != 0;

        die "unable to redirect STDIN - $!"
            if !open(STDIN, "</dev/null");

        # redirect STDOUT
        $fd = fileno(STDOUT);
        close STDOUT;
        POSIX::close(1) if $fd != 1;

        die "unable to redirect STDOUT - $!"
            if !open(STDOUT, ">&", $outfh);

        STDOUT->autoflush(1);

        #  redirect STDERR to STDOUT
        $fd = fileno(STDERR);
        close STDERR;
        POSIX::close(2) if $fd != 2;

        die "unable to redirect STDERR - $!"
            if !open(STDERR, ">&1");

        STDERR->autoflush(1);

        if ($type eq 'crm') {

            my $crm = PVE::HA::CRM->new($haenv);

            for (;;) {
                if (!$crm->do_one_iteration()) {
                    $haenv->log("info", "daemon stopped");
                    exit(0);
                }
            }

        } else {

            my $lrm = PVE::HA::LRM->new($haenv);

            for (;;) {
                if (!$lrm->do_one_iteration()) {
                    $haenv->log("info", "daemon stopped");
                    exit(0);
                }
            }
        }

        POSIX::_exit(-1);
    }

    # parent

    POSIX::close($psync[1]);

    Glib::IO->add_watch(
        $psync[0],
        ['in', 'hup'],
        sub {
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
        },
    );

    return $pid;
}

# for controlling the resource manager services (CRM and LRM)
sub crm_control {
    my ($self, $action, $data, $lock_fh) = @_;

    if ($action eq 'start') {

        return $self->fork_daemon($lock_fh, 'crm', $data->{crm_env});

    } elsif ($action eq 'stop') {

        kill(9, $data->{crm});
        while (waitpid($data->{crm}, 0) != $data->{crm}) { }

    } else {
        die "unknown CRM control action: '$action'\n";
    }
}

sub lrm_control {
    my ($self, $action, $data, $lock_fh) = @_;

    if ($action eq 'start') {

        return $self->fork_daemon($lock_fh, 'lrm', $data->{lrm_env});

    } elsif ($action eq 'stop') {

        kill(9, $data->{lrm});
        while (waitpid($data->{lrm}, 0) != $data->{lrm}) { }

    } else {
        die "unknown LRM control action: '$action'\n";
    }

}

# simulate hardware commands
sub sim_hardware_cmd {
    my ($self, $cmdstr, $logid) = @_;

    my $cstatus = $self->SUPER::sim_hardware_cmd($cmdstr, $logid);

    # update GUI outside lock
    foreach my $node (keys %$cstatus) {
        my $d = $self->{nodes}->{$node};
        $d->{network_btn}->set_active($cstatus->{$node}->{network} eq 'on');
        $d->{power_btn}->set_active($cstatus->{$node}->{power} eq 'on');
    }

    return $cstatus;
}

sub cleanup {
    my ($self) = @_;

    my @nodes = sort keys %{ $self->{nodes} };
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

    $self->{logfh}->print($text);
    $self->{logfh}->flush();

    my $logview = $self->{gui}->{text_view} || die "GUI not ready";
    my $textbuf = $logview->get_buffer();

    my $end_iter = $textbuf->get_end_iter();
    $textbuf->insert($end_iter, $text, -1);

    my $lines = $textbuf->get_line_count();

    my $history = 102;

    if ($lines > $history) {
        my $start = $textbuf->get_iter_at_line(0);
        my $end = $textbuf->get_iter_at_line($lines - $history);
        $textbuf->delete($start, $end);
    }
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

sub create_node_control {
    my ($self) = @_;

    my $ngrid = Gtk3::Grid->new();
    $ngrid->set_row_spacing(2);
    $ngrid->set_column_spacing(5);
    $ngrid->set('margin-left', 5);

    my $w = Gtk3::Label->new('Node');
    $ngrid->attach($w, 0, 0, 1, 1);
    $w = Gtk3::Label->new('Power');
    $ngrid->attach($w, 1, 0, 1, 1);
    $w = Gtk3::Label->new('Network');
    $ngrid->attach($w, 2, 0, 1, 1);
    $w = Gtk3::Label->new('Status');
    $w->set_size_request(150, -1);
    $w->set_alignment(0, 0.5);
    $ngrid->attach($w, 3, 0, 1, 1);

    my $row = 1;

    my @nodes = sort keys %{ $self->{nodes} };

    foreach my $node (@nodes) {
        my $d = $self->{nodes}->{$node};

        $w = Gtk3::Label->new($node);
        $ngrid->attach($w, 0, $row, 1, 1);
        $w = Gtk3::Switch->new();
        $ngrid->attach($w, 1, $row, 1, 1);
        $d->{power_btn} = $w;
        $w->signal_connect(
            'notify::active' => sub {
                $self->set_power_state($node);
            },
            ),

            $w = Gtk3::Switch->new();
        $ngrid->attach($w, 2, $row, 1, 1);
        $d->{network_btn} = $w;
        $w->signal_connect(
            'notify::active' => sub {
                $self->set_network_state($node);
            },
            ),

            $w = Gtk3::Label->new('-');
        $w->set_alignment(0, 0.5);
        $ngrid->attach($w, 3, $row, 1, 1);
        $d->{node_status_label} = $w;

        $row++;
    }

    return $ngrid;
}

sub show_service_add_dialog {
    my ($self) = @_;

    my ($service_type, $service_id, $service_node) = ('', '', '');

    my $win = $self->{main_window};
    my $flags = [qw( modal destroy-with-parent )];
    my $dialog = Gtk3::Dialog->new_with_buttons('Add Service', $win, $flags);

    my $ok_btn = $dialog->add_button('_OK', 'ok');
    $ok_btn->set_sensitive(0);

    $dialog->add_button('_Cancel', 'cancel');

    my $service_description = Gtk3::Label->new(undef);
    $service_description->set_line_wrap(1);
    $service_description->set_width_chars(76);

    my $grid = Gtk3::Grid->new();
    $grid->set_row_spacing(2);
    $grid->set_column_spacing(5);
    $grid->set('margin', 5);

    my $w = Gtk3::Label->new('Type');
    $grid->attach($w, 0, 0, 1, 1);
    $w = Gtk3::Label->new('ID');
    $grid->attach($w, 1, 0, 1, 1);
    $w = Gtk3::Label->new('Node');
    $grid->attach($w, 2, 0, 1, 1);

    # service type combo box
    my $type_cb = Gtk3::ComboBoxText->new();
    my $service_types = ['vm', 'ct']; # TODO: PVE::HA::Resources->lookup_types();
    foreach my $type (@$service_types) {
        $type_cb->append_text($type);
    }

    $type_cb->signal_connect(
        'notify::active' => sub {
            my $w = shift;

            my $sel = $w->get_active();
            return if $sel < 0;

            $service_type = $service_types->[$sel];
        },
    );

    $type_cb->set_active(0);
    $grid->attach($type_cb, 0, 1, 1, 1);

    my $id_entry = Gtk3::Entry->new();
    $id_entry->set_max_length(7);
    $id_entry->signal_connect(
        'changed' => sub {
            my $w = shift;

            $service_id = $w->get_text();
            chomp $service_id;

            if ($service_id =~ m/^\d+$/) {
                my $sid = "$service_type:$service_id";
                if (!defined($self->{service_gui}->{$sid})) {
                    $ok_btn->set_sensitive(1);
                } else {
                    $ok_btn->set_sensitive(0);
                }
            } else {
                $ok_btn->set_sensitive(0);
            }
        },
    );

    $grid->attach($id_entry, 1, 1, 1, 1);

    my @nodes = sort keys %{ $self->{nodes} };
    my $node_cb = Gtk3::ComboBoxText->new();
    foreach my $node (@nodes) {
        $node_cb->append_text($node);
    }
    $node_cb->signal_connect(
        'notify::active' => sub {
            my $w = shift;

            $service_node = $node_cb->get_active_text();
        },
    );
    $node_cb->set_active(0);
    $grid->attach($node_cb, 2, 1, 1, 1);

    my $cpu_label = Gtk3::Label->new('CPU Count');
    $cpu_label->set_hexpand(1);
    $cpu_label->set_xalign(0);

    my $cpu_count_spin = Gtk3::SpinButton->new_with_range(1.0, 1024, 1.0);
    $cpu_count_spin->set_value($DEFAULT_MAXCPU);

    my $cpu_box = Gtk3::Box->new('horizontal', 6);
    $cpu_box->add($cpu_label);
    $cpu_box->add($cpu_count_spin);

    my $memory_label = Gtk3::Label->new('Memory (MiB)');
    $memory_label->set_hexpand(1);
    $memory_label->set_xalign(0);

    # There is an arbitrary limit of 10 TiB
    my $memory_spin = Gtk3::SpinButton->new_with_range(1.0, 10485760.0, 1.0);
    $memory_spin->set_value($DEFAULT_MAXMEM);

    my $memory_box = Gtk3::Box->new('horizontal', 6);
    $memory_box->add($memory_label);
    $memory_box->add($memory_spin);

    my $vbox = Gtk3::Box->new('vertical', 6);
    $vbox->set_margin_start(6);
    $vbox->set_margin_end(6);
    $vbox->set_margin_top(6);
    $vbox->set_margin_bottom(6);
    $vbox->add($grid);
    $vbox->add($cpu_box);
    $vbox->add($memory_box);
    $vbox->show_all();

    my $contarea = $dialog->get_content_area();
    $contarea->add($vbox);

    $dialog->show_all();
    my $res = $dialog->run();

    if (defined($res) && $res eq 'ok') {
        my $sid = "$service_type:$service_id";
        $self->sim_hardware_cmd("service $sid add $service_node", 'command');

        my $maxcpu = $cpu_count_spin->get_value();
        my $maxmemory = $memory_spin->get_value();
        $self->sim_hardware_cmd("service $sid set-static-stats $maxcpu $maxmemory", 'command');

        $self->add_service_to_gui($sid);
    }

    $dialog->destroy();
}

sub show_service_delete_dialog {
    my ($self, $sid) = @_;

    my $win = $self->{main_window};
    my $flags = [qw( modal destroy-with-parent )];

    my $dialog =
        Gtk3::MessageDialog->new($win, $flags, 'warning', 'yes_no', "Delete Service '$sid'?");

    $dialog->set_title("Delete $sid");

    my $res = $dialog->run();

    $dialog->destroy();

    if (defined($res) && $res eq 'yes') {
        $self->sim_hardware_cmd("service $sid delete", 'command');
        $self->delete_service_from_gui($sid);
    }
}

sub show_migrate_dialog {
    my ($self, $sid) = @_;

    my $dialog = Gtk3::Dialog->new();

    $dialog->set_title("Migrate $sid");
    $dialog->set_modal(1);
    $dialog->set_transient_for($self->{main_window});

    my $grid = Gtk3::Grid->new();
    $grid->set_row_spacing(2);
    $grid->set_column_spacing(5);
    $grid->set('margin', 5);

    my $w = Gtk3::Label->new('Target Node');
    $grid->attach($w, 0, 0, 1, 1);

    my @nodes = sort keys %{ $self->{nodes} };
    $w = Gtk3::ComboBoxText->new();
    foreach my $node (@nodes) {
        $w->append_text($node);
    }

    my $target = '';
    $w->signal_connect(
        'notify::active' => sub {
            my $w = shift;

            my $sel = $w->get_active();
            return if $sel < 0;

            $target = $nodes[$sel];
        },
    );
    $grid->attach($w, 1, 0, 1, 1);

    my $relocate_btn = Gtk3::CheckButton->new_with_label("stop service (relocate)");
    $grid->attach($relocate_btn, 1, 1, 1, 1);

    my $contarea = $dialog->get_content_area();

    $contarea->add($grid);

    $dialog->add_button("_OK", 'ok');
    $dialog->add_button("_Cancel", 'cancel');

    $dialog->show_all();
    my $res = $dialog->run();

    $dialog->destroy();

    if (defined($res) && $res eq 'ok' && $target) {
        if ($relocate_btn->get_active()) {
            $self->queue_crm_commands("relocate $sid $target");
        } else {
            $self->queue_crm_commands("migrate $sid $target");
        }
    }
}

sub add_service_to_gui {
    my ($self, $sid) = @_;

    my $sgrid = $self->{service_grid};

    die "service grid not initialised yet\n" if !defined($sgrid);

    die "service '$sid' has already an entry in service_gui table!\n"
        if defined($self->{service_gui}->{$sid});

    my $row = 0;
    while (my $label = $sgrid->get_child_at(0, $row)) {
        $row++;
    }

    $self->new_service_gui_entry($sid, $row);
}

sub delete_service_from_gui {
    my ($self, $sid) = @_;

    my $sgrid = $self->{service_grid};

    die "service grid not initialised yet" if !defined($sgrid);

    die "service '$sid' has no entry in service_gui table"
        if !defined($self->{service_gui}->{$sid});

    delete $self->{service_gui}->{$sid};

    my $row = 1;

    while (my $label = $sgrid->get_child_at(0, $row)) {
        if ($sid eq $label->get_text()) {
            $sgrid->remove_row($row);
            last;
        }

        $row++;
    }
}

sub new_service_gui_entry {
    my ($self, $sid, $row) = @_;

    my $d = $self->{service_config}->{$sid};

    my $sgrid = $self->{service_grid};

    $sgrid->insert_row($row);

    my $w = Gtk3::Label->new($sid);
    $sgrid->attach($w, 0, $row, 1, 1);

    $w = Gtk3::ComboBoxText->new();
    $sgrid->attach($w, 1, $row, 1, 1);

    my $count = 0;
    foreach my $state (qw(started stopped disabled ignored)) {
        $w->append_text($state);
        $w->set_active($count) if $d->{state} eq $state;
        $count++;
    }
    $w->signal_connect(
        changed => sub {
            my $w = shift;
            my $state = $w->get_active_text();
            $self->set_service_state($sid, $state);
        },
    );

    $w = Gtk3::Button->new('Migrate');
    $sgrid->attach($w, 2, $row, 1, 1);
    $w->signal_connect(
        clicked => sub {
            $self->show_migrate_dialog($sid);
        },
    );

    $w = Gtk3::Label->new($d->{node});
    $sgrid->attach($w, 3, $row, 1, 1);
    $self->{service_gui}->{$sid}->{node_label} = $w;

    $w = Gtk3::Label->new('-');
    $w->set_alignment(0, 0.5);
    $sgrid->attach($w, 4, $row, 1, 1);
    $self->{service_gui}->{$sid}->{status_label} = $w;

    $w = Gtk3::Button->new_from_icon_name('edit-delete', 1);
    $sgrid->attach($w, 5, $row, 1, 1);
    $w->signal_connect(
        clicked => sub {
            $self->show_service_delete_dialog($sid);
        },
    );
    $sgrid->show_all();
}

sub create_service_control {
    my ($self) = @_;

    my $sgrid = Gtk3::Grid->new();
    $sgrid->set_row_spacing(2);
    $sgrid->set_column_spacing(5);
    $sgrid->set('margin', 5);

    $self->{service_grid} = $sgrid;

    my $w = Gtk3::Label->new('Service ID');
    $sgrid->attach($w, 0, 0, 1, 1);
    $w = Gtk3::Label->new('Request State');
    $sgrid->attach($w, 1, 0, 1, 1);
    $w = Gtk3::Label->new('Node');
    $sgrid->attach($w, 3, 0, 1, 1);
    $w = Gtk3::Label->new('Status');
    $w->set_alignment(0, 0.5);
    $w->set_size_request(150, -1);
    $sgrid->attach($w, 4, 0, 1, 1);

    my $row = 1;
    my @nodes = keys %{ $self->{nodes} };

    foreach my $sid (sort keys %{ $self->{service_config} }) {
        $self->new_service_gui_entry($sid, $row);
        $row++;
    }

    $w = Gtk3::Button->new_from_icon_name('list-add', 1);
    $sgrid->attach($w, 5, $row, 1, 1);
    $w->signal_connect(
        clicked => sub {
            $self->show_service_add_dialog();
        },
    );

    return $sgrid;
}

sub create_log_view {
    my ($self) = @_;

    my $nb = Gtk3::Notebook->new();

    my $l1 = Gtk3::Label->new('Cluster Log');

    my $logview = Gtk3::TextView->new();
    $logview->set_editable(0);
    $logview->set_cursor_visible(0);

    $self->{gui}->{text_view} = $logview;

    my $swindow = Gtk3::ScrolledWindow->new();
    $swindow->set_size_request(1024, 768);
    $swindow->add($logview);

    $self->{gui}->{text_view_swindow} = $swindow;

    $logview->signal_connect(
        'size-allocate' => sub {
            my $swindow = $self->{gui}->{text_view_swindow};

            # swindows V-adjustment controls the child vertical scrollbar, set it to
            # its upper bound to scroll to the end every time child's size changes
            my $adjustment = $swindow->get_vadjustment();
            $adjustment->set_value($adjustment->get_upper());
        },
    );

    $nb->insert_page($swindow, $l1, 0);

    my $l2 = Gtk3::Label->new('Manager Status');

    my $statview = Gtk3::TextView->new();
    $statview->set_editable(0);
    $statview->set_cursor_visible(0);

    $self->{gui}->{stat_view} = $statview;

    $swindow = Gtk3::ScrolledWindow->new();
    $swindow->set_size_request(640, 400);
    $swindow->add($statview);

    $nb->insert_page($swindow, $l2, 1);
    return $nb;
}

sub create_main_window {
    my ($self) = @_;

    my $window = Gtk3::Window->new();
    $window->set_title("Proxmox HA Simulator");

    $window->signal_connect(destroy => sub { Gtk3::main_quit(); });

    $self->{main_window} = $window;

    my $grid = Gtk3::Grid->new();

    my $frame = $self->create_log_view();
    $grid->attach($frame, 0, 0, 1, 1);
    $frame->set('expand', 1);

    my $vbox = Gtk3::VBox->new(0, 0);
    $grid->attach($vbox, 1, 0, 1, 1);

    my $ngrid = $self->create_node_control();
    $vbox->pack_start($ngrid, 0, 0, 0);

    my $sep = Gtk3::HSeparator->new;
    $sep->set('margin-top', 10);
    $vbox->pack_start($sep, 0, 0, 0);

    my $sgrid = $self->create_service_control();
    $vbox->pack_start($sgrid, 0, 0, 0);

    $window->add($grid);

    $window->show_all;
    $window->realize();
}

sub run {
    my ($self) = @_;

    Glib::Timeout->add(
        1000,
        sub {

            $self->{service_config} = $self->read_service_config();

            # check all watchdogs
            my @nodes = sort keys %{ $self->{nodes} };
            foreach my $node (@nodes) {
                if (!$self->watchdog_check($node)) {
                    $self->sim_hardware_cmd("power $node off", 'watchdog');
                    $self->log('info', "server '$node' stopped by poweroff (watchdog)");
                }
            }

            my $mstatus = $self->read_manager_status();
            my $node_status = $mstatus->{node_status} || {};

            foreach my $node (@nodes) {
                my $ns = $node_status->{$node} || '-';
                my $d = $self->{nodes}->{$node};
                next if !$d;
                my $sl = $d->{node_status_label};
                next if !$sl;

                $ns .= " (master)"
                    if ($mstatus->{master_node} && ($mstatus->{master_node} eq $node));

                $sl->set_text($ns);
            }

            my $service_status = $mstatus->{service_status} || {};
            my @services = sort keys %{ $self->{service_config} };

            foreach my $sid (@services) {
                my $sc = $self->{service_config}->{$sid};
                my $ss = $service_status->{$sid};

                my $sgui = $self->{service_gui}->{$sid};
                next if !$sgui;
                my $nl = $sgui->{node_label};
                $nl->set_text($sc->{node});

                my $sl = $sgui->{status_label};
                next if !$sl;

                my $service_state = PVE::HA::Tools::get_verbose_service_state($ss, $sc);
                $sl->set_text($service_state);
            }

            if (my $sv = $self->{gui}->{stat_view}) {
                my $text = to_json($mstatus, { pretty => 1, canonical => 1 });
                my $textbuf = $sv->get_buffer();
                $textbuf->set_text($text, -1);
            }

            return 1; # repeat
        },
    );

    Gtk3->main;

    $self->cleanup();
}

1;
