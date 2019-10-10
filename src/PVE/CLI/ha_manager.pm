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

__PACKAGE__->register_method ({
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
	    }
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
	    print to_json($data, { pretty => 1, canonical => 1} );
	}

	return undef;
    }});

__PACKAGE__->register_method ({
    name => 'stop',
    path => 'stop',
    method => 'POST',
    description => "Request the service to be stopped.",
    permissions => {
	check => ['perm', '/', [ 'Sys.Console' ]],
    },
    parameters => {
	additionalProperties => 0,
	properties => {
	    sid => get_standard_option('pve-ha-resource-or-vm-id',
				      { completion => \&PVE::HA::Tools::complete_sid }),
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
    }});

our $cmddef = {
    status => [ __PACKAGE__, 'status'],
    config => [ 'PVE::API2::HA::Resources', 'index', [], {}, sub {
	my $res = shift;
	foreach my $rec (sort { $a->{sid} cmp $b->{sid} } @$res) {
	    my ($type, $name) = split(':', $rec->{sid}, 2);
	    print "$type:$name\n";
	    foreach my $k (sort keys %$rec) {
		next if $k eq 'digest' || $k eq 'sid' ||
		    $k eq 'type' || $k eq 'errors';
		print "\t$k $rec->{$k}\n";
	    }
	    if (my $errors = $rec->{errors}) {
		foreach my $p (keys %$errors) {
		    warn "error: property '$p' - $errors->{$p}\n";
		}
	    }
	    print "\n";
	}}],
    groupconfig => [ 'PVE::API2::HA::Groups', 'index', [], {}, sub {
	my $res = shift;
	foreach my $rec (sort { $a->{group} cmp $b->{group} } @$res) {
	    print "group: $rec->{group}\n";
	    foreach my $k (sort keys %$rec) {
		next if $k eq 'digest' || $k eq 'group' ||
		    $k eq 'type';
		print "\t$k $rec->{$k}\n";
	    }
	    print "\n";
	}}],
    groupadd => [ "PVE::API2::HA::Groups", 'create', ['group'] ],
    groupremove => [ "PVE::API2::HA::Groups", 'delete', ['group'] ],
    groupset => [ "PVE::API2::HA::Groups", 'update', ['group'] ],

    add => [ "PVE::API2::HA::Resources", 'create', ['sid'] ],
    remove => [ "PVE::API2::HA::Resources", 'delete', ['sid'] ],
    set => [ "PVE::API2::HA::Resources", 'update', ['sid'] ],

    migrate => { alias => 'crm-command migrate' },
    relocate => { alias => 'crm-command relocate' },

    'crm-command' => {
	migrate => [ "PVE::API2::HA::Resources", 'migrate', ['sid', 'node'] ],
	relocate => [ "PVE::API2::HA::Resources", 'relocate', ['sid', 'node'] ],
	stop => [ __PACKAGE__, 'stop', ['sid', 'timeout'] ],
    }

};

1;
