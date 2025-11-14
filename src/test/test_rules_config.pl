#!/usr/bin/perl

use strict;
use warnings;
use Getopt::Long;

use lib qw(..);

use JSON;

use Test::More;
use Test::MockModule;

use PVE::HA::Rules;
use PVE::HA::Rules::NodeAffinity;
use PVE::HA::Rules::ResourceAffinity;

PVE::HA::Rules::NodeAffinity->register();
PVE::HA::Rules::ResourceAffinity->register();

PVE::HA::Rules->init(property_isolation => 1);

my $opt_nodiff;

if (!GetOptions("nodiff" => \$opt_nodiff)) {
    print "usage: $0 [test.cfg] [--nodiff]\n";
    exit -1;
}

sub _log {
    my ($fh, $source, $message) = @_;

    chomp $message;
    $message = "[$source] $message" if $source;

    print "$message\n";

    $fh->print("$message\n");
    $fh->flush();
}

sub check_cfg {
    my ($cfg_fn, $outfile) = @_;

    my $raw = PVE::Tools::file_get_contents($cfg_fn);

    my $nodes = ['node1', 'node2', 'node3'];

    open(my $LOG, '>', "$outfile");
    select($LOG);
    $| = 1;

    print "--- Log ---\n";
    my $cfg = PVE::HA::Rules->parse_config($cfg_fn, $raw);
    PVE::HA::Rules->set_rule_defaults($_) for values %{ $cfg->{ids} };
    my $messages = PVE::HA::Rules->transform($cfg, $nodes);
    my $compiled_cfg = PVE::HA::Rules->compile($cfg, $nodes);
    print $_ for @$messages;
    print "--- Config ---\n";
    print to_json($cfg, { canonical => 1, pretty => 1, utf8 => 1 });
    print "--- Compiled Config ---\n";
    print to_json($compiled_cfg, { canonical => 1, pretty => 1, utf8 => 1 });

    select(STDOUT);
}

sub run_test {
    my ($cfg_fn) = @_;

    print "* check: $cfg_fn\n";

    my $outfile = "$cfg_fn.output";
    my $expect = "$cfg_fn.expect";

    eval { check_cfg($cfg_fn, $outfile); };
    if (my $err = $@) {
        die "Test '$cfg_fn' failed:\n$err\n";
    }

    return if $opt_nodiff;

    my $res;

    if (-f $expect) {
        my $cmd = ['diff', '-u', $expect, $outfile];
        $res = system(@$cmd);
        die "test '$cfg_fn' failed\n" if $res != 0;
    } else {
        $res = system('cp', $outfile, $expect);
        die "test '$cfg_fn' failed\n" if $res != 0;
    }

    print "* end rules test: $cfg_fn (success)\n\n";
}

# exec tests

if (my $testcfg = shift) {
    run_test($testcfg);
} else {
    for my $cfg (<rules_cfgs/*cfg>) {
        run_test($cfg);
    }
}
