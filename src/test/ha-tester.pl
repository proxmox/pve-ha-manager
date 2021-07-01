#!/usr/bin/perl

use strict;
use warnings;
use Getopt::Long;

use File::Path qw(make_path remove_tree);

my $opt_nodiff;
my $opt_nofail;

if (!GetOptions(
    "no-diff" => \$opt_nodiff,
    "c|continue-on-fail" => \$opt_nofail,
)) {
    print "usage: $0 testdir [--no-diff] [-c | --continue-on-fail]\n";
    exit -1;
}

sub do_run_test {
    my $dir = shift;

    $dir =~ s!/+$!!;

    print "run: $dir\n";

    my $logfile = "$dir/status/log";
    my $logexpect = "$dir/log.expect";

    my $res = system("perl -I ../ ../pve-ha-tester $dir");
    return "Test '$dir' failed\n" if $res != 0;

    return if $opt_nodiff;

    if (-f $logexpect) {
	my $cmd = ['diff', '-u', $logexpect, $logfile]; 
	$res = system(@$cmd);
	return "test '$dir' failed\n" if $res != 0;
    } else {
	$res = system('cp', $logfile, $logexpect);
	return "test '$dir' failed\n" if $res != 0;
    }
    print "end: $dir (success)\n";

    return undef;
}

my sub handle_test_result {
    my ($res, $test) = @_;

    return if !defined($res); # undef -> passed

    die "$res\n" if !$opt_nofail;
    warn "$res\n";
}

sub run_test {
    my $dir = shift;

    my $res = do_run_test($dir);

    handle_test_result($res, $dir);
}

if (my $testdir = shift) {
    run_test($testdir);
} else {
    foreach my $dir (<test-*>) {
	run_test($dir);
    }
}



