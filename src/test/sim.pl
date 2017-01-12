#!/usr/bin/perl

use strict;
use warnings;

use lib '..';

use File::Path qw(remove_tree);

use PVE::HA::Sim::RTHardware;

sub show_usage {
    print "usage: $0 [testdir]\n";
    exit(-1);
};

my $testdir = shift;

if (!defined($testdir)) {
    $testdir = "simtest";
    remove_tree($testdir);
    mkdir $testdir;
}

my $hardware = PVE::HA::Sim::RTHardware->new($testdir);

$hardware->log('info', "starting simulation");

eval { $hardware->run(); };
if (my $err = $@) {
    $hardware->log('err', "exit simulation - $err ");
} else {
    $hardware->log('info', "exit simulation - done");
}

exit(0);
