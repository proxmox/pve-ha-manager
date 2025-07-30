package PVE::HA::Sim::Resources::VirtFail;

use strict;
use warnings;

use base qw(PVE::HA::Sim::Resources);

# This module lets us simulate failing resources for the regression tests.
#
# To make it more interesting we can encode some behavior in the VMID
# with the following format, where fa: is the type and a, b, c, ... are digits
# in base 10, i.e. the full service ID would be one of the following formats:
#   fa:abc
#   fa:abcd
#   fa:abcde
#   fa:abcdef
# And the digits after the fa: type prefix would mean:
#   - a: no meaning but can be used for differentiating similar resources
#   - b: how many tries are needed to start correctly (0 is normal behavior) (should be set)
#   - c: how many tries are needed to migrate correctly (0 is normal behavior) (should be set)
#   - d: should shutdown be successful (0 = yes, anything else no) (optional)
#   - e: return value of $plugin->exists() defaults to 1 if not set (optional)
#   - f: limits the constraints of b and c to the nodeX (0 = apply to all nodes) (optional)

my $decode_id = sub {
    my $id = shift;

    my ($start, $migrate, $stop, $exists, $limit_to_node) =
        $id =~ /^\d(\d)(\d)(\d)?(\d)?(\d)?/g;

    $start = 0 if !defined($start);
    $migrate = 0 if !defined($migrate);
    $stop = 0 if !defined($stop);
    $exists = 1 if !defined($exists);
    $limit_to_node = 0 if !defined($limit_to_node);

    return ($start, $migrate, $stop, $exists, $limit_to_node);
};

my $tries = {
    start => {},
    migrate => {},
};

sub type {
    return 'fa';
}

sub exists {
    my ($class, $id, $noerr) = @_;

    my (undef, undef, undef, $exists) = &$decode_id($id);
    print $exists . "\n";

    return $exists;
}

sub start {
    my ($class, $haenv, $id) = @_;

    my ($start_failure_count, $limit_to_node) = ($decode_id->($id))[0, 4];

    if ($limit_to_node == 0 || $haenv->nodename() eq "node$limit_to_node") {
        $tries->{start}->{$id} = 0 if !$tries->{start}->{$id};
        $tries->{start}->{$id}++;

        return if $start_failure_count >= $tries->{start}->{$id};
    }

    $tries->{start}->{$id} = 0; # reset counts

    return $class->SUPER::start($haenv, $id);

}

sub shutdown {
    my ($class, $haenv, $id) = @_;

    my (undef, undef, $cannot_stop) = &$decode_id($id);

    return if $cannot_stop;

    return $class->SUPER::shutdown($haenv, $id);
}

sub migrate {
    my ($class, $haenv, $id, $target, $online) = @_;

    my ($migrate_failure_count, $limit_to_node) = ($decode_id->($id))[1, 4];

    if ($limit_to_node == 0 || $haenv->nodename() eq "node$limit_to_node") {
        $tries->{migrate}->{$id} = 0 if !$tries->{migrate}->{$id};
        $tries->{migrate}->{$id}++;

        return if $migrate_failure_count >= $tries->{migrate}->{$id};
    }

    $tries->{migrate}->{$id} = 0; # reset counts

    return $class->SUPER::migrate($haenv, $id, $target, $online);

}
1;
