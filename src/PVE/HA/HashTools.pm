package PVE::HA::HashTools;

use strict;
use warnings;

use base qw(Exporter);

our @EXPORT_OK = qw(
    set_intersect
    set_union
    sets_are_disjoint
);

=head1 NAME

PVE::HA::HashTools - Helpers for Hashes

=head1 DESCRIPTION

This packages provides helpers for common operations on hashes.

Even though these operations' implementation are often one-liners, they are
meant to improve code readability by stating a operation name instead of the
more verbose implementation.

=cut

=head1 FUNCTIONS

=cut

=head3 set_intersect($hash1, $hash2)

Returns a hash set of the intersection of the hash sets C<$hash1> and
C<$hash2>, i.e. the elements that are both in C<$hash1> and C<$hash2>.

The hashes C<$hash1> and C<$hash2> are expected to be hash sets, i.e.
key-value pairs are always set to C<1> or another truthy value.

=cut

sub set_intersect {
    my ($hash1, $hash2) = @_;

    my $result = { map { $hash1->{$_} && $hash2->{$_} ? ($_ => 1) : () } keys %$hash1 };

    return $result;
}

=head3 set_union($hash1, $hash2)

Returns a hash set of the union of the hash sets C<$hash1> and C<$hash2>, i.e.
the elements that are in either C<$hash1> or C<$hash2>.

The hashes C<$hash1> and C<$hash2> are expected to be hash sets, i.e.
key-value pairs are always set to C<1> or another truthy value.

=cut

sub set_union {
    my ($hash1, $hash2) = @_;

    my $result = { map { $_ => 1 } keys %$hash1, keys %$hash2 };

    return $result;
}

=head3 sets_are_disjoint($hash1, $hash2)

Checks whether the two given hash sets C<$hash1> and C<$hash2> are disjoint,
i.e. have no common element in both of them.

The hashes C<$hash1> and C<$hash2> are expected to be hash sets, i.e.
key-value pairs are always set to C<1> or another truthy value.

Returns C<1> if they are disjoint, C<0> otherwise.

=cut

sub sets_are_disjoint {
    my ($hash1, $hash2) = @_;

    for my $key (keys %$hash1) {
        return 0 if $hash2->{$key};
    }

    return 1;
}

1;
