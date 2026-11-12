package PVE::HA::Sim::Resources::VirtVM;

use strict;
use warnings;

use base qw(PVE::HA::Sim::Resources);

sub type {
    return 'vm';
}

sub exists {
    my ($class, $id, $noerr) = @_;

    # in the virtual cluster every virtual VM (of this type) exists
    return 1;
}

sub migrate_json_properties {
    my ($class) = @_;
    my $props = { $class->SUPER::migrate_json_properties()->%* };
    $props->{'vm-bar'} = {
        type => 'string',
        optional => 1,
        description => 'A migration property specific to VMs.',
    };
    return $props;
}

1;
