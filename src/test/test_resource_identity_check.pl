#!/usr/bin/perl

# Tests the checks of the VM and container resources whether a migration target keeps the guest
# identity, with the guest state stubbed, as qemu-server and pve-container are no build dependency.

use strict;
use warnings;

use lib '..';

use Test::MockModule;
use Test::More;

# keeps the resource plugins from loading qemu-server and pve-container
BEGIN { $ENV{PVE_GENERATING_DOCS} = 1; }

my $guest = {};

{

    package PVE::GuestIdentity;

    sub assert_node_supports_identity {
        my ($node) = @_;
        die "node '$node' drops the guest identity\n" if $node eq 'old-node';
    }

    package PVE::QemuConfig;
    sub has_stored_identity { return $guest->{identity} }
    sub load_config { return $guest->{conf} }

    package PVE::LXC::Config;
    sub load_config { return $guest->{conf} }

    # the rule of pve-container 6.1.27, to check what the resource passes to it
    sub migration_needs_identity_support {
        my ($class, $vmid, $conf, $running) = @_;
        return 1 if $running && scalar(keys(($conf->{pending} // {})->%*));
        return $guest->{identity};
    }
}

use PVE::HA::Resources::PVECT;
use PVE::HA::Resources::PVEVM;

my $mocks = [];
for my $class ('PVE::HA::Resources::PVEVM', 'PVE::HA::Resources::PVECT') {
    my $module = Test::MockModule->new($class);
    $module->mock(check_running => sub { return $guest->{running} });
    push $mocks->@*, $module;
}

my sub check {
    my ($class, $target, $online) = @_;
    eval { $class->assert_migration_target(undef, 100, $target, $online) };
    return 'accepted' if !$@;
    return 'refused' if $@ =~ /drops the guest identity/;
    die $@;
}

my $pending = { pending => { memory => 1024 } };
my $fleecing =
    { 'special-sections' => { fleecing => { 'fleecing-images' => 'local:vm-100-fleece-0' } } };

# guest state, then the expected result for the VM, online and not, and for the container
my $cases = [
    ['identity', { identity => 1, conf => {} }, 'refused', 'refused', 'refused'],
    ['no identity', { conf => {} }, 'accepted', 'accepted', 'accepted'],
    ['stopped, pending changes', { conf => $pending }, 'accepted', 'accepted', 'accepted'],
    [
        'running, pending changes',
        { running => 1, conf => $pending },
        'accepted',
        'refused',
        'refused',
    ],
    [
        'running, left-over fleecing images',
        { running => 1, conf => $fleecing },
        'accepted',
        'refused',
        'accepted',
    ],
];

for my $case ($cases->@*) {
    my ($what, $state, $vm_online, $vm_offline, $ct) = $case->@*;
    $guest = $state;
    is(check('PVE::HA::Resources::PVEVM', 'old-node', 1), $vm_online, "VM, migrate, $what");
    is(check('PVE::HA::Resources::PVEVM', 'old-node', 0), $vm_offline, "VM, relocate, $what");
    is(check('PVE::HA::Resources::PVECT', 'old-node', 1), $ct, "container, migrate, $what");
    is(check('PVE::HA::Resources::PVECT', 'old-node', 0), $ct, "container, relocate, $what");
    is(check('PVE::HA::Resources::PVEVM', 'new-node', 0), 'accepted', "VM, new node, $what");
}

done_testing();
