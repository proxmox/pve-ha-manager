package PVE::HA::Helpers;

use v5.36;

use PVE::HA::Rules::NodeAffinity qw(get_node_affinity);
use PVE::HA::Rules::ResourceAffinity qw(get_affinitive_resources);

=head3 get_resource_motion_info

Gathers which other HA resources in C<$ss> put a node placement dependency or
node placement restriction on C<$sid> according to the compiled rules in
C<$compiled_rules> and the online nodes in C<$online_nodes>.

Returns a list of two elements, where the first element is a list of HA resource
ids which are dependent on the node placement of C<$sid>, and the second element
is a hash of nodes blocked for C<$sid>, where each entry value is a list of the
causes that make the node unavailable to C<$sid>.

=cut

sub get_resource_motion_info($ss, $sid, $cd, $online_nodes, $compiled_rules) {
    my $dependent_resources = [];
    my $blocking_resources_by_node = {};

    my ($node_affinity, $resource_affinity) =
        $compiled_rules->@{qw(node-affinity resource-affinity)};
    my ($allowed_nodes, $pri_nodes) = get_node_affinity($node_affinity, $sid, $online_nodes);
    my ($together, $separate) = get_affinitive_resources($resource_affinity, $sid);

    for my $csid (sort keys %$together) {
        next if !defined($ss->{$csid});
        next if $ss->{$csid}->{state} eq 'ignored';

        push @$dependent_resources, $csid;
    }

    for my $node (keys %$online_nodes) {
        if (!$allowed_nodes->{$node} || ($cd->{failback} && !$pri_nodes->{$node})) {
            push $blocking_resources_by_node->{$node}->@*,
                {
                    sid => $sid,
                    cause => 'node-affinity',
                };
        }

        for my $csid (sort keys %$separate) {
            next if !defined($ss->{$csid});
            next if $ss->{$csid}->{state} eq 'ignored';
            next if $ss->{$csid}->{node} && $ss->{$csid}->{node} ne $node;
            next if $ss->{$csid}->{target} && $ss->{$csid}->{target} ne $node;

            push $blocking_resources_by_node->{$node}->@*,
                {
                    sid => $csid,
                    cause => 'resource-affinity',
                };
        }
    }

    return ($dependent_resources, $blocking_resources_by_node);
}

=head3 check_enable_maintenance

Checks whether a request to enable the maintenance mode for C<$node>, which is
in the HA node state C<$node_state>, can be recorded in the manager status
C<$ms>.

Returns an empty list if the request can be recorded. Otherwise, returns a list
of two elements, where the first element is the log level and the second element
is the reason why the request is ignored.

=cut

sub check_enable_maintenance($node, $node_state, $ms) {
    return ('warning', "HA stack is disarmed") if $ms->{disarm};

    if ($node_state eq 'maintenance') {
        # only ignore the request if one is already recorded, as the node can be in maintenance
        # without one: while it shuts down with the 'migrate' shutdown policy, or if a disable
        # request queued right before this one cleared it, as the node state only catches up in
        # the next round. recording it keeps the node in maintenance either way
        my $request = ($ms->{node_request} // {})->{$node} // {};
        return ('info', "node is already in maintenance state") if $request->{maintenance};
    } elsif ($node_state ne 'online') {
        return ('err', "node is not online");
    }

    return;
}

1;
