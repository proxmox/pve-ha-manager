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

1;
