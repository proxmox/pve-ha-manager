package PVE::HA::Rules::NodeAffinity;

use strict;
use warnings;

use Storable qw(dclone);

use PVE::Cluster;
use PVE::JSONSchema qw(get_standard_option);
use PVE::Tools;

use PVE::HA::HashTools qw(set_difference);
use PVE::HA::Rules;
use PVE::HA::Tools;

use base qw(Exporter);
use base qw(PVE::HA::Rules);

our @EXPORT_OK = qw(
    get_node_affinity
);

=head1 NAME

PVE::HA::Rules::NodeAffinity

=head1 DESCRIPTION

This package provides the capability to specify and apply rules, which put
affinity constraints between a set of HA resources and a set of nodes.

HA Node Affinity rules can be one of two types:

=over

=item C<'positive'>

Positive node affinity rules specify the nodes, which SHOULD/MUST be preferred
by the given HA resources.

=item C<'negative'>

Negative node affinity rules specify the nodes, which SHOULD/MUST be avoided by
the given HA resources.

=back

HA Node Affinity rules can be either C<'non-strict'> or C<'strict'>:

=over

=item C<'non-strict'>

Non-strict node affinity rules SHOULD be applied if possible.

That is, HA resources SHOULD prefer to be on the defined nodes, but may fall
back to other nodes, if none of the defined nodes are available.

=item C<'strict'>

Strict node affinity rules MUST be applied.

That is, HA resources MUST prefer to be on the defined nodes. In other words,
these HA resources are restricted to the defined nodes and may not run on any
other node.

=back

=cut

sub type {
    return 'node-affinity';
}

sub properties {
    return {
        nodes => get_standard_option(
            'pve-ha-node-list',
            {
                completion => \&PVE::Cluster::get_nodelist,
                optional => 0,
            },
        ),
        affinity => {
            description => "Describes whether the HA resources are supposed to"
                . " be placed on the given nodes ('positive'), or are supposed"
                . " to be placed on any but the given nodes ('negative').",
            type => 'string',
            enum => ['positive', 'negative'],
            default => 'positive',
            optional => 1,
        },
        strict => {
            description => "Describes whether the node affinity rule is strict or non-strict.",
            verbose_description => <<EODESC,
Describes whether the node affinity rule is strict or non-strict.

A non-strict node affinity rule makes resources prefer to be on the defined nodes.
If none of the defined nodes are available, the resource may run on any other node.

A strict node affinity rule makes resources be restricted to the defined nodes. If
none of the defined nodes are available, the resource will be stopped.
EODESC
            type => 'boolean',
            optional => 1,
            default => 0,
        },
    };
}

sub options {
    return {
        resources => { optional => 0 },
        nodes => { optional => 0 },
        affinity => { optional => 1 },
        strict => { optional => 1 },
        disable => { optional => 1 },
        comment => { optional => 1 },
    };
}

sub decode_plugin_value {
    my ($class, $type, $key, $value) = @_;

    if ($key eq 'nodes') {
        my $res = {};

        for my $node (PVE::Tools::split_list($value)) {
            if (my ($node, $priority) = PVE::HA::Tools::parse_node_priority($node, 1)) {
                $res->{$node} = {
                    priority => $priority,
                };
            }
        }

        return $res;
    }

    return $value;
}

sub encode_plugin_value {
    my ($class, $type, $key, $value) = @_;

    if ($key eq 'nodes') {
        my $res = [];

        for my $node (sort keys %$value) {
            my $priority = $value->{$node}->{priority};

            if ($priority) {
                push @$res, "$node:$priority";
            } else {
                push @$res, "$node";
            }
        }

        return join(',', @$res);
    }

    return $value;
}

sub get_plugin_check_arguments {
    my ($self, $rules) = @_;

    my $result = {
        node_affinity_rules => {},
    };

    PVE::HA::Rules::foreach_rule(
        $rules,
        sub {
            my ($rule, $ruleid) = @_;

            $result->{node_affinity_rules}->{$ruleid} = $rule;
        },
        type => 'node-affinity',
        exclude_disabled_rules => 1,
    );

    return $result;
}

sub plugin_compile {
    my ($class, $rules, $cluster_nodes) = @_;

    my $node_affinity = {};

    PVE::HA::Rules::foreach_rule(
        $rules,
        sub {
            my ($rule) = @_;

            my $effective_nodes = dclone($rule->{nodes});

            # add remaining nodes with low priority for non-strict node affinity
            if (!$rule->{strict}) {
                for my $node (@$cluster_nodes) {
                    next if defined($effective_nodes->{$node});

                    $effective_nodes->{$node} = { priority => -1 };
                }
            }

            for my $sid (keys $rule->{resources}->%*) {
                $node_affinity->{$sid} = {
                    nodes => $effective_nodes,
                };
            }
        },
        type => 'node-affinity',
        exclude_disabled_rules => 1,
    );

    return $node_affinity;
}

=head1 NODE AFFINITY RULE CHECKERS

=cut

=head3 check_single_resource_reference($node_affinity_rules)

Returns all in C<$node_affinity_rules> as a list of lists, each consisting of
the node affinity id and the resource id, where at least one resource is shared
between them.

If there are none, the returned list is empty.

=cut

sub check_single_resource_reference {
    my ($node_affinity_rules) = @_;

    my @conflicts = ();
    my $resource_ruleids = {};

    while (my ($ruleid, $rule) = each %$node_affinity_rules) {
        for my $sid (keys %{ $rule->{resources} }) {
            push @{ $resource_ruleids->{$sid} }, $ruleid;
        }
    }

    for my $sid (keys %$resource_ruleids) {
        my $ruleids = $resource_ruleids->{$sid};

        next if @$ruleids < 2;

        for my $ruleid (@$ruleids) {
            push @conflicts, [$ruleid, $sid];
        }
    }

    @conflicts = sort { $a->[0] cmp $b->[0] || $a->[1] cmp $b->[1] } @conflicts;
    return \@conflicts;
}

__PACKAGE__->register_check(
    sub {
        my ($args) = @_;

        return check_single_resource_reference($args->{node_affinity_rules});
    },
    sub {
        my ($conflicts, $errors) = @_;

        for my $conflict (@$conflicts) {
            my ($ruleid, $sid) = @$conflict;

            push $errors->{$ruleid}->{resources}->@*,
                "resource '$sid' is already used in another node affinity rule";
        }
    },
);

=head3 check_nonempty_negative_nodes_complement($node_affinity_rules, $cluster_nodes)

Returns a list of negative node affinity rule ids in C<$node_affinity_rules>,
where the complement of the negative node set is an empty node set according to
the currently configured cluster node list C<$cluster_nodes>, i.e., the
negative node set specifies all cluster nodes.

Even though this is only relevant for strict negative node affinity rules, this
check also includes non-strict negative node affinity rules as their effective
node set would be equivalent to setting no rule at all.

If there are none, the returned list is empty.

=cut

sub check_nonempty_negative_nodes_complement {
    my ($node_affinity_rules, $cluster_nodes) = @_;

    my @conflicts = ();

    my $total_node_count = @$cluster_nodes;

    while (my ($ruleid, $rule) = each %$node_affinity_rules) {
        next if $rule->{affinity} ne 'negative';

        push @conflicts, $ruleid if keys $rule->{nodes}->%* >= $total_node_count;
    }

    @conflicts = sort @conflicts;
    return \@conflicts;
}

__PACKAGE__->register_check(
    sub {
        my ($args) = @_;

        return check_nonempty_negative_nodes_complement(
            $args->{node_affinity_rules},
            $args->{'cluster-nodes'},
        );
    },
    sub {
        my ($ruleids, $errors) = @_;

        for my $ruleid (@$ruleids) {
            push $errors->{$ruleid}->{nodes}->@*,
                "negative node affinity rule must not specify all cluster nodes";
        }
    },
);

=head3 check_unprioritized_negative_nodes($node_affinity_rules)

Returns a list of negative node affinity rule ids in C<$node_affinity_rules>,
where at least one node has a priority set. A node priority does not have any
meaningful semantic value for negative node affinity rules.

If there are none, the returned list is empty.

=cut

sub check_unprioritized_negative_nodes {
    my ($node_affinity_rules) = @_;

    my @conflicts = ();

    while (my ($ruleid, $rule) = each %$node_affinity_rules) {
        next if $rule->{affinity} ne 'negative';

        for my $node (keys $rule->{nodes}->%*) {
            if ($rule->{nodes}->{$node}->{priority}) {
                push @conflicts, $ruleid;
                last; # one non-zero priority is enough to invalidate rule
            }
        }
    }

    @conflicts = sort @conflicts;
    return \@conflicts;
}

__PACKAGE__->register_check(
    sub {
        my ($args) = @_;

        return check_unprioritized_negative_nodes($args->{node_affinity_rules});
    },
    sub {
        my ($ruleids, $errors) = @_;

        for my $ruleid (@$ruleids) {
            push $errors->{$ruleid}->{nodes}->@*,
                "negative node affinity rule must not specify node priorities";
        }
    },
);

=head1 NODE AFFINITY RULE TRANSFORMATION HELPERS

=cut

=head3 invert_negative_node_affinity_rules($rules, $node_affinity_rules, $cluster_nodes)

Modifies C<$rules> such that all negative node affinity rules, defined in
C<$node_affinity_rules>, are transformed to positive node affinity rules, where
the nodes set is the complement of the negative node affinity rules' nodes set.

C<$cluster_nodes> is a list of the configured cluster nodes, which is used as
the universal set to build the complement node set.

=cut

sub invert_negative_node_affinity_rules {
    my ($rules, $node_affinity_rules, $cluster_nodes) = @_;

    # set_difference(...) requires a hash set instead of a list
    my $cluster_nodes_hash = { map { $_ => 1 } @$cluster_nodes };

    while (my ($node_affinity_id, $node_affinity_rule) = each %$node_affinity_rules) {
        next if $node_affinity_rule->{affinity} ne 'negative';

        my $negative_nodes = { map { $_ => 1 } keys $node_affinity_rule->{nodes}->%* };
        my $positive_nodes = set_difference($cluster_nodes_hash, $negative_nodes);
        $positive_nodes->{$_} = { priority => 0 } for keys %$positive_nodes;

        $rules->{ids}->{$node_affinity_id}->{affinity} = 'positive';
        $rules->{ids}->{$node_affinity_id}->{nodes} = $positive_nodes;
    }
}

__PACKAGE__->register_transform(sub {
    my ($rules, $args) = @_;

    invert_negative_node_affinity_rules(
        $rules,
        $args->{node_affinity_rules},
        $args->{'cluster-nodes'},
    );
});

=head1 NODE AFFINITY RULE HELPERS

=cut

=head3 get_node_affinity($node_affinity, $sid, $online_nodes)

Returns a list of two hashes representing the node affinity of C<$sid>
according to the node affinity C<$node_affinity> and the available nodes in
the C<$online_nodes> hash.

The first hash is a hash set of available nodes, i.e. nodes where the
resource C<$sid> is allowed to be assigned to, and the second hash is a hash set
of preferred nodes, i.e. nodes where the resource C<$sid> should be assigned to.

If there are no available nodes at all, returns C<undef>.

=cut

sub get_node_affinity {
    my ($node_affinity, $sid, $online_nodes) = @_;

    return ($online_nodes, $online_nodes) if !defined($node_affinity->{$sid});

    my $allowed_nodes = {};
    my $prioritized_nodes = {};

    while (my ($node, $props) = each $node_affinity->{$sid}->{nodes}->%*) {
        next if !defined($online_nodes->{$node}); # node is offline

        my $node_priority = $props->{priority} // 0;

        $allowed_nodes->{$node} = 1;
        $prioritized_nodes->{$node_priority}->{$node} = 1;
    }

    my $preferred_nodes = {};
    my $highest_priority = (sort { $b <=> $a } keys %$prioritized_nodes)[0];
    $preferred_nodes = $prioritized_nodes->{$highest_priority} if defined($highest_priority);

    return ($allowed_nodes, $preferred_nodes);
}

1;
