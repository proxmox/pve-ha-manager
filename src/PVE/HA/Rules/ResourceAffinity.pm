package PVE::HA::Rules::ResourceAffinity;

use strict;
use warnings;

use PVE::HA::HashTools qw(set_intersect);
use PVE::HA::Rules;
use PVE::HA::Rules::Helpers;
use PVE::HA::Usage;

use base qw(Exporter);
use base qw(PVE::HA::Rules);

our @EXPORT_OK = qw(
    get_affinitive_resources
    get_resource_affinity
    apply_positive_resource_affinity
    apply_negative_resource_affinity
);

=head1 NAME

PVE::HA::Rules::ResourceAffinity - Resource Affinity Plugin for HA Rules

=head1 DESCRIPTION

This package provides the capability to specify and apply rules, which put
affinity constraints between the HA resources.

HA resource affinity rules have one of the two types:

=over

=item C<'positive'>

Positive resource affinity rules specify that HA resources need to be be kept
together.

=item C<'negative'>

Negative resource affinity rules (or resource anti-affinity rules) specify that
HA resources need to be kept separate.

=back

HA resource affinity rules MUST be applied. That is, if a HA resource cannot
comply with the resource affinity rule, it is put in recovery or other
error-like states, if there is no other way to recover them.

=cut

sub type {
    return 'resource-affinity';
}

sub properties {
    return {
        affinity => {
            description => "Describes whether the HA resources are supposed to"
                . " be kept on the same node ('positive'), or are supposed to"
                . " be kept on separate nodes ('negative').",
            type => 'string',
            enum => ['positive', 'negative'],
            optional => 0,
        },
    };
}

sub options {
    return {
        resources => { optional => 0 },
        affinity => { optional => 0 },
        disable => { optional => 1 },
        comment => { optional => 1 },
    };
}

sub get_plugin_check_arguments {
    my ($self, $rules) = @_;

    my $result = {
        resource_affinity_rules => {},
        positive_rules => {},
        negative_rules => {},
    };

    PVE::HA::Rules::foreach_rule(
        $rules,
        sub {
            my ($rule, $ruleid) = @_;

            $result->{resource_affinity_rules}->{$ruleid} = $rule;

            $result->{positive_rules}->{$ruleid} = $rule if $rule->{affinity} eq 'positive';
            $result->{negative_rules}->{$ruleid} = $rule if $rule->{affinity} eq 'negative';
        },
        type => 'resource-affinity',
        exclude_disabled_rules => 1,
    );

    return $result;
}

sub plugin_compile {
    my ($class, $rules, $nodes) = @_;

    my $positive = {};
    my $negative = {};

    PVE::HA::Rules::foreach_rule(
        $rules,
        sub {
            my ($rule, $ruleid) = @_;

            my $affinity_set = $rule->{affinity} eq 'positive' ? $positive : $negative;

            for my $sid (keys $rule->{resources}->%*) {
                for my $csid (keys $rule->{resources}->%*) {
                    $affinity_set->{$sid}->{$csid} = 1 if $csid ne $sid;
                }
            }
        },
        type => 'resource-affinity',
        exclude_disabled_rules => 1,
    );

    return {
        positive => $positive,
        negative => $negative,
    };
}

=head1 RESOURCE AFFINITY RULE CHECKERS

=cut

=head3 check_resource_affinity_resources_count($resource_affinity_rules)

Returns a list of resource affinity rule ids, defined in
C<$resource_affinity_rules>, which do not have enough resources defined to be
effective resource affinity rules.

If there are none, the returned list is empty.

=cut

sub check_resource_affinity_resources_count {
    my ($resource_affinity_rules) = @_;

    my @conflicts = ();

    while (my ($ruleid, $rule) = each %$resource_affinity_rules) {
        push @conflicts, $ruleid if keys %{ $rule->{resources} } < 2;
    }

    @conflicts = sort @conflicts;
    return \@conflicts;
}

__PACKAGE__->register_check(
    sub {
        my ($args) = @_;

        return check_resource_affinity_resources_count($args->{resource_affinity_rules});
    },
    sub {
        my ($ruleids, $errors) = @_;

        for my $ruleid (@$ruleids) {
            push $errors->{$ruleid}->{resources}->@*,
                "rule is ineffective as there are less than two resources";
        }
    },
);

=head3 check_negative_resource_affinity_resources_count($negative_rules, $nodes)

Returns a list of negative resource affinity rule ids, defined in
C<$negative_rules>, which do have more resources defined than available according
to the node list C<$nodes>, i.e., there are not enough nodes to separate the
resources on, even if all nodes are available.

If there are none, the returned list ist empty.

=cut

sub check_negative_resource_affinity_resources_count {
    my ($negative_rules, $nodes) = @_;

    my @conflicts = ();

    my $total_node_count = @$nodes;

    while (my ($negativeid, $negative_rule) = each %$negative_rules) {
        push @conflicts, $negativeid if keys $negative_rule->{resources}->%* > $total_node_count;
    }

    @conflicts = sort @conflicts;
    return \@conflicts;
}

__PACKAGE__->register_check(
    sub {
        my ($args) = @_;

        return check_negative_resource_affinity_resources_count(
            $args->{negative_rules},
            $args->{nodes},
        );
    },
    sub {
        my ($ruleids, $errors) = @_;

        for my $ruleid (@$ruleids) {
            push $errors->{$ruleid}->{resources}->@*,
                "rule defines more resources than available nodes";
        }
    },
);

=head3 check_inter_resource_affinity_rules_consistency($positive_rules, $negative_rules)

Returns a list of lists consisting of a positive resource affinity rule, defined
in C<$positive_rules> and a negative resource affinity rule id, defined in
C<$negative_rules>, which share at least the same two resources among them.

This is an impossible constraint as the same resources cannot be kept together on
the same node and kept separate on different nodes at the same time.

If there are none, the returned list is empty.

=cut

sub check_inter_resource_affinity_rules_consistency {
    my ($positive_rules, $negative_rules) = @_;

    my @conflicts = ();

    my @disjoint_positive_rules =
        PVE::HA::Rules::Helpers::find_disjoint_rules_resource_sets($positive_rules);

    for my $entry (@disjoint_positive_rules) {
        my $positive_resources = $entry->{resources};

        while (my ($negativeid, $negative) = each %$negative_rules) {
            my $common_resources = set_intersect($positive_resources, $negative->{resources});
            next if %$common_resources < 2;

            push @conflicts, ['negative', $negativeid];
            push @conflicts, ['positive', $_] for $entry->{ruleids}->@*;
        }
    }

    @conflicts = sort { $a->[0] cmp $b->[0] || $a->[1] cmp $b->[1] } @conflicts;
    return \@conflicts;
}

__PACKAGE__->register_check(
    sub {
        my ($args) = @_;

        return check_inter_resource_affinity_rules_consistency(
            $args->{positive_rules},
            $args->{negative_rules},
        );
    },
    sub {
        my ($conflicts, $errors) = @_;

        for my $conflict (@$conflicts) {
            my ($type, $ruleid) = @$conflict;

            if ($type eq 'positive') {
                push $errors->{$ruleid}->{resources}->@*,
                    "rule shares two or more resources with a negative resource affinity rule";
            } elsif ($type eq 'negative') {
                push $errors->{$ruleid}->{resources}->@*,
                    "rule shares two or more resources with a positive resource affinity rule";
            }
        }
    },
);

=head1 RESOURCE AFFINITY RULE TRANSFORMATION HELPERS

=cut

=head3 merge_connected_positive_resource_affinity_rules($rules, $positive_rules)

Modifies C<$rules> to contain only disjoint positive resource affinity rules
among the ones defined in C<$positive_rules>, i.e., all positive resource
affinity rules put positive resource affinity constraints on disjoint sets of
resources.

If two or more positive resource affinity rules have overlapping resource sets,
then these will be removed from C<$rules> and a new positive resource affinity
rule, where the rule id is the dashed concatenation of the rule ids
(e.g. C<'$rule1-$rule2'>), is inserted in C<$rules>.

This makes it cheaper to find the resources, which are in positive affinity with
a resource, in C<$rules> at a later point in time.

=cut

sub merge_connected_positive_resource_affinity_rules {
    my ($rules, $positive_rules) = @_;

    my @disjoint_positive_rules =
        PVE::HA::Rules::Helpers::find_disjoint_rules_resource_sets($positive_rules);

    for my $entry (@disjoint_positive_rules) {
        next if @{ $entry->{ruleids} } < 2;

        my $new_ruleid = '_merged-' . join('-', @{ $entry->{ruleids} });
        my $first_ruleid = @{ $entry->{ruleids} }[0];

        $rules->{ids}->{$new_ruleid} = {
            type => 'resource-affinity',
            affinity => 'positive',
            resources => $entry->{resources},
        };
        $rules->{order}->{$new_ruleid} = $rules->{order}->{$first_ruleid};

        for my $ruleid (@{ $entry->{ruleids} }) {
            delete $rules->{ids}->{$ruleid};
            delete $rules->{order}->{$ruleid};
        }
    }
}

__PACKAGE__->register_transform(sub {
    my ($rules, $args) = @_;

    merge_connected_positive_resource_affinity_rules($rules, $args->{positive_rules});
});

# retrieve the existing negative resource affinity relationships for any of the
# $resources in the $negative_rules; returns a hash map, where the keys are the
# resources to be separated from and the values are subsets of the $resources
my $get_negative_resource_affinity_for_resources = sub {
    my ($negative_rules, $resources) = @_;

    my $separated_from = {};

    while (my ($negativeid, $negative_rule) = each %$negative_rules) {
        # assuming that there is at most one $sid in a $negative_rule, because
        # these are removed by the inter-resource-affinity checker before
        for my $sid (keys %$resources) {
            next if !$negative_rule->{resources}->{$sid};

            for my $csid (keys $negative_rule->{resources}->%*) {
                $separated_from->{$csid}->{$sid} = 1 if $csid ne $sid;
            }
        }
    }

    return $separated_from;
};

=head3 create_implicit_negative_resource_affinity_rules($rules, $positive_rules, $negative_rules)

Modifies C<$rules> to contain the negative resource affinity rules, which are
implied by the negative resource affinity relationships, defined in
C<$negative_rules>, the resources in a positive resource affinity rule are in,
defined in C<$positive_rules>.

If one or more resources in a positive resource affinity rule is also in a
negative resource affinity rule, then for each of the resources in the positive
resource affinity rule not in the negative resource affinity will also be put in
that relationship by its own negative resource affinity rule in C<$rules>.

This helper assumes that 1) the resource sets in positive resource affinity rules
are disjoint from each other (i.e. already merged connected ones before), and
2) there cannot be two or more same resources in a positive and a negative
resource affinity rule (i.e. these are removed beforehand).

For example, if two resources A and B must be kept together, but resource A must
be kept apart from resource C and resource B must be kept apart from resource D,
then the inferred rules will be a negative resource affinity between A and D
and a negative resource affinity between B and C.

This makes it cheaper to infer these implicit constraints later instead of
propagating that information in each scheduler invocation.

=cut

sub create_implicit_negative_resource_affinity_rules {
    my ($rules, $positive_rules, $negative_rules) = @_;

    my @conflicts = ();

    while (my ($positiveid, $positive_rule) = each %$positive_rules) {
        my $positive_resources = $positive_rule->{resources};

        # assuming that every positive rule's resource set is disjoint from the others
        my $separated_from =
            $get_negative_resource_affinity_for_resources->($negative_rules, $positive_resources);

        for my $csid (keys %$separated_from) {
            for my $sid (keys %$positive_resources) {
                next if $separated_from->{$csid}->{$sid};

                my $new_ruleid = "_implicit-negative-$positiveid-$sid-$csid";
                my $new_negative_resources = {
                    $sid => 1,
                    $csid => 1,
                };

                $rules->{ids}->{$new_ruleid} = {
                    type => 'resource-affinity',
                    affinity => 'negative',
                    resources => $new_negative_resources,
                };
                $rules->{order}->{$new_ruleid} = PVE::HA::Rules::get_next_ordinal($rules);
            }
        }
    }
}

# must come after merging connected positive rules, because of this helpers
# assumptions about resource sets and inter-resource affinity consistency
__PACKAGE__->register_transform(sub {
    my ($rules, $args) = @_;

    create_implicit_negative_resource_affinity_rules(
        $rules,
        $args->{positive_rules},
        $args->{negative_rules},
    );
});

=head1 RESOURCE AFFINITY RULE HELPERS

=cut

=head3 get_affinitive_resources($resource_affinity, $sid)

Returns a list of two hash sets, where the first hash set contains the
resources, which C<$sid> is positively affinitive to, and the second hash
contains the resources, which C<$sid> is negatively affinitive to, acording to
the resource's resource affinity in C<$resource_affinity>.

Note that a resource C<$sid> becomes part of any negative affinity relation
of its positively affinitive resources.

For example, if a resource is negatively affinitive to C<'vm:101'> and positively
affinitive to C<'ct:200'> and C<'ct:201'>, the returned value will be:

    {
        together => {
            'vm:101' => 1
        },
        separate => {
            'ct:200' => 1,
            'ct:201' => 1
        }
    }

=cut

sub get_affinitive_resources : prototype($$) {
    my ($resource_affinity, $sid) = @_;

    my $together = $resource_affinity->{positive}->{$sid} // {};
    my $separate = $resource_affinity->{negative}->{$sid} // {};

    return ($together, $separate);
}

=head3 get_resource_affinity($resource_affinity, $sid, $ss, $online_nodes)

Returns a list of two hashes, where the first describes the positive resource
affinity and the second hash describes the negative resource affinity for
resource C<$sid> according to the resource's resource affinity rules in
C<$resource_affinity>, the service status C<$ss> and the C<$online_nodes> hash.

For the positive resource affinity of a resource C<$sid>, each element in the
hash represents an online node, where other resources, which C<$sid> is in
positive affinity with, are already running, and how many of them. That is,
each element represents a node, where the resource must be.

For the negative resource affinity of a resource C<$sid>, each element in the
hash represents an online node, where other resources, which C<$sid> is in
negative affinity with, are alreaddy running. That is, each element represents
a node, where the resource must not be.

For example, if there are already three resources running, which the resource
C<$sid> is in a positive affinity with, and two running resources, which the
resource C<$sid> is in a negative affinity with, the returned value will be:

    {
        together => {
            node2 => 3
        },
        separate => {
            node1 => 1,
            node3 => 1
        }
    }

=cut

sub get_resource_affinity : prototype($$$$) {
    my ($resource_affinity, $sid, $ss, $online_nodes) = @_;

    my $together = {};
    my $separate = {};

    my ($positive, $negative) = get_affinitive_resources($resource_affinity, $sid);

    my $get_used_service_nodes = sub {
        my ($sid) = @_;
        return (undef, undef) if !defined($ss->{$sid});
        my ($state, $node, $target) = $ss->{$sid}->@{qw(state node target)};
        return PVE::HA::Usage::get_used_service_nodes($online_nodes, $state, $node, $target);
    };

    for my $csid (keys $positive->%*) {
        my ($current_node, $target_node) = $get_used_service_nodes->($csid);

        $together->{$current_node}++ if defined($current_node);
        $together->{$target_node}++ if defined($target_node);
    }

    for my $csid (keys $negative->%*) {
        my ($current_node, $target_node) = $get_used_service_nodes->($csid);

        $separate->{$current_node} = 1 if defined($current_node);
        $separate->{$target_node} = 1 if defined($target_node);
    }

    return ($together, $separate);
}

=head3 is_allowed_on_node($together, $separate, $node)

Checks whether the resource affinity hashes C<$together> or C<$separate> state
whether for C<$together> the C<$node> must be selected, or for C<$separate> the
node C<$node> must be avoided.

=cut

sub is_allowed_on_node : prototype($$$) {
    my ($together, $separate, $node) = @_;

    return $together->{$node} || !$separate->{$node};
}

=head3 apply_positive_resource_affinity($together, $allowed_nodes)

Applies the positive resource affinity C<$together> on the allowed node hash set
C<$allowed_nodes> by modifying it directly.

Positive resource affinity means keeping resources together on a single node and
therefore minimizing the separation of resources.

The allowed node hash set C<$allowed_nodes> is expected to contain all nodes,
which are available to the resource this helper is called for, i.e. each node
is currently online, available according to other location constraints, and the
resource has not failed running there yet.

=cut

sub apply_positive_resource_affinity : prototype($$) {
    my ($together, $allowed_nodes) = @_;

    for my $node (keys %$together) {
        delete $together->{$node} if !$allowed_nodes->{$node};
    }

    my @possible_nodes = sort keys $together->%*
        or return; # nothing to do if there is no positive resource affinity

    # select the most populated node from a positive resource affinity
    @possible_nodes = sort { $together->{$b} <=> $together->{$a} } @possible_nodes;
    my $majority_node = $possible_nodes[0];

    for my $node (keys %$allowed_nodes) {
        delete $allowed_nodes->{$node} if $node ne $majority_node;
    }
}

=head3 apply_negative_resource_affinity($separate, $allowed_nodes)

Applies the negative resource affinity C<$separate> on the allowed node hash set
C<$allowed_nodes> by modifying it directly.

Negative resource affinity means keeping resources separate on multiple nodes
and therefore maximizing the separation of resources.

The allowed node hash set C<$allowed_nodes> is expected to contain all nodes,
which are available to the resource this helper is called for, i.e. each node
is currently online, available according to other location constraints, and the
resource has not failed running there yet.

=cut

sub apply_negative_resource_affinity : prototype($$) {
    my ($separate, $allowed_nodes) = @_;

    my $forbidden_nodes = { $separate->%* };

    for my $node (keys %$forbidden_nodes) {
        delete $allowed_nodes->{$node};
    }
}

1;
