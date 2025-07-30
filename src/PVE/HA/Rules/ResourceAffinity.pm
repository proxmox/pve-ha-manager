package PVE::HA::Rules::ResourceAffinity;

use strict;
use warnings;

use PVE::HA::HashTools qw(set_intersect sets_are_disjoint);
use PVE::HA::Rules;

use base qw(PVE::HA::Rules);

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
        {
            type => 'resource-affinity',
            exclude_disabled_rules => 1,
        },
    );

    return $result;
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
            $args->{negative_rules}, $args->{nodes},
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

    while (my ($positiveid, $positive) = each %$positive_rules) {
        my $positive_resources = $positive->{resources};

        while (my ($negativeid, $negative) = each %$negative_rules) {
            my $common_resources = set_intersect($positive_resources, $negative->{resources});
            next if %$common_resources < 2;

            push @conflicts, [$positiveid, $negativeid];
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
            my ($positiveid, $negativeid) = @$conflict;

            push $errors->{$positiveid}->{resources}->@*,
                "rule shares two or more resources with '$negativeid'";
            push $errors->{$negativeid}->{resources}->@*,
                "rule shares two or more resources with '$positiveid'";
        }
    },
);

=head1 RESOURCE AFFINITY RULE CANONICALIZATION HELPERS

=cut

my $sort_by_lowest_resource_id = sub {
    my ($rules) = @_;

    my $lowest_rule_resource_id = {};
    for my $ruleid (keys %$rules) {
        my @rule_resources = sort keys $rules->{$ruleid}->{resources}->%*;
        $lowest_rule_resource_id->{$ruleid} = $rule_resources[0];
    }

    # sort rules such that rules with the lowest numbered resource come first
    my @sorted_ruleids = sort {
        $lowest_rule_resource_id->{$a} cmp $lowest_rule_resource_id->{$b}
    } sort keys %$rules;

    return @sorted_ruleids;
};

# returns a list of hashes, which contain disjoint resource affinity rules, i.e.,
# put resource affinity constraints on disjoint sets of resources
my $find_disjoint_resource_affinity_rules = sub {
    my ($rules) = @_;

    my @disjoint_rules = ();

    # order needed so that it is easier to check whether there is an overlap
    my @sorted_ruleids = $sort_by_lowest_resource_id->($rules);

    for my $ruleid (@sorted_ruleids) {
        my $rule = $rules->{$ruleid};

        my $found = 0;
        for my $entry (@disjoint_rules) {
            next if sets_are_disjoint($rule->{resources}, $entry->{resources});

            $found = 1;
            push @{ $entry->{ruleids} }, $ruleid;
            $entry->{resources}->{$_} = 1 for keys $rule->{resources}->%*;

            last;
        }
        if (!$found) {
            push @disjoint_rules,
                {
                    ruleids => [$ruleid],
                    resources => { $rule->{resources}->%* },
                };
        }
    }

    return @disjoint_rules;
};

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

    my @disjoint_positive_rules = $find_disjoint_resource_affinity_rules->($positive_rules);

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

sub plugin_canonicalize {
    my ($class, $rules) = @_;

    my $args = $class->get_plugin_check_arguments($rules);

    merge_connected_positive_resource_affinity_rules($rules, $args->{positive_rules});

    $args = $class->get_plugin_check_arguments($rules);

    # must come after merging connected positive rules, because of this helpers
    # assumptions about resource sets and inter-resource affinity consistency
    create_implicit_negative_resource_affinity_rules(
        $rules,
        $args->{positive_rules},
        $args->{negative_rules},
    );
}

1;
