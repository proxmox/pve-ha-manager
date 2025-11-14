package PVE::HA::Rules::Helpers;

use v5.36;

use PVE::HA::HashTools qw(sets_are_disjoint);

my sub sort_by_lowest_resource_id($rules) {
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
}

=head3 find_disjoint_rules_resource_sets($rules)

Finds the rule subsets in C<$rules>, which reference a disjoint set of resources
with respect to the other rules. The disjoint rule resource sets are returned as
a list of hashes, where each item contains the disjoint resource set and the
ruleids from C<$rules> that were used to create the resource subset.

For example, if one rule references the resources C<'vm:101'> and C<'vm:102'>,
and another rule references the resources C<'vm:102'> and C<'vm:103'>, these
will result in one disjoint rule resource set, thus the return value:

    (
        {
            ruleids = [ 'rule1', 'rule2' ],
            resources => {
                'vm:101' => 1,
                'vm:102' => 1,
                'vm:103' => 1
            }
        }
    )

=cut

sub find_disjoint_rules_resource_sets($rules) {
    my @disjoint_rules = ();

    # order needed so that it is easier to check whether there is an overlap
    my @sorted_ruleids = sort_by_lowest_resource_id($rules);

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
}

1;
