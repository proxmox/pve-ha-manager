package PVE::HA::Rules;

use strict;
use warnings;

use PVE::JSONSchema qw(get_standard_option);
use PVE::Tools;

use PVE::HA::HashTools qw(set_intersect set_union sets_are_disjoint);
use PVE::HA::Tools;
use PVE::HA::Rules::Helpers;

use base qw(PVE::SectionConfig);

=head1 NAME

PVE::HA::Rules - Base Plugin for HA Rules

=head1 SYNOPSIS

    use base qw(PVE::HA::Rules);

=head1 DESCRIPTION

This package provides the capability to have different types of rules in the
same config file, which put constraints or other rules on the HA Manager's
behavior for how it handles HA resources handling.

Since rules can interfere with each other, i.e., rules can make other rules
invalid or infeasible, this package also provides the capability to check for
the feasibility between rules of the same type and and between rules of
different types, and prune the rule set in such a way, that it becomes feasible
again, while minimizing the amount of rules that need to be pruned.

More so, the rules given by the config file might not be in the best format to
be used internally or does not contain the implicitly stated rules, which are
induced by the relationship between different rules. Therefore, this package
also provides the capability to C<L<register transforms|/REGISTERING TRANSFORMS>>
to implement these internal rule transformations.

This packages inherits its config-related methods from C<L<PVE::SectionConfig>>
and therefore rule plugins need to implement methods from there as well.

=head1 USAGE

Each I<rule plugin> is required to implement the methods C<L<type()>>,
C<L<properties()>>, and C<L<options>> from the C<L<PVE::SectionConfig>> to
extend the properties of this I<base plugin> with plugin-specific properties.

Each I<rule plugin> is required to implement the method
C<L<< plugin_compile()|/$class->plugin_compile(...) >>> to distill a compiled
representation of the more verbose C<$rules> from the config file, which is
returned by C<L<< compile()|/$class->compile(...) >>> to be more appropriate
and efficient for the scheduler and other users.

=head2 REGISTERING CHECKS

In order to C<L<< register checks|/$class->register_check(...) >>> for a rule
plugin, the plugin can override the
C<L<< get_plugin_check_arguments()|/$class->get_plugin_check_arguments(...) >>>
method, which allows the plugin's checkers to pass plugin-specific data, usually
subsets of specific rules, which are relevant to the checks.

The following example shows a plugin's implementation of its
C<L<< get_plugin_check_arguments()|/$class->get_plugin_check_arguments(...) >>>
and a trivial check, which will render all rules defining a comment erroneous,
and blames these errors on the I<comment> property:

    sub get_plugin_check_arguments {
        my ($class, $rules) = @_;

        my @ruleids = sort {
            $rules->{order}->{$a} <=> $rules->{order}->{$b}
        } keys %{$rules->{ids}};

        my $result = {
            custom_rules => {},
        };

        for my $ruleid (@ruleids) {
            my $rule = $rules->{ids}->{$ruleid};

            $result->{custom_rules}->{$ruleid} = $rule if defined($rule->{comment});
        }

        return $result;
    }

    __PACKAGE__->register_check(
        sub {
            my ($args) = @_;

            return [ sort keys $args->{custom_rules}->%* ];
        },
        sub {
            my ($ruleids, $errors) = @_;

            for my $ruleid (@$ruleids) {
                push @{$errors->{$ruleid}->{comment}},
                    "rule is ineffective, because I said so.";
            }
        }
    );

=head2 REGISTERING TRANSFORMS

Rule transforms are used for transforming the rule set in such a way that
the rules provided by the rules config are easier to work with (for example,
transforming rules into equivalent forms) or make the rule set more complete
(e.g. explicitly create semantically implicit rules).

C<L<< Registering transforms|/$class->register_transform(...) >>> is the same
as for registering checks. Following up on the example from that section, the
following example shows a possible rule plugin's transform, which removes the
I<comment> property from each rule:

    __PACKAGE__->register_transformer(
        sub {
            my ($rules, $args) = @_;

            for my $ruleid (keys $args->{custom_rules}->%*) {
                delete $rules->{ids}->{$ruleid}->{comment};
            }
        }
    );

=head1 METHODS

=cut

my $defaultData = {
    propertyList => {
        type => {
            description => "HA rule type.",
        },
        rule => get_standard_option(
            'pve-ha-rule-id',
            {
                completion => \&PVE::HA::Tools::complete_rule,
                optional => 0,
            },
        ),
        disable => {
            description => 'Whether the HA rule is disabled.',
            type => 'boolean',
            optional => 1,
        },
        resources => get_standard_option(
            'pve-ha-resource-id-list',
            {
                completion => \&PVE::HA::Tools::complete_sid,
                optional => 0,
            },
        ),
        comment => {
            description => "HA rule description.",
            type => 'string',
            maxLength => 4096,
            optional => 1,
        },
    },
};

sub private {
    return $defaultData;
}

=head3 $class->decode_plugin_value(...)

=head3 $class->decode_plugin_value($type, $key, $value)

B<OPTIONAL:> Can be implemented in a I<rule plugin>.

Called during base plugin's C<decode_value(...)> in order to extend the
deserialization for plugin-specific values which need it (e.g. lists).

If it is not overrridden by the I<rule plugin>, then it does nothing to
C<$value> by default.

=cut

sub decode_plugin_value {
    my ($class, $type, $key, $value) = @_;

    return $value;
}

sub decode_value {
    my ($class, $type, $key, $value) = @_;

    if ($key eq 'resources') {
        my $res = {};

        for my $sid (PVE::Tools::split_list($value)) {
            if (PVE::HA::Tools::pve_verify_ha_resource_id($sid)) {
                $res->{$sid} = 1;
            }
        }

        return $res;
    } elsif ($key eq 'comment') {
        return PVE::Tools::decode_text($value);
    }

    my $plugin = $class->lookup($type);
    return $plugin->decode_plugin_value($type, $key, $value);
}

=head3 $class->encode_plugin_value(...)

=head3 $class->encode_plugin_value($type, $key, $value)

B<OPTIONAL:> Can be implemented in a I<rule plugin>.

Called during base plugin's C<encode_value(...)> in order to extend the
serialization for plugin-specific values which need it (e.g. lists).

If it is not overrridden by the I<rule plugin>, then it does nothing to
C<$value> by default.

=cut

sub encode_plugin_value {
    my ($class, $type, $key, $value) = @_;

    return $value;
}

sub encode_value {
    my ($class, $type, $key, $value) = @_;

    if ($key eq 'resources') {
        PVE::HA::Tools::pve_verify_ha_resource_id($_) for keys %$value;

        return join(',', sort keys %$value);
    } elsif ($key eq 'comment') {
        return PVE::Tools::encode_text($value);
    }

    my $plugin = $class->lookup($type);
    return $plugin->encode_plugin_value($type, $key, $value);
}

sub parse_section_header {
    my ($class, $line) = @_;

    if ($line =~ m/^(\S+):\s*(\S+)\s*$/) {
        my ($type, $ruleid) = (lc($1), $2);
        my $errmsg = undef; # set if you want to skip whole section
        eval { PVE::JSONSchema::pve_verify_configid($ruleid); };
        $errmsg = $@ if $@;
        my $config = {}; # to return additional attributes
        return ($type, $ruleid, $errmsg, $config);
    }
    return undef;
}

# General rule helpers

=head3 $class->set_rule_defaults($rule)

Sets the optional properties in the C<$rule>, which have default values, but
haven't been explicitly set yet.

=cut

sub set_rule_defaults : prototype($$) {
    my ($class, $rule) = @_;

    if (my $plugin = $class->lookup($rule->{type})) {
        my $properties = $plugin->properties();

        for my $prop (keys %$properties) {
            next if defined($rule->{$prop});
            next if !$properties->{$prop}->{default};
            next if !$properties->{$prop}->{optional};

            $rule->{$prop} = $properties->{$prop}->{default};
        }
    }
}

# Rule checks and transforms definition and methods

my $types = [];
my $checkdef;
my $transformdef;

sub register {
    my ($class) = @_;

    $class->SUPER::register($class);

    # store order in which plugin types are registered
    push @$types, $class->type();
}

=head3 $class->register_check(...)

=head3 $class->register_check($check_func, $collect_errors_func)

Used to register rule checks for a rule plugin.

=cut

sub register_check : prototype($$$) {
    my ($class, $check_func, $collect_errors_func) = @_;

    my $type = eval { $class->type() };
    $type = 'global' if $@; # check registered here in the base plugin

    push @{ $checkdef->{$type} }, [
        $check_func, $collect_errors_func,
    ];
}

=head3 $class->register_transform(...)

=head3 $class->register_transform($transform_func)

Used to register rule transformers for a rule plugin.

=cut

sub register_transform : prototype($$) {
    my ($class, $transform_func) = @_;

    my $type = eval { $class->type() };
    $type = 'global' if $@;

    push $transformdef->{$type}->@*, $transform_func;
}

=head3 $class->get_plugin_check_arguments(...)

=head3 $class->get_plugin_check_arguments($rules)

B<OPTIONAL:> Can be implemented in the I<rule plugin>.

Returns a hash, usually subsets of rules relevant to the plugin, which are
passed to the plugin's C<L<< registered checks|/$class->register_check(...) >>>
and C<L<< registered transforms|/$class->register_transform(...) >>>
so that the creation of these can be shared inbetween rule check
implementations.

=cut

sub get_plugin_check_arguments : prototype($$) {
    my ($class, $rules) = @_;

    return {};
}

=head3 $class->get_check_arguments(...)

=head3 $class->get_check_arguments($rules)

Returns the union of the plugin's check argument hashes, which are passed to the
plugin's C<L<< registered checks|/$class->register_check(...) >>> so that the
creation of these can be shared inbetween rule check implementations.

=cut

sub get_check_arguments : prototype($$) {
    my ($class, $rules) = @_;

    my $global_args = {};

    for my $type (@$types) {
        my $plugin = $class->lookup($type);
        my $plugin_args = eval { $plugin->get_plugin_check_arguments($rules) };
        next if $@; # plugin doesn't implement get_plugin_check_arguments(...)

        $global_args = { $global_args->%*, $plugin_args->%* };
    }

    return $global_args;
}

=head3 $class->check_feasibility($rules, $nodes)

Checks whether the given C<$rules> are feasible by running all checks, which
were registered with C<L<< register_check()|/$class->register_check(...) >>>,
and returns a hash map of errorneous rules.

C<$nodes> is a list of the configured cluster nodes.

The checks are run in the order in which the rule plugins were registered,
while global checks, i.e. checks between different rule types, are run at the
very last.

=cut

sub check_feasibility : prototype($$$) {
    my ($class, $rules, $nodes) = @_;

    my $global_errors = {};
    my $removable_ruleids = [];

    my $global_args = $class->get_check_arguments($rules);

    $global_args->{nodes} = $nodes;

    for my $type (@$types, 'global') {
        for my $entry (@{ $checkdef->{$type} }) {
            my ($check, $collect_errors) = @$entry;

            my $errors = $check->($global_args);
            $collect_errors->($errors, $global_errors);
        }
    }

    return $global_errors;
}

=head3 $class->transform($rules, $nodes)

Modifies C<$rules> to contain only feasible rules.

C<$nodes> is a list of the configured cluster nodes.

This is done by running all checks, which were registered with
C<L<< register_check()|/$class->register_check(...) >>> and removing any
rule, which makes the rule set infeasible, and afterwards running all
transforms on the feasible rule set, which were registered with
C<L<< register_transform()|/$class->register_transform(...) >>>.

Returns a list of messages with the reasons why rules were removed.

=cut

sub transform : prototype($$$) {
    my ($class, $rules, $nodes) = @_;

    my $messages = [];
    my $global_errors = $class->check_feasibility($rules, $nodes);

    for my $ruleid (keys %$global_errors) {
        delete $rules->{ids}->{$ruleid};
        delete $rules->{order}->{$ruleid};
    }

    for my $ruleid (sort keys %$global_errors) {
        for my $opt (sort keys %{ $global_errors->{$ruleid} }) {
            for my $message (@{ $global_errors->{$ruleid}->{$opt} }) {
                push @$messages, "Drop rule '$ruleid', because $message.\n";
            }
        }
    }

    for my $type (@$types, 'global') {
        for my $transform ($transformdef->{$type}->@*) {
            my $global_args = $class->get_check_arguments($rules);

            $transform->($rules, $global_args);
        }
    }

    return $messages;
}

=head3 $class->plugin_compile(...)

=head3 $class->plugin_compile($rules, $nodes)

B<MANDATORY:> Must be implemented in a I<rule plugin>.

Called in C<$class->compile($rules, $nodes)> in order to get a more compact
representation of the rule plugin's rules in C<$rules>, which holds only the
relevant information for the scheduler and other users.

C<$nodes> is a list of the configured cluster nodes.

=cut

sub plugin_compile {
    my ($class, $rules, $nodes) = @_;

    die "implement in subclass";
}

=head3 $class->compile(...)

=head3 $class->compile($rules, $nodes)

Compiles and returns a hash, where each key-value pair represents a rule
plugin's more compact representation compiled from the more verbose rules
defined in C<$rules>.

C<$nodes> is a list of the configured cluster nodes.

The transformation to the compact representation for each rule plugin is
implemented in C<L<< plugin_compile()|/$class->plugin_compile(...) >>>.

=cut

sub compile {
    my ($class, $rules, $nodes) = @_;

    my $compiled_rules = {};

    for my $type (@$types) {
        my $plugin = $class->lookup($type);
        my $compiled_plugin_rules = $plugin->plugin_compile($rules, $nodes);

        die "plugin_compile(...) of type '$type' must return hash reference\n"
            if ref($compiled_plugin_rules) ne 'HASH';

        $compiled_rules->{$type} = $compiled_plugin_rules;
    }

    return $compiled_rules;
}

=head1 FUNCTIONS

=cut

=head3 foreach_rule(...)

=head3 foreach_rule($rules, $func [, %opts])

Filters the given C<$rules> according to the C<$opts> and loops over the
resulting rules in the order as defined in the section config and executes
C<$func> with the parameters C<L<< ($rule, $ruleid) >>>.

The following key-value pairs for C<$opts> are:

=over

=item C<$sid>: Limits C<$rules> to those which contain the given resource C<$sid>.

=item C<$type>: Limits C<$rules> to those which are of rule type C<$type>.

=item C<$exclude_disabled_rules>: Limits C<$rules> to those which are enabled.

=item C<$sorted>: Sorts C<$rules> according to C<< $rules->{order} >>.

=back

=cut

sub foreach_rule : prototype($$;%) {
    my ($rules, $func, %opts) = @_;

    my $sid = $opts{sid};

    my @ruleids = keys $rules->{ids}->%*;
    @ruleids = sort { $rules->{order}->{$a} <=> $rules->{order}->{$b} } @ruleids
        if defined($opts{sorted});

    for my $ruleid (@ruleids) {
        my $rule = $rules->{ids}->{$ruleid};

        next if !$rule; # skip invalid rules
        next if defined($sid) && !defined($rule->{resources}->{$sid});
        next if defined($opts{type}) && $rule->{type} ne $opts{type};
        next if $opts{exclude_disabled_rules} && exists($rule->{disable});

        $func->($rule, $ruleid);
    }
}

=head3 get_next_ordinal($rules)

Returns the next available ordinal number in the C<$rules> order hash that can
be used a newly introduced rule afterwards.

=cut

sub get_next_ordinal : prototype($) {
    my ($rules) = @_;

    my $current_order = (sort { $a <=> $b } values %{ $rules->{order} })[0] || 0;

    return $current_order + 1;
}

=head1 INTER-PLUGIN RULE CHECKERS

=cut

my $has_multiple_priorities = sub {
    my ($node_affinity_rule) = @_;

    my $priority;
    for my $node (values $node_affinity_rule->{nodes}->%*) {
        $priority = $node->{priority} if !defined($priority);

        return 1 if $priority != $node->{priority};
    }
};

=head3 check_single_priority_node_affinity_in_resource_affinity_rules(...)

Returns all rules in C<$resource_affinity_rules> and C<$node_affinity_rules> as
a list of lists, each consisting of the rule type and resource id, where at
least one resource in a resource affinity rule are in node affinity rules,
which have multiple priority groups defined.

That is, the resource affinity rule cannot be statically checked to be feasible
as the selection of the priority group is dependent on the currently online
nodes.

If there are none, the returned list is empty.

=cut

sub check_single_priority_node_affinity_in_resource_affinity_rules {
    my ($resource_affinity_rules, $node_affinity_rules) = @_;

    my @conflicts = ();

    while (my ($resource_affinity_id, $resource_affinity_rule) = each %$resource_affinity_rules) {
        my $has_conflicts;
        my $resources = $resource_affinity_rule->{resources};
        my @paired_node_affinity_rules = ();

        for my $node_affinity_id (keys %$node_affinity_rules) {
            my $node_affinity_rule = $node_affinity_rules->{$node_affinity_id};

            next if sets_are_disjoint($resources, $node_affinity_rule->{resources});

            $has_conflicts = $has_multiple_priorities->($node_affinity_rule)
                if !$has_conflicts;

            push @paired_node_affinity_rules, $node_affinity_id;
        }
        if ($has_conflicts) {
            push @conflicts, ['resource-affinity', $resource_affinity_id];
            push @conflicts, ['node-affinity', $_] for @paired_node_affinity_rules;
        }
    }

    @conflicts = sort { $a->[0] cmp $b->[0] || $a->[1] cmp $b->[1] } @conflicts;
    return \@conflicts;
}

__PACKAGE__->register_check(
    sub {
        my ($args) = @_;

        return check_single_priority_node_affinity_in_resource_affinity_rules(
            $args->{resource_affinity_rules},
            $args->{node_affinity_rules},
        );
    },
    sub {
        my ($conflicts, $errors) = @_;

        for my $conflict (@$conflicts) {
            my ($type, $ruleid) = @$conflict;

            if ($type eq 'node-affinity') {
                push $errors->{$ruleid}->{resources}->@*,
                    "resources are in a resource affinity rule and cannot be in"
                    . " a node affinity rule with multiple priorities";
            } elsif ($type eq 'resource-affinity') {
                push $errors->{$ruleid}->{resources}->@*,
                    "resources are in node affinity rules with multiple priorities";
            }
        }
    },
);

=head3 check_single_node_affinity_per_positive_resource_affinity_rule(...)

Returns all rules in C<$positive_rules> and C<$node_affinity_rules> as a list of
lists, each consisting of the rule type and resource id, where one of the
resources is used in a positive resource affinity rule and more than one node
affinity rule.

If there are none, the returned list is empty.

=cut

sub check_single_node_affinity_per_positive_resource_affinity_rule {
    my ($positive_rules, $node_affinity_rules) = @_;

    my @conflicts = ();

    my @disjoint_positive_rules =
        PVE::HA::Rules::Helpers::find_disjoint_rules_resource_sets($positive_rules);

    for my $entry (@disjoint_positive_rules) {
        my $positive_resources = $entry->{resources};
        my @paired_node_affinity_rules = ();

        while (my ($node_affinity_id, $node_affinity_rule) = each %$node_affinity_rules) {
            next if sets_are_disjoint($positive_resources, $node_affinity_rule->{resources});

            push @paired_node_affinity_rules, $node_affinity_id;
        }
        if (@paired_node_affinity_rules > 1) {
            push @conflicts, ['positive', $_] for $entry->{ruleids}->@*;
            push @conflicts, ['node-affinity', $_] for @paired_node_affinity_rules;
        }
    }

    @conflicts = sort { $a->[0] cmp $b->[0] || $a->[1] cmp $b->[1] } @conflicts;
    return \@conflicts;
}

__PACKAGE__->register_check(
    sub {
        my ($args) = @_;

        return check_single_node_affinity_per_positive_resource_affinity_rule(
            $args->{positive_rules},
            $args->{node_affinity_rules},
        );
    },
    sub {
        my ($conflicts, $errors) = @_;

        for my $conflict (@$conflicts) {
            my ($type, $ruleid) = @$conflict;

            if ($type eq 'positive') {
                push $errors->{$ruleid}->{resources}->@*,
                    "resources are in multiple node affinity rules";
            } elsif ($type eq 'node-affinity') {
                push $errors->{$ruleid}->{resources}->@*,
                    "at least one resource is in a positive resource affinity"
                    . " rule and there are other resources in at least one"
                    . " other node affinity rule already";
            }
        }
    },
);

=head3 check_negative_resource_affinity_node_affinity_consistency(...)

Returns all rules in C<$negative_rules> and C<$node_affinity_rules> as a list
of lists, each consisting of the rule type and resource id, where the resources
in the negative resource affinity rule are restricted to less nodes than needed
to keep them separate by their node affinity rules.

That is, the negative resource affinity rule cannot be fullfilled as there are
not enough nodes to spread the resources on.

If there are none, the returned list is empty.

=cut

sub check_negative_resource_affinity_node_affinity_consistency {
    my ($negative_rules, $node_affinity_rules) = @_;

    my @conflicts = ();

    while (my ($negativeid, $negative_rule) = each %$negative_rules) {
        my $allowed_nodes = {};
        my $located_resources;
        my $resources = $negative_rule->{resources};
        my @paired_node_affinity_rules = ();

        for my $node_affinity_id (keys %$node_affinity_rules) {
            my ($node_affinity_resources, $node_affinity_nodes) =
                $node_affinity_rules->{$node_affinity_id}->@{qw(resources nodes)};
            my $common_resources = set_intersect($resources, $node_affinity_resources);

            next if keys %$common_resources < 1;

            $located_resources = set_union($located_resources, $common_resources);
            $allowed_nodes = set_union($allowed_nodes, $node_affinity_nodes);

            push @paired_node_affinity_rules, $node_affinity_id;
        }
        if (keys %$allowed_nodes < keys %$located_resources) {
            push @conflicts, ['negative', $negativeid];
            push @conflicts, ['node-affinity', $_] for @paired_node_affinity_rules;
        }
    }

    @conflicts = sort { $a->[0] cmp $b->[0] || $a->[1] cmp $b->[1] } @conflicts;
    return \@conflicts;
}

__PACKAGE__->register_check(
    sub {
        my ($args) = @_;

        return check_negative_resource_affinity_node_affinity_consistency(
            $args->{negative_rules},
            $args->{node_affinity_rules},
        );
    },
    sub {
        my ($conflicts, $errors) = @_;

        for my $conflict (@$conflicts) {
            my ($type, $ruleid) = @$conflict;

            if ($type eq 'negative') {
                push $errors->{$ruleid}->{resources}->@*,
                    "two or more resources are restricted to less nodes than"
                    . " available to the resources";
            } elsif ($type eq 'node-affinity') {
                push $errors->{$ruleid}->{resources}->@*,
                    "at least one resource is in a negative resource affinity"
                    . " rule and this rule would restrict these to less nodes"
                    . " than available to the resources";
            }
        }
    },
);

=head1 INTER-PLUGIN RULE TRANSFORMATION HELPERS

=cut

=head3 create_implicit_positive_resource_affinity_node_affinity_rules(...)

Modifies C<$rules> such that all resources of a positive resource affinity rule,
defined in C<$positive_rules>, where at least one of their resources is also in
a node affinity rule, defined in C<$node_affinity_rules>, makes all the other
positive resource affinity rule's resources also part of the node affinity rule.

This helper assumes that there can only be a single node affinity rule per
positive resource affinity rule as there is no heuristic yet what should be
done in the case of multiple node affinity rules.

This also makes it cheaper to infer these implicit constraints later instead of
propagating that information in each scheduler invocation.

=cut

sub create_implicit_positive_resource_affinity_node_affinity_rules {
    my ($rules, $positive_rules, $node_affinity_rules) = @_;

    my @conflicts = ();

    while (my ($positiveid, $positive_rule) = each %$positive_rules) {
        my $found_node_affinity_id;
        my $positive_resources = $positive_rule->{resources};

        for my $node_affinity_id (keys %$node_affinity_rules) {
            my $node_affinity_rule = $rules->{ids}->{$node_affinity_id};
            next if sets_are_disjoint($positive_resources, $node_affinity_rule->{resources});

            # assuming that all $resources have at most one node affinity rule,
            # take the first found node affinity rule.
            $node_affinity_rule->{resources}->{$_} = 1 for keys %$positive_resources;
            last;
        }
    }
}

__PACKAGE__->register_transform(sub {
    my ($rules, $args) = @_;

    create_implicit_positive_resource_affinity_node_affinity_rules(
        $rules,
        $args->{positive_rules},
        $args->{node_affinity_rules},
    );
});

1;
