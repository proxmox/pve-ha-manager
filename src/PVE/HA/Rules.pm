package PVE::HA::Rules;

use strict;
use warnings;

use PVE::JSONSchema qw(get_standard_option);
use PVE::Tools;

use PVE::HA::Tools;

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

This packages inherits its config-related methods from C<L<PVE::SectionConfig>>
and therefore rule plugins need to implement methods from there as well.

=head1 USAGE

Each I<rule plugin> is required to implement the methods C<L<type()>>,
C<L<properties()>>, and C<L<options>> from the C<L<PVE::SectionConfig>> to
extend the properties of this I<base plugin> with plugin-specific properties.

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

    if ($key eq 'comment') {
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

    if ($key eq 'comment') {
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

# Rule checks definition and methods

my $types = [];
my $checkdef;

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

=head3 $class->get_plugin_check_arguments(...)

=head3 $class->get_plugin_check_arguments($rules)

B<OPTIONAL:> Can be implemented in the I<rule plugin>.

Returns a hash, usually subsets of rules relevant to the plugin, which are
passed to the plugin's C<L<< registered checks|/$class->register_check(...) >>>
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

=head3 $class->check_feasibility($rules)

Checks whether the given C<$rules> are feasible by running all checks, which
were registered with C<L<< register_check()|/$class->register_check(...) >>>,
and returns a hash map of errorneous rules.

The checks are run in the order in which the rule plugins were registered,
while global checks, i.e. checks between different rule types, are run at the
very last.

=cut

sub check_feasibility : prototype($$) {
    my ($class, $rules) = @_;

    my $global_errors = {};
    my $removable_ruleids = [];

    my $global_args = $class->get_check_arguments($rules);

    for my $type (@$types, 'global') {
        for my $entry (@{ $checkdef->{$type} }) {
            my ($check, $collect_errors) = @$entry;

            my $errors = $check->($global_args);
            $collect_errors->($errors, $global_errors);
        }
    }

    return $global_errors;
}

=head3 $class->canonicalize($rules)

Modifies C<$rules> to contain only feasible rules.

This is done by running all checks, which were registered with
C<L<< register_check()|/$class->register_check(...) >>> and removing any
rule, which makes the rule set infeasible.

Returns a list of messages with the reasons why rules were removed.

=cut

sub canonicalize : prototype($$) {
    my ($class, $rules) = @_;

    my $messages = [];
    my $global_errors = $class->check_feasibility($rules);

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

    return $messages;
}

=head1 FUNCTIONS

=cut

=head3 foreach_rule(...)

=head3 foreach_rule($rules, $func [, $opts])

Filters the given C<$rules> according to the C<$opts> and loops over the
resulting rules in the order as defined in the section config and executes
C<$func> with the parameters C<L<< ($rule, $ruleid) >>>.

The filter properties for C<$opts> are:

=over

=item C<$type>: Limits C<$rules> to those which are of rule type C<$type>.

=item C<$exclude_disabled_rules>: Limits C<$rules> to those which are enabled.

=back

=cut

sub foreach_rule : prototype($$;$) {
    my ($rules, $func, $opts) = @_;

    my $type = $opts->{type};
    my $exclude_disabled_rules = $opts->{exclude_disabled_rules};

    my @ruleids = sort {
        $rules->{order}->{$a} <=> $rules->{order}->{$b}
    } keys %{ $rules->{ids} };

    for my $ruleid (@ruleids) {
        my $rule = $rules->{ids}->{$ruleid};

        next if !$rule; # skip invalid rules
        next if defined($type) && $rule->{type} ne $type;
        next if $exclude_disabled_rules && exists($rule->{disable});

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

1;
