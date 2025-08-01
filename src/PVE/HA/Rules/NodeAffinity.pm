package PVE::HA::Rules::NodeAffinity;

use strict;
use warnings;

use Storable qw(dclone);

use PVE::Cluster;
use PVE::JSONSchema qw(get_standard_option);
use PVE::Tools;

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
            'pve-ha-group-node-list',
            {
                completion => \&PVE::Cluster::get_nodelist,
                optional => 0,
            },
        ),
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

=head1 NODE AFFINITY RULE HELPERS

=cut

my $get_resource_node_affinity_rule = sub {
    my ($rules, $sid) = @_;

    # with the current restriction a resource can only be in one node affinity rule
    my $node_affinity_rule;
    PVE::HA::Rules::foreach_rule(
        $rules,
        sub {
            my ($rule) = @_;

            $node_affinity_rule = dclone($rule) if !$node_affinity_rule;
        },
        sid => $sid,
        type => 'node-affinity',
        exclude_disabled_rules => 1,
    );

    return $node_affinity_rule;
};

=head3 get_node_affinity($rules, $sid, $online_node_usage)

Returns a list of two hashes representing the node affinity of C<$sid>
according to the node affinity rules in C<$rules> and the available nodes in
C<$online_node_usage>.

The first hash is a hash set of available nodes, i.e. nodes where the
resource C<$sid> is allowed to be assigned to, and the second hash is a hash set
of preferred nodes, i.e. nodes where the resource C<$sid> should be assigned to.

If there are no available nodes at all, returns C<undef>.

=cut

sub get_node_affinity : prototype($$$) {
    my ($rules, $sid, $online_node_usage) = @_;

    my $node_affinity_rule = $get_resource_node_affinity_rule->($rules, $sid);

    # default to a node affinity rule with all available nodes
    if (!$node_affinity_rule) {
        for my $node ($online_node_usage->list_nodes()) {
            $node_affinity_rule->{nodes}->{$node} = { priority => 0 };
        }
    }

    # add remaining nodes with low priority for non-strict node affinity rules
    if (!$node_affinity_rule->{strict}) {
        for my $node ($online_node_usage->list_nodes()) {
            next if defined($node_affinity_rule->{nodes}->{$node});

            $node_affinity_rule->{nodes}->{$node} = { priority => -1 };
        }
    }

    my $allowed_nodes = {};
    my $prioritized_nodes = {};

    while (my ($node, $props) = each %{ $node_affinity_rule->{nodes} }) {
        next if !$online_node_usage->contains_node($node); # node is offline

        $allowed_nodes->{$node} = 1;
        $prioritized_nodes->{ $props->{priority} }->{$node} = 1;
    }

    my $preferred_nodes = {};
    my $highest_priority = (sort { $b <=> $a } keys %$prioritized_nodes)[0];
    $preferred_nodes = $prioritized_nodes->{$highest_priority} if defined($highest_priority);

    return ($allowed_nodes, $preferred_nodes);
}

1;
