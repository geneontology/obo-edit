package OBO::InferenceEngine;
use Moose;
use strict;
use OBO::Statement;
use OBO::Annotation;
use OBO::Graph;
use OBO::Node;
use OBO::TermNode;
use OBO::RelationNode;

has graph => (is=>'rw', isa=> 'OBO::Graph');
has inferred_graph => (is=>'rw', isa=> 'OBO::Graph', default=>sub{new OBO::Graph});

sub backward_chain {
    my $self = shift;
    my $n = shift;
    my $g = $self->graph;
    my $ig = $self->inferred_graph;
    
    # initialize link set based on input node;
    # we will iteratively extend upwards
    my @links = @{$g->get_target_links($n)};
    my %outlink_h = ();
    my %link_closure_h = ();
    while (@links) {
       my $link = shift @links;
       next if $outlink_h{$link};
       $outlink_h{$link} = 1;
       my $r = $link->relation;
       my $t = $link->target;
       #my @extlinks = @{$g->linkset->about($t)};
       my $extlinks = $self->extend_link($link);
       if (@$extlinks) {
           push(@links,@$extlinks);
           #push(@outlinks,@$extlinks);
           map {$link_closure_h{$_}=1} @$extlinks;
       }
    }
    return [keys %outlink_h];
}

=head2 get_inferred_target_links (subject OBO::Node, relation OBO::RelationNode OPTIONAL)

given a subject (child), get inferred target (parent) links

if relation is specified, also filters results on relation

backward-chaining

=cut

sub get_inferred_target_links {
    my $self = shift;
    my $n = shift;
    my $g = $self->graph;
    my $ig = $self->inferred_graph;
    my $tlinks = $ig->get_target_links($n);
    if (@$tlinks) {
        return $tlinks;
    }
    
    # initialize link set based on input node;
    # we will iteratively extend upwards
    my @links = @{$g->get_target_links($n)};
    #printf STDERR "target $n => @links\n";

    my %outlink_h = ();
    my %link_closure_h = ();
    while (@links) {
       my $link = shift @links;
       next if $outlink_h{$link};
       $outlink_h{$link} = $link;
       my $r = $link->relation;
       my $t = $link->target;
       #my @extlinks = @{$g->linkset->about($t)};
       my $extlinks = $self->extend_link($link);
       #printf STDERR "extending $link => @$extlinks\n";
       if (@$extlinks) {
           push(@links,@$extlinks);
           #push(@outlinks,@$extlinks);
           map {$link_closure_h{$_}=1} @$extlinks;
       }
    }
    $ig->add_links([values %outlink_h]);
    return [values %outlink_h];
}

=head2 get_inferred_target_nodes (subject OBO::Node, relation OBO::RelationNode OPTIONAL)

given a subject (child), get inferred target (parent) nodes

if relation is specified, also filters results on relation

backward-chaining

=cut

sub get_inferred_target_nodes {
    my $self = shift;
    my %tn = ();
    foreach my $link (@{ $self->get_inferred_target_links(@_) }) {
        $tn{$link->target->id} = $link->target;
    }
    return [values %tn];
}

sub extend_link {
    my $self = shift;
    my $link = shift;
    my $rel_1 = $link->relation;
    my @newlinks = ();
    foreach my $xlink (@{$self->graph->get_target_links($link->target)}) {
        #printf STDERR "  XLINK: $xlink\n";
        my $rel_2 = $xlink->relation;
        my $rel = $self->relation_composition($rel_1, $rel_2);
        #printf STDERR "  RC: $rel\n";
        if ($rel) {
            my $newlink = new OBO::LinkStatement(node=>$link->node,
                                                 relation=>$rel,
                                                 target=>$xlink->target);
            # todo - provenance/evidence of link
            push(@newlinks, $newlink);
        }
    }
    return \@newlinks;
}

sub get_nonredundant_set {
    my $self = shift;
    my $nodes = shift;
    #print STDERR "Finding NR set for @$nodes\n";
    my %nh = map { ($_ => $_) } @$nodes;
    foreach my $node (@$nodes) {
        my $targets = $self->get_inferred_target_nodes($node);
        foreach (@$targets) {
            delete $nh{$_->id};
        }
    }
    # TODO
    return [values %nh];
}

sub relation_composition {
    my $self = shift;
    my $r1 = shift;
    my $r2 = shift;
    if ($r1->equals($r2) && $r1->transitive) {
        return $r1;
    }
    if ($r1->is_subsumption && $r2->propagates_over_is_a) {
        return $r2;
    }
    if ($r2->is_subsumption && $r1->propagates_over_is_a) {
        return $r1;
    }
    if ($r1->transitive_over && $r1->transitive_over->equals($r2)) {
        return $r1;
    }
    # TODO: arbitrary chains
    return undef;
}

sub forward_chain {
    my $self = shift;
    my $g = $self->graph;
    my $ig = new OBO::Graph;
    $ig->copy_from($g);
    $self->inferred_graph($ig);
    $self->calculate_deductive_closure;
    
}

sub calculate_deductive_closure {
    my $self = shift;
    my $g = $self->graph;
    my $ig = $self->inferred_graph;
    
    my $saturated = 0;
    while (!$saturated) {
        
    }
}

sub subsumed_by {
    my $self = shift;
    my $parent = shift; # OBO::Class
    my $child = shift;  # OBO::Class

    my $subsumes = 0;

    if (grep {$_->id eq $parent->id} @{$self->get_inferred_target_nodes($child, new OBO::RelationNode(id=>'is_a'))}) {
        return 1;
    }

    if ($parent->isa('OBO::ClassExpression')) {
        if ($parent->isa('OBO::ClassExpression::RelationalExpression')) {
        }
        elsif ($parent->isa('OBO::ClassExpression::BooleanExpression')) {
            my $args = $parent->arguments;
            if ($parent->isa('OBO::ClassExpression::Intersection')) {
                $subsumes = 1;
                foreach my $arg (@$args) {
                    if (!$self->subsumed_by($child, $arg)) {
                        $subsumes = 0;
                        last;
                    }
                }
            }
            elsif ($parent->isa('OBO::ClassExpression::Union')) {
                foreach my $arg (@$args) {
                    if ($self->subsumed_by($child, $arg)) {
                        $subsumes = 1;
                        last;
                    }
                }
            }
            else {
                $self->throw("cannot infer with $parent");
            }
        }
        else {
            
        }
    }
    return $subsumes;
}

=head1 NAME

OBO::InferenceEngine

=head1 SYNOPSIS

NOT FULLY IMPLEMENTED

=head1 DESCRIPTION

An OBO::Graph is a collection of OBO::Statements. These statements can
be either 'asserted' or 'inferred'. Inferred statements are created by
an Inference Engine. An InferenceEngine object provides two accessors,
'graph' for the source graph and 'inferred_graph' for the set of
statements derived from the source graph after applying rules that
take into account properties of the relations in the statements.

The notion of transitive closure in a graph can be expressed in terms
of the deductive closure of a graph of links in which each
OBO::RelationNode has the property 'transitive'. The notion of
ancestry in a graph can be thought of as the inferred links where the
relations are transitive.

=head2 Rules

Rules are horn rules with sets of statements in the antecedent and
typically a single statement in the consequent.

In the notation below, subject and target nodes are indicated with
lower case variables (x, y, z, ...) and relations (OBO::RelationNode)
are indicated with upper case (R, R1, R2, ...). Each statement is
written in this order:

  $s->node $->relation $s->target

=head3 Transitivity

  x R y, y R z => x R z (where $R->transitive)

=head3 Propagation over and under is_a

  x R y, y is_a z => x R z (where $R->propagates_over_is_a)
  x is_a y, y R z => x R z (where $R->propagates_over_is_a)

=head3 Link composition

  x R1 y, y R2 z => x R z (where R=R1.R2)

The above two rules are degenerate cases of this one.

The notion R = R1.R2 is used to specify these compositions. See $r->holds_over_chain_list

=head3 Reflexivity

 x ? ? => x R x (where $R->reflexive)

ie where x exists, x stands in relation R to itself

=head3 Symmetry

 x R y => y R x (where $R->symmetric)

Note that the type level adjacenct_to relation is not transitive

=head3 Inverses

 x R1 y => y R2 x (where $R1->inverse_of_list contains $R2 or vice versa)

Note that the type level part_of relation is not the inverse of has_part

=head3 Inference over OBO::ClassExpressions

class expressions define sets of entities. We can infer the existing
of subset/subsumption relationships between these sets.

TODO

=head2 Inference strategies

=head3 Backward chaining

Starting for a given node, find all inferred Statements related to
that node by applying rules. Ancestors statements: all statements in
which this node plays the role of subject. Descendants statements: all
statements in which this node plays the role of target.

Can be combined with memoization, in which case subsequent queries can
retrieve cached results.

=head3 Forward chaining

Starting with a graph of asserted statements, iteratively keep
applying rules and adding resulting statements to the inferred graph,
until no new statements are added.

=head1 STATUS

PRE-ALPHA!!!

=cut

1;
