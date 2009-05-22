=head1 NAME

OBO::ClassExpression

=head1 SYNOPSIS

=head1 DESCRIPTION

A class expression is an OBO::ClassNode whose members are identified
by a boolean or relational expression. For example, the class 'nuclei
of cardiac cells' is expressed as the intersection (an
OBO::ClassExpression::Intersection) between the set 'nucleus' (an
OBO::TermNode) and the set of all things that stand in the part_of
relation to 'class node' (a OBO::ClassExpression::RelationalExpression).

An OBO::TermNode can be formally and logically defined by stating
equivalence to a OBO::ClassExpression

  [Term]
  id: GO:new
  name: oocyte cytoplasm
  intersection_of: GO:0005737 ! cytoplasm
  intersection_of: part_of CL:0000023 ! oocyte 

This can also be thought of as necessary and sufficient conditions for
membership of a class.

Simple ontologies do not include class expressions. They can often be
ignored for many purposes.

The notion of ClassExpression here is largely borrowed from OWL and
Description Logics


=cut

package OBO::ClassExpression;
use Moose;
use strict;
extends 'OBO::ClassNode';
use OBO::ClassExpression::RelationalExpression;
use OBO::ClassExpression::Intersection;

# abstract class - no accessors

# utility methods follow...

=head2 ID Expressions

A class expression can be expressed as an ID expression. See:

http://www.geneontology.org/GO.format.obo-1_3.shtml#S.1.6

For example:
  GO:0005737^part_of(CL:0000023) 

The set of all cytoplasms that are part of some oocyte

The grammar is

  OBO::ClassExpression = OBO::BooleanExpression | OBO::RelationalExpression | OBO::TermNode
  OBO::BooleanExpression = OBO::Intersection | OBO::Union
  OBO::Intersection = OBO::ClassExpression '^' OBO::ClassExpression
  OBO::Union = OBO::ClassExpression '|' OBO::ClassExpression
  OBO::RelationalExpression = OBO::RelationNode '(' OBO::ClassExpression ')'

=head3 parse_idexpr

Generates a OBO::ClassExpression based on an ID expression string

=cut

sub parse_idexpr {
    my $self = shift;
    my $g = shift;
    my $expr = shift;
    return unless $expr;
    print STDERR "Parsing: $expr\n";
    my @toks = split(/([\(\)\^\|])/,$expr);
    return _parse_idexpr_toks($g,\@toks);
}

sub _parse_idexpr_toks {
    my $g = shift;
    my $toks = shift;
    print STDERR "Parsing tokens: @$toks\n";
    if (!@$toks) {
        return;
    }
    my $tok = shift @$toks;
    if (!@$toks) {
        # atom
        printf STDERR "atom: $tok\n";
        return $tok;
    }
    my $this;
    if ($toks->[0] eq '(') {
        # relational expression
        shift @$toks;
        my $filler = _parse_idexpr_toks($g,$toks);
        $this = new OBO::ClassExpression::RelationalExpression(relation=>$tok,target=>$filler);
        printf STDERR "relexpr $tok $filler ==> $this\n";
    }
    else {
        printf STDERR "atom: $tok\n";
        $this = $g->noderef($tok);
    }
    if (@$toks) {
        my $combo;
        my $op = shift @$toks;
        my $next = _parse_idexpr_toks($g,$toks);
        if ($op eq '^') {
            printf STDERR "intersection: $this $next\n";
            $combo = new OBO::ClassExpression::Intersection(arguments=>[$this,$next]);
        }
        elsif ($op eq '|') {
        }
        elsif ($op eq ')') {
            # TODO: check balance
            printf STDERR "end-brace: $this\n";
            return $this;
        }
        else {
        }
        return $combo; # TODO -- DNF
    }
    printf STDERR "return: $this\n";
    return $this;
}

1;

