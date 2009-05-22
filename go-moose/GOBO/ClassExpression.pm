=head1 NAME

GOBO::ClassExpression

=head1 SYNOPSIS

=head1 DESCRIPTION

A class expression is an GOBO::ClassNode whose members are identified
by a boolean or relational expression. For example, the class 'nuclei
of cardiac cells' is expressed as the intersection (an
GOBO::ClassExpression::Intersection) between the set 'nucleus' (an
GOBO::TermNode) and the set of all things that stand in the part_of
relation to 'class node' (a GOBO::ClassExpression::RelationalExpression).

An GOBO::TermNode can be formally and logically defined by stating
equivalence to a GOBO::ClassExpression

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

package GOBO::ClassExpression;
use Moose;
use strict;
extends 'GOBO::ClassNode';
use GOBO::ClassExpression::RelationalExpression;
use GOBO::ClassExpression::Intersection;

# abstract class - no accessors

# utility methods follow...

=head2 ID Expressions

A class expression can be expressed as an ID expression. See:

http://www.geneontology.org/GO.format.obo-1_3.shtml#S.1.6

For example:
  GO:0005737^part_of(CL:0000023) 

The set of all cytoplasms that are part of some oocyte

The grammar is

  GOBO::ClassExpression = GOBO::BooleanExpression | GOBO::RelationalExpression | GOBO::TermNode
  GOBO::BooleanExpression = GOBO::Intersection | GOBO::Union
  GOBO::Intersection = GOBO::ClassExpression '^' GOBO::ClassExpression
  GOBO::Union = GOBO::ClassExpression '|' GOBO::ClassExpression
  GOBO::RelationalExpression = GOBO::RelationNode '(' GOBO::ClassExpression ')'

=head3 parse_idexpr

Generates a GOBO::ClassExpression based on an ID expression string

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
        $this = new GOBO::ClassExpression::RelationalExpression(relation=>$tok,target=>$filler);
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
            $combo = new GOBO::ClassExpression::Intersection(arguments=>[$this,$next]);
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

