package GOBO::ClassExpression::Intersection;
use Moose;
use strict;
extends 'GOBO::ClassExpression::BooleanExpression';

sub operator { ' AND ' }

=head1 NAME

GOBO::ClassExpression::Intersection

=head1 SYNOPSIS

=head1 DESCRIPTION

A GOBO::ClassExpression::BooleanExpression in which the set operator is one of intersection.

An Intersection that consists of arguments a1, ..., aN means the set
{x : x instance_of(a1), .... x instance_of(aN)}

It is conventional in GO and many OBO ontologies for class
intersections to follow the genus-differentia pattern. In this case,
exactly one of the arguments is an GOBO::TermNode (the genus), and all
the other arguments are of type
GOBO::ClassExpression::RelationalExpression

For example, after parsing the following OBO stanza into variable $class:

  [Term]
  id: GO:0043005 ! neuron projection
  intersection_of: GO:0042995 ! cell projection
  intersection_of: part_of CL:0000540 ! neuron

The following boolean expressions are all true

  $class->id eq 'GO:0043005';
  $class->logical_definion->isa('GOBO::ClassExpression::Intersection');
  $class->logical_definion->operator eq 'AND';
  scalar(@{$class->logical_definion->arguments}) == 2;
  grep { $_->id eq 'GO:0042995' } @{$class->logical_definion->arguments};
  grep { $_->isa('GOBO::ClassExpression::RelationalExpression') &&
             $_->relation->id eq 'part_of' &&
             $_->target->id eq 'CL:0000540'
             } @{$class->logical_definion->arguments};

The "" operator is overloaded, so the logical defition is written out as

  GO:0042995^part_of(CL:0000540)

=head2 OWL Translation

Same as intersectionOf description expressions in OWL

=cut

1; 
