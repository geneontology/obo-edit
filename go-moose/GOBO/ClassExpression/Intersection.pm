package GOBO::ClassExpression::Intersection;
use Moose;
use strict;
extends 'GOBO::ClassExpression::BooleanExpression';

sub operator { ' AND ' }

=head1 NAME

GOBO::Intersection

=head1 SYNOPSIS

=head1 DESCRIPTION

An GOBO::ClassExpression::BooleanExpression in which the set operator is one of intersection.

=cut

1; 
