package OBO::ClassExpression::Intersection;
use Moose;
use strict;
extends 'OBO::ClassExpression::BooleanExpression';

sub operator { ' AND ' }

=head1 NAME

OBO::Intersection

=head1 SYNOPSIS

=head1 DESCRIPTION

An OBO::ClassExpression::BooleanExpression in which the set operator is one of intersection.

=cut

1; 
