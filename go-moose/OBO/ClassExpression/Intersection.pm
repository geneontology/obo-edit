package OBO::ClassExpression::Intersection;
use Moose;
use strict;
extends 'OBO::ClassExpression::BooleanExpression';

sub operator { ' AND ' }

1; 
