package OBO::ClassExpression::Union;
use Moose;
use strict;
extends 'OBO::ClassExpression::BooleanExpression';

sub operator { ' OR ' }

=head1 NAME

OBO::Union

=head1 SYNOPSIS

=head1 DESCRIPTION

An OBO::ClassExpression::BooleanExpression in which the set operator is one of union.

=cut

1; 
