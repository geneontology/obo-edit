package GOBO::ClassExpression::Union;
use Moose;
use strict;
extends 'GOBO::ClassExpression::BooleanExpression';

sub operator { ' OR ' }

=head1 NAME

GOBO::Union

=head1 SYNOPSIS

=head1 DESCRIPTION

An GOBO::ClassExpression::BooleanExpression in which the set operator is one of union.

=cut

1; 
