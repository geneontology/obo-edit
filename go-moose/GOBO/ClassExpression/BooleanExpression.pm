package GOBO::ClassExpression::BooleanExpression;
use Moose;
use strict;
extends 'GOBO::ClassExpression';

has 'arguments' => (is=>'rw', isa=>'ArrayRef[GOBO::Node]');

sub operator { " ? " }

use overload ('""' => 'as_string');
sub as_string {
    my $self = shift;
    return join($self->operator, @{$self->arguments});
}

=head1 NAME

GOBO::ClassExpression::BooleanExpression

=head1 SYNOPSIS

=head1 DESCRIPTION

An GOBO::ClassExpression in which the members are constructed via a
boolean operation. These are AND, OR, NOT - or in set terms, GOBO::ClassExpression::Intersection, GOBO::ClassExpression::Union
or GOBO::ClassExpression::Complement)

=cut

1; 
