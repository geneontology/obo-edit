package OBO::ClassExpression::BooleanExpression;
use Moose;
use strict;
extends 'OBO::ClassExpression';

has 'arguments' => (is=>'rw', isa=>'ArrayRef[OBO::Node]');

sub operator { " ? " }

use overload ('""' => 'as_string');
sub as_string {
    my $self = shift;
    return join($self->operator, @{$self->arguments});
}

=head1 NAME

OBO::ClassExpression::BooleanExpression

=head1 SYNOPSIS

=head1 DESCRIPTION

An OBO::ClassExpression in which the members are constructed via a
boolean operation. These are AND, OR, NOT - or in set terms, OBO::ClassExpression::Intersection, OBO::ClassExpression::Union
or OBO::ClassExpression::Complement)

=cut

1; 
