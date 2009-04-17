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

1; 
