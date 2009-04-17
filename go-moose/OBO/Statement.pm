package OBO::Statement;
use Moose;
use strict;
with 'OBO::Attributed';
with 'OBO::Identified';

use OBO::Node;
use OBO::RelationNode;

has 'node' => ( is=>'ro', isa=>'OBO::Node', coerce=>1 );
has 'relation' => ( is=>'ro', isa=>'OBO::RelationNode', coerce=>1 );
has 'target' => ( is=>'ro', isa=>'Item');


use overload ('""' => 'as_string');
sub as_string {
    my $self = shift;
    return sprintf("(%s --[%s]-->%s)",$self->node,$self->relation ? $self->relation : '?', $self->target);
}

1;
