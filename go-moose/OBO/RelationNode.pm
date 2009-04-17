package OBO::RelationNode;
use Moose;
use strict;
extends 'OBO::Node';
with 'OBO::Definable';
use Moose::Util::TypeConstraints;

coerce 'OBO::RelationNode'
    => from 'OBO::Node'
    => via { new OBO::RelationNode(%$_) }
    => from 'Str'
    => via { new OBO::RelationNode(id=>$_) };

has transitive => ( is=>'rw', isa=>'Bool' );
has symmetric => ( is=>'rw', isa=>'Bool' );
has asymmetric => ( is=>'rw', isa=>'Bool' );
has cyclic => ( is=>'rw', isa=>'Bool' );
has reflexive => ( is=>'rw', isa=>'Bool' );
has irreflexive => ( is=>'rw', isa=>'Bool' );
has functional => ( is=>'rw', isa=>'Bool' );
has inverse_functional => ( is=>'rw', isa=>'Bool' );

sub unary_property_names { 
    return qw(has transitive symmetric asymmetric cyclic reflexive irreflexive functional inverse_functional);
}

1;
