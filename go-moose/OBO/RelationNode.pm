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
    => via { my $rel = new OBO::RelationNode(id=>$_); $rel->post_init;return $rel; };  # TODO -- is there a more elegant way of doing this?

has transitive => ( is=>'rw', isa=>'Bool' );
has symmetric => ( is=>'rw', isa=>'Bool' );
has asymmetric => ( is=>'rw', isa=>'Bool' );
has antisymmetric => ( is=>'rw', isa=>'Bool' );
has cyclic => ( is=>'rw', isa=>'Bool' );
has reflexive => ( is=>'rw', isa=>'Bool' );
has irreflexive => ( is=>'rw', isa=>'Bool' );
has functional => ( is=>'rw', isa=>'Bool' );
has inverse_functional => ( is=>'rw', isa=>'Bool' );
has transitive_over => ( is=>'rw', isa=>'OBO::RelationNode' );

sub post_init {
    my $self = shift;
    if ($self->is_subsumption) {
        $self->transitive(1);
        $self->reflexive(1);
        $self->antisymmetric(1);
    }
}

sub unary_property_names { 
    return qw(has transitive symmetric asymmetric cyclic reflexive irreflexive functional inverse_functional);
}

sub is_subsumption {
    return shift->id eq 'is_a';
}

sub propagates_over_is_a {
    return 1; # by default all links propagate over is_a
}

1;
