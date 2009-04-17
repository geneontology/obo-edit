package OBO::RelationNode;
use Moose;
use strict;
extends 'OBO::Node';
use Moose::Util::TypeConstraints;

coerce 'OBO::RelationNode'
    => from 'OBO::Node'
    => via { new OBO::RelationNode(%$_) }
    => from 'Str'
    => via { new OBO::RelationNode(id=>$_) };

has 'transitive' => ( is=>'ro', isa=>'Bool' );
has 'symmetric' => ( is=>'ro', isa=>'Bool' );
has 'asymmetric' => ( is=>'ro', isa=>'Bool' );
has 'cyclic' => ( is=>'ro', isa=>'Bool' );
has 'reflexive' => ( is=>'ro', isa=>'Bool' );
has 'irreflexive' => ( is=>'ro', isa=>'Bool' );
has 'functional' => ( is=>'ro', isa=>'Bool' );
has 'inverse_functional' => ( is=>'ro', isa=>'Bool' );

1;
