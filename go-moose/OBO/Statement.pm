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
has 'inferred' => ( is=>'ro', isa=>'Bool');
has 'negated' => ( is=>'ro', isa=>'Bool');
has 'is_intersection' => ( is=>'ro', isa=>'Bool');


use overload ('""' => 'as_string');
sub as_string {
    my $self = shift;
    return sprintf("(%s --[%s]-->%s)",$self->node,$self->relation ? $self->relation : '?', $self->target);
}

sub equals {
    my $self = shift;
    my $s = shift;
    if ($self->node->id eq $s->node->id &&
        ($self->relation && $s->relation && $self->relation->id eq $s->relation->id) && 
        $self->target eq $s->target) {
        return $self->is_intersection && $s->is_intersection;
    }
    return 0;
}


=head1 NAME

OBO::Statement

=head1 SYNOPSIS

  printf '%s --[%s]--> %s', $s->node, $s->relation, $->target;

=head1 DESCRIPTION

A type of OBO::Statement that connects an OBO::Node object to another
entity via a OBO::RelationNode object. This can be thought of as a
sentence or statement about a node.

In RDF and Chado terminology, the node can be thought of as the
"subject", and the target the "object". The terms "subject" and
"object" are avoided due to being overloaded.

The two subtypes are OBO::LinkStatement (edges) or
OBO::LiteralStatement (tag-values). For most bio-ontologies, the
Statements will be LinkStatements.

Statements have the roles OBO::Attributed and OBO::Identified. This
means they can have metadata attached. For example, who made the
statement and when.


=cut

1;
