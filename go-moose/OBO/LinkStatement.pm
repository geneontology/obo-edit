package OBO::LinkStatement;
use Moose;
use strict;
extends 'OBO::Statement';
use OBO::Node;

has 'target' => ( is=>'ro', isa=>'OBO::Node', coerce=>1 );
has 'distance_index' => ( is=>'rw', isa=>'HashRef[Number]', coerce=>1 );

=head1 NAME

OBO::LinkStatement

=head1 SYNOPSIS

  printf '%s --[%s]--> %s', $s->node->id, $s->relation, $->target->id;

=head1 DESCRIPTION

A type of OBO::Statement that connects two OBO::Node objects via a OBO::RelationNode object.

Can also be thought of as an "edge" in an OBO::Graph

LiteralStatements inherit the roles OBO::Attributed and
OBO::Identified (via OBO::Statement). This means they can have
metadata attached. For example, who created the edge, when and why.

=head2 Subtypes

An important subtype of this class is OBO::Annotation, which attaches
evidence to edges.

This class could conceivably be extended to add accessors for distance
etc for phylogenetic trees

=cut

1;
