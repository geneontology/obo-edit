=head1 NAME

OBO::Defined

=head1 SYNOPSIS

=head1 DESCRIPTION

A role for any kind of entity that can have a definition attached

Typically one would define an OBO::RelationNode or an OBO::ClassNode,
but not an OBO::InstanceNode (instances do not have definitions, but
they may have descriptions)

There are both textual definitions and logical definitions. A logical
definition is an equivalence relationship to an
OBO::ClassExpression. Simple terminology-style ontologies do not have
logical definitions.

=head2 TBD

Is this over-abstraction to use a Role? This could be simply mixed in with Node

=cut

package OBO::Definable;
use Moose::Role;

has definition => (is=>'rw', isa=>'Str');
has definition_xrefs => (is=>'rw', isa=>'ArrayRef[OBO::Node]');
has logical_definition => (is=>'rw', isa=>'OBO::ClassExpression');
has logical_definition_xrefs => (is=>'rw', isa=>'ArrayRef[OBO::Node]'); # not yet in obo-format

1;

