=head1 NAME

GOBO::Defined

=head1 SYNOPSIS

=head1 DESCRIPTION

A role for any kind of entity that can have a definition attached

Typically one would define an GOBO::RelationNode or an GOBO::ClassNode,
but not an GOBO::InstanceNode (instances do not have definitions, but
they may have descriptions)

There are both textual definitions and logical definitions. A logical
definition is an equivalence relationship to an
GOBO::ClassExpression. Simple terminology-style ontologies do not have
logical definitions.

=head2 TBD

Is this over-abstraction to use a Role? This could be simply mixed in with Node

=cut

package GOBO::Definable;
use Moose::Role;

has definition => (is=>'rw', isa=>'Str');
has definition_xrefs => (is=>'rw', isa=>'ArrayRef[GOBO::Node]');
has logical_definition => (is=>'rw', isa=>'GOBO::ClassExpression');
has logical_definition_xrefs => (is=>'rw', isa=>'ArrayRef[GOBO::Node]'); # not yet in obo-format
has union_definition => (is=>'rw', isa=>'GOBO::ClassExpression::Union');

1;

