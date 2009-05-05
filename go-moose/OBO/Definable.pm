=head1 NAME

OBO::Defined

=head1 SYNOPSIS

=head1 DESCRIPTION

A role for any kind of entity that can have a definition attached

Typically one would define an OBO::RelationNode or an OBO::ClassNode,
but not an OBO::InstanceNode (instances do not have definitions, but
they may have descriptions)

=head2 TBD

Is this over-abstraction? This could be simply mixed in with Node

=cut

package OBO::Definable;
use Moose::Role;

has definition => (is=>'rw', isa=>'Str');
has definition_xrefs => (is=>'rw', isa=>'ArrayRef[Str]');

1;

