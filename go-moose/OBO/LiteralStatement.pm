package OBO::LiteralStatement;
use Moose;
use strict;
extends 'OBO::Statement';
use OBO::Node;

has 'target' => ( is=>'ro', isa=>'Value');

# TODO -- use this or use frame-style? both

=head1 NAME

OBO::LiteralStatement

=head1 SYNOPSIS

  printf '%s --[%s]--> %s', $s->node, $s->relation, $->target;

=head1 DESCRIPTION

LiteralStatements are one of two basic types of
OBO::Statement. Whereas an OBO::LinkStatement will connect a node to
another node, a literal statement connects a node to a "literal", or
Value which can be a a string, number of other kind of moose Value.

=head2 What's this for?

LiteralStatements inherit the roles OBO::Attributed and
OBO::Identified (via OBO::Statement). This means they can have
metadata attached, in the same way as edges in a graph. This stands in
contrast to implementing the tag-values as accessors on the node
itself. The difference between these styles is sometimes called
"Axiom-style" vs "Frame-style", depending on whether the statement or
the node is the primary entity of interest.

At this stage the object model allows both styles.... It may even be
possible to use the Moose machinery to allow seamless switching
between both...?

=cut


1;
