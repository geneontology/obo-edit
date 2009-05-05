package OBO::ClassNode;
use Moose;
use strict;
extends 'OBO::Node';

use Moose::Util::TypeConstraints;

coerce 'OBO::ClassNode'
      => from 'Str'
      => via { new OBO::ClassNode(id=>$_) };

1;

=head1 NAME

OBO::ClassNode

=head1 SYNOPSIS

  printf '%s "%s"', $n->id, $n->label;

=head1 DESCRIPTION

Formally, a class is a collection of instances. However, in many cases these are not instantiated in perl.

ClassNodes can either be explicitly named (OBO::TermNode) or they can be logical boolean expressions (OBO::ClassExpression)

=head2 Terminological note

Note the parallel terminology: ontology formalisms consist of classes
(types) and instances (particulars). These should NOT be confused with
their object-oriented counterparts. An instance of the GO type
"nucleus" is an actual cell nucleus. These are almost never
"instantiated" in the object-oriented sense, but in reality there are
trillions of these instances. ClassNodes can be thought of as sets,
and InstanceNodes for their extension.

Here we use the term "ClassNode" and "InstanceNode" to denote elements
of the perl object model.

=head1 SEE ALSO

OBO::Graph

=cut
