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

Formally, a class is a collection of instances. However, in many cases these are not instantiated in perl

Classes can either be explicitly named (OBO::TermNode) or they can be logical boolean expressions (OBO::ClassExpression)

=head1 SEE ALSO

OBO::Graph

=cut
