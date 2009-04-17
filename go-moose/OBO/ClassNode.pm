package OBO::ClassNode;
use Moose;
use strict;
extends 'OBO::Node';

use Moose::Util::TypeConstraints;

coerce 'OBO::ClassNode'
      => from 'Str'
      => via { new OBO::ClassNode(id=>$_) };

1;

=head2 Documentation

A class is a collection of instances. Classes can either be explicitly named (OBO::TermNode) or they can be logical boolean expressions (OBO::ClassExpression)

=cut

