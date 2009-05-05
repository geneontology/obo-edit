package OBO::Node;
use Moose;
use strict;
with 'OBO::Identified';
with 'OBO::Labeled';
with 'OBO::Attributed';
use Moose::Util::TypeConstraints;

coerce 'OBO::Node'
      => from 'Str'
      => via { new OBO::Node(id=>$_) };

has 'source' => (is => 'rw', isa => 'OBO::Node');

use overload ('""' => 'as_string');
sub as_string {
    my $self = shift;
    if ($self->label) {
        return sprintf('%s "%s"',$self->id,$self->label);
    }
    if ($self->id) {
        return $self->id;
    }
    return $self;
}

1;

=head1 NAME

OBO::Node

=head1 SYNOPSIS

  printf '%s "%s"', $n->id, $n->label;

=head1 DESCRIPTION

 A unit in a graph. The Node class hierarchy:

 * OBO::ClassNode 
 ** OBO::TermNode 
 ** OBO::ClassExpression
 * OBO::RelationNode
 * OBO::InstanceNode

With a simple ontology graph, the core units are TermNodes.

=back

=head1 SEE ALSO

OBO::Graph

=cut



