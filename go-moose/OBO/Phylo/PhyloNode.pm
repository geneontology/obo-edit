package OBO::Phylo::PhyloNode;
use Moose;
use strict;
extends 'OBO::Node';

use Moose::Util::TypeConstraints;

coerce 'OBO::Phylo::PhyloNode'
    => from 'OBO::Node'
    => via { new OBO::Phylo::PhyloNode(represents=>$_) };

#has 'represents' => (is=>'ro', isa=>'OBO::Statement',handles=>qr/.*/);
has 'represents' => (is=>'ro', isa=>'OBO::Statement');
has 'parent' => (is=>'ro', isa=>'OBO::Phylo::PhyloNode');
has 'tree' => (is=>'ro', isa=>'OBO::Phylo::PhyloTree');

coerce 'OBO::PhyloNode'
      => from 'Str'
      => via { new OBO::PhyloNode(id=>$_) };

1;

=head1 NAME

OBO::Phylo::PhyloNode

=head1 SYNOPSIS

  printf '%s "%s"', $n->id, $n->label;

=head1 DESCRIPTION

An OBO::Node in a phylogenetic tree that represents some kind of evolvable entity

Note that the same entity (e.g. gene, species) can be present in
multiple OBO::Phylo::PhyloTree. It may have different parents in each.

This necessitates having a separate object to represent both (a) the
node in the tree, together with its hypothetical placements and (b)
the entity it represents. The 'represents' accessor links these.

=head2 TBD

The parent attribute can also be obtained from the tree
object. Redundancy? Frame-style vs axiom-style

=cut
