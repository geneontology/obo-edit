package OBO::Phylo::PhyloTree;
use Moose;
use strict;
extends 'OBO::Graph';

sub rooted {
}

1;

=head1 NAME

OBO::Phylo::PhyloTree

=head1 SYNOPSIS

=head1 DESCRIPTION

An OBO::Graph in which each node has at most 1 parents, and each node
is a OBO::Phylo::PhyloNode

=head1 SEE ALSO

OBO::Graph

=cut
