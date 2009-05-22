package OBO::Phylo::Branch;
use Moose;
use strict;
extends 'OBO::LinkStatement';
use OBO::Evidence;

has distance => ( is=>'rw', isa=>'float');

=head1 NAME

OBO::Phylo::Branch

=head1 SYNOPSIS

  printf '%s --[%s]--> %s branchlen: %s', $s->node->id, $s->relation, $s->target->id,$s->distance ;

=head1 DESCRIPTION

An edge in a Phylogenetic tree. Extends OBO::LinkStatement with distances

=cut

1;
