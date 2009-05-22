package OBO::Gene;
use Moose;
use strict;
extends 'OBO::Node';
use OBO::Node;

has type => (is=>'rw', isa=>'OBO::Node', coerce=>1);
has taxon => (is=>'rw', isa=>'OBO::Node', coerce=>1);

1;

=head1 NAME

OBO::Gene

=head1 SYNOPSIS

  printf '%s "%s" %s %s', $n->id, $n->label, $n->type, $n->taxon;

=head1 DESCRIPTION

An OBO::Node that corresponds to a specific SO:gene

In GO, genes are the subject of OBO::Annotation statements

=head2 TBD

This should really be in bioperl.

Modeled as nodes as they can be elements in a graph. cf Chado

=cut
