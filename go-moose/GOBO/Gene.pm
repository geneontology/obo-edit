package GOBO::Gene;
use Moose;
use Moose::Util::TypeConstraints;
use GOBO::Types;
extends 'GOBO::AnnotationSubject';

has gp_type => (is=>'rw', isa=>'GOBO::Node', coerce=>1);
has taxon => (is=>'rw', isa=>'GOBO::Node', coerce=>1);

1;

=head1 NAME

GOBO::Gene

=head1 SYNOPSIS

  printf '%s "%s" %s %s', $n->id, $n->label, $n->gp_type, $n->taxon;

=head1 DESCRIPTION

An GOBO::Node that corresponds to a specific SO:gene

In GO, genes are the subject of GOBO::Annotation statements

=head2 TBD

This should really be in bioperl.

Modeled as nodes as they can be elements in a graph. cf Chado

=cut
