package OBO::TermNode;
use Moose;
use strict;
extends 'OBO::ClassNode';
with 'OBO::Definable';

1;

=head1 NAME

OBO::TermNode

=head1 SYNOPSIS

  printf '%s "%s" def: "%s"', $n->id, $n->label, $n->definition;

=head1 DESCRIPTION

Core element in an ontology. 

=head1 SEE ALSO

OBO::Graph

=cut


