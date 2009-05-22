package GOBO::Annotation;
use Moose;
use strict;
extends 'GOBO::LinkStatement';
use GOBO::Evidence;

# cardinality?
has evidence => ( is=>'rw', isa=>'GOBO::Evidence');
has specific_node => ( is=>'rw', isa=>'GOBO::Node');
has target_differentium_list => ( is=>'rw', isa=>'ArrayRef[GOBO::LinkStatement]');

# alias. TBD - keep?
sub gene {
    shift->node(@_);
}

=head1 NAME

GOBO::Annotation

=head1 SYNOPSIS

  printf '%s --[%s]--> %s evidence: %s', $s->node->id, $s->relation, $s->target->id,$s->evidence ;

=head1 DESCRIPTION

An GOBO::LinkStatement that has GOBO::Evidence attached

Annotations need not be stored in the main ontology GOBO::Graph, but this is possible

=head2 Use in GO
 
In GO, annotations are also thought of as associations between genes
and GOBO::TermNode objects. The statement is 'about' a gene, i.e. geneG
has_function termF, so the node points to a gene and the target points
to a GOBO::TermNode

The relation may be unassigned

=head2 Advanced

See http://wiki.geneontology.org/index.php/GAF_2.0

=head3 specific_node

In the GO GAF2.0 specification it's possible to enhance a gene
annotation by denoting the specific gene product that has the assigned
function. This is handled here via the specific_node accessor: e.g.

  printf 'gene: %s gene_product:%s has_function/location: %s', $a->node, $a->specific_node, $a->target;

=head3 target_differentium_list

In the GO GAF2.0 specification it's possible to enhance a gene
annotation by refining the target term using a list of relationships. See:

http://wiki.geneontology.org/index.php/Annotation_Cross_Products

=cut

1;
