package OBO::Annotation;
use Moose;
use strict;
extends 'OBO::LinkStatement';
use OBO::Evidence;

# cardinality?
has evidence => ( is=>'rw', isa=>'OBO::Evidence');
has specific_node => ( is=>'rw', isa=>'OBO::Node');
has target_differentium_list => ( is=>'rw', isa=>'ArrayRef[OBO::LinkStatement]');

# alias. TBD - keep?
sub gene {
    shift->node(@_);
}

=head1 NAME

OBO::Annotation

=head1 SYNOPSIS

  printf '%s --[%s]--> %s evidence: %s', $s->node->id, $s->relation, $s->target->id,$s->evidence ;

=head1 DESCRIPTION

An OBO::LinkStatement that has OBO::Evidence attached

Annotations need not be stored in the main ontology OBO::Graph, but this is possible

=head2 Use in GO
 
In GO, annotations are also thought of as associations between genes
and OBO::TermNode objects. The statement is 'about' a gene, i.e. geneG
has_function termF, so the node points to a gene and the target points
to a OBO::TermNode

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
