package OBO::Writers::GAFWriter;
use Moose;
use strict;
extends 'OBO::Writers::Writer';
use OBO::Node;
use OBO::Gene;
use OBO::Evidence;
use OBO::Annotation;

sub write_header {
    my $self = shift;
    my $g = $self->graph;

    return;
}

sub write_body {
    my $self = shift;
    my $g = $self->graph;

    foreach my $ann (@{$g->annotations}) {
        my $gene = $ann->node;
        my $gene_product = $ann->specific_node;
        my @vals =
            (
             $gene->id_db,
             $gene->local_id,
             $gene->label,
             '',         # qualifier
             $ann->target->id,
             join('|',map { $_ } (@{$ann->provenance->xrefs || []}, $ann->provenance->id)),
             $ann->evidence->type->id,
             $ann->evidence->with_str, # with,
             _aspect($ann->target), # aspect
             '', # gene name
             $gene->type->id, # 
             $gene->taxon->id, #
             $ann->date_compact,
             $ann->source->id,
             '',
             $gene_product ? $gene_product->id : '');
        $self->printrow(\@vals);
    }
    return;
}

sub _aspect {
    my $ns = shift->namespace || '';
    if ($ns eq 'molecular_function') {'F'}
    elsif ($ns eq 'biological_process') {'P'}
    elsif ($ns eq 'cellular_component') {'C'}
    else {'-'}
    
}

1;
