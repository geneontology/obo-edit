package GOBO::Writers::PerlHashWriter;
use Moose;
use strict;
extends 'GOBO::Writers::Writer';
use GOBO::Node;
use GOBO::LinkStatement;

has hashref => (is=>'rw', isa=>'HashRef', default=>sub{{}} );

sub write_header {
    my $self = shift;
    return;
}


sub write_body {
    my $self = shift;
    my $g = $self->graph;
    foreach my $term (@{$g->terms}) {
        $self->write_stanza($term);
    }
    foreach my $relation (@{$g->relations}) {
        $self->write_stanza($relation);
    }
    foreach my $ann (@{$g->annotations}) {
        $self->write_annotation_stanza($ann);
    }
    # TODO: instances
    return;
}

sub write_stanza {
    my $self = shift;
    my $node = shift;
    my $g = $self->graph;

    my $doc = {};

    $self->nl;
    my $type = 'instance';
    if ($node->isa('GOBO::TermNode')) {
        $type = 'class';
    }
    elsif ($node->isa('GOBO::RelationNode')) {
        $type = 'relation';
    }
    elsif ($node->isa('GOBO::Annotation')) {
        $type = 'annotation';
    }
    $doc->{type} = $type;
    
    $doc->{id}=$node->id;
    $doc->{name}=$node->label if $node->label;
    $doc->{namespace}=$node->namespace if $node->namespace;
    $doc->{alt_ids} = $node->alt_ids if $node->alt_ids;
    if ($node->definition) {
        $doc->{definition} = $node->definition;
        $doc->{definition_xrefs} = [map {$_->id} @{$node->definition_xrefs || []}];
    }
    $doc->{comment}=$node->comment if $node->comment;
    $doc->{subsets} = [map {$_->id} @{$node->subsets || []}];
    $doc->{synonyms} = 
        map {
            (scope=>$_->scope,
             text=>$_->label,
             xrefs => [map {$_->id} @{$_->xrefs || []}])
    } @{$node->synonyms || []}; 

    # xref

    if ($node->isa('GOBO::RelationNode')) {
        $doc->{domain}= $node->domain;
        $doc->{range}= $node->range;
        foreach (GOBO::RelationNode->unary_property_names) {
            $doc->{$_} = 1 if $node->$_();
        }
        #$doc->{holds_over_chain}= _chain($_)) foreach @{$node->holds_over_chain_list || []};
        #$doc->{equivalent_to_chain}= _chain($_)) foreach @{$node->equivalent_to_chain_list || []};
    }

    my @isas = ();
    my @rels = ();
    foreach (@{$g->get_target_links($node)}) {
        if ($_->is_intersection) {
        }
        else {
            if ($_->relation->is_subsumption) {
                push(@isas, $_->target->id);
            }
            else {
                push(@rels,
                     {relation=>$_->relation->id,
                      target=>$_->target->id});
            }
        }
    }
    $doc->{is_as} = \@isas;
    $doc->{all_some_relationships} = \@rels;
    # TODO - logical definition
    $self->hashref->{$doc->{id}} = $doc;
    return $doc;
}


sub write_annotation_stanza {
    my $self = shift;
    my $ann = shift;
    my $g = $self->graph;
    my $doc;
    $self->nl;
    my $stanzaclass = 'Annotation';
    
    $self->open_stanza($stanzaclass);
    $doc->{id}=$ann->id if $ann->id;  # annotations need not have an ID
    $self->tagval(subject=>$ann->node->id);
    $self->tagval(relation=>$ann->relation->id);
    $self->tagval(object=>$ann->target->id);
    $self->tagval(description=>$ann->description);
    $self->tagval(source=>$ann->provenance->id) if $ann->provenance;
    $self->tagval(assigned_by=>$ann->source->id) if $ann->source;
    return;
}




1;
