=head1 NAME

GOBO::Analysis::AnalysisEngine

=head1 SYNOPSIS


=head1 DESCRIPTION


=head2 Rules


=cut

package GOBO::Analysis::AnalysisEngine;
use Moose;
extends 'GOBO::InferenceEngine::GAFInferenceEngine';
use strict;
use GOBO::Statement;
use GOBO::Annotation;
use GOBO::Graph;
use GOBO::Node;
use GOBO::TermNode;
use GOBO::RelationNode;
use Math::GSL;

has feature_index_map => (is=>'rw', isa=> 'HashRef[Int]', default=>sub{{}});
has attribute_index_map => (is=>'rw', isa=> 'HashRef[Int]', default=>sub{{}});

sub init_attribute_index_map {
    my $self = shift;
    my $amap = $self->attribute_index_map;
    my $inc = 0;
    my %nidh = ();
    foreach my $ann (@{$self->graph->annotations}) {
        foreach my $n ($self->get_inferred_target_nodes($ann->target)) {
            $nidh{$n->id} = 1;
        }
    }
    foreach my $nid (keys %nidh) {
        $amap->{$nid} = $inc++;
    }
    return;
}

sub init_feature_index_map {
    my $self = shift;
    my $amap = $self->feature_index_map;
    my $incnode->id = 0;
    foreach my $node (@{$self->graph->annotated_entities}) {
        $amap->{$node->id} = $inc++;
    }
    return;
}



1;

=head1 SEE ALSO

  bin/go-gaf-inference.pl

=cut
