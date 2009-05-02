#!/usr/bin/perl

use OBO::Graph;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::Annotation;
use OBO::Node;
use OBO::Parsers::GAFParser;
use OBO::Parsers::OBOParser;
use OBO::Writers::GAFWriter;
use OBO::InferenceEngine;
use DateTime;
use FileHandle;

my $ontf;
my %relh = ();
while ($ARGV[0] =~ /^\-/) {
    my $opt = shift @ARGV;
    if ($opt eq '-i' || $opt eq '--ontology') {
        $ontf = shift;
    }
    elsif ($opt eq '-r' || $opt eq '--relation') {
        $relh{shift @ARGV} = 1;
    }
    elsif ($opt eq '-h' || $opt eq '--help') {
        system("perldoc $0");
        exit(0);
    }
    else {
        die "no such opt: $opt";
    }
}
if (!$ontf) {
    $ontf = shift;
}
if (!$ontf) {
    system("perldoc $0");
    exit(1);
}

my $obo_parser = new OBO::Parsers::OBOParser(file=>$ontf);
$obo_parser->parse;
my $ontg = $obo_parser->graph;
my $ie = new OBO::InferenceEngine(graph=>$ontg);

my %nodemap = ();

# iterate through annotations writing new ICs
my @ics = ();
my $got_h = ();
foreach my $f (@ARGV) {
    my $parser = new OBO::Parsers::GAFParser(file=>$f);
    # iterate through one chunk at a time
    while ($parser->parse_chunk(10000)) {
        push(@ics, infer_annotations($parser->graph->annotations));
        $parser->graph(new OBO::Graph);
    }
    my $icgraph = new OBO::Graph();
    my @ic_anns =
        map {
            new OBO::Annotation(node => $_->[0],
                                target => $ontg->term_noderef($_->[1]),
                                provenance => $_->[2],
                                evidence => new OBO::Evidence(type=>$ontg->term_noderef('IC'),
                                                              supporting_entities => [$_->[1]]),
                                source=>'GOC',
                                date=>DateTime->today)
    } @ics;
    $icgraph->annotations(\@ic_anns);
    my $w = new OBO::Writers::GAFWriter;
    $w->graph($icgraph);
    $w->write;
}
exit 0;

sub infer_annotations {
    my $anns = shift;
    my @ics = ();
    foreach my $ann (@$anns) {
        my $t = $ann->target;
        my $tid = $t->id;
        my $t_ns = $t->namespace;
        my $gene = $ann->node;
        if (!$nodemap{$tid}) {
            print STDERR "building nodemap for $tid\n";
            my $xlinks = $ie->get_inferred_target_links($t);
            my %candidate_h = ();
            foreach my $xlink (@$xlinks) {
                next unless $xlink->relation->id eq 'part_of';
                next unless $xlink->target->namespace ne $t_ns;
                $candidate_h{$xlink->target->id} = 1;
                print STDERR " xlink: $xlink\n";
            }
            printf STDERR " candidates for $tid: %s\n", join('; ', keys %candidate_h);
            
            $nodemap{$tid} =
                $ie->get_nonredundant_set([keys %candidate_h]);
        }
        if (@{$nodemap{$tid}}) {
            if (!$got_h{$gene}{$t}) {
                push(@ics, map {[$gene,$_,$ann->provenance]} @{$nodemap{$tid}});
                #printf STDERR "$gene $t\n";
            }
            $got_h{$gene}{$t} = 1;
        }
        
    }
    return @ics;
}

# find 
sub calculate_inference_graph {
    my $graph = shift;
    my $igraph = new OBO::Graph;
    
}




=head1 NAME

go-gaf-inference.pl

=head1 SYNOPSIS

 go-gaf-inference.pl go/ontology/obo_format_1_2/gene_ontology_ext.obo go/gene-associations/gene_association.sgd.gz

=head1 DESCRIPTION

Performs inference upon a GAF (Gene Association File), generating ICs based on configurable criteria

=cut
