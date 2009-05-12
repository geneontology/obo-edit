#!/usr/bin/perl
use strict;
use OBO::Graph;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::Annotation;
use OBO::Node;
use OBO::Parsers::GAFParser;
use OBO::Parsers::OBOParser;
use OBO::Writers::GAFWriter;
use OBO::InferenceEngine;
use OBO::InferenceEngine::GAFInferenceEngine;
use DateTime;
use FileHandle;

my $ontf;
my %relh = ();
my $per_file_ic=0;
while ($ARGV[0] =~ /^\-/) {
    my $opt = shift @ARGV;
    if ($opt eq '-i' || $opt eq '--ontology') {
        $ontf = shift;
    }
    elsif ($opt eq '-r' || $opt eq '--relation') {
        $relh{shift @ARGV} = 1;
    }
    elsif ($opt eq '--per-file') {
        $per_file_ic = 1;
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
my $ie = new OBO::InferenceEngine::GAFInferenceEngine(graph=>$ontg);

my %nodemap = ();

# iterate through annotations writing new ICs
foreach my $f (@ARGV) {
    my @ics = ();
    $ontg->annotations([]);
    my $gafparser = new OBO::Parsers::GAFParser(file=>$f);
    # iterate through one chunk at a time
    while ($gafparser->parse_chunk(10000)) {
        printf STDERR "processing %d annots in in $f\n", scalar(@{$gafparser->graph->annotations});
        $ontg->add_annotations($gafparser->graph->annotations);
        printf STDERR "  ontg annots %d\n", scalar(@{$ontg->annotations});
        push(@ics, @{$ie->infer_annotations($gafparser->graph->annotations)});
        # clear
        printf STDERR "  inferences %d\n", scalar(@ics);
        $gafparser->graph(new OBO::Graph);
    }
    my $icgraph = new OBO::Graph();
    $icgraph->annotations(\@ics);
    my $w = new OBO::Writers::GAFWriter;
    if ($per_file_ic) {
        my $of = $f;
        $of =~ s/.*\///g;
        $of =~ s/\.gz//;
        $of .= ".ics.gaf";
        $w->file($of);
        $w->init_fh;
    }
    $w->graph($icgraph);
    $w->write;
}
exit 0;


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
