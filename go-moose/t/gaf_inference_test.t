use Test;
plan tests => 4;
use strict;
use OBO::Graph;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::NegatedStatement;
use OBO::Node;
use OBO::Parsers::OBOParser;
use OBO::Parsers::GAFParser;
use OBO::Writers::GAFWriter;
use OBO::InferenceEngine::GAFInferenceEngine;
use FileHandle;

my $ontf = "t/data/gtp.obo";
my $obo_parser = new OBO::Parsers::OBOParser(file=>$ontf);
$obo_parser->parse;
my $ontg = $obo_parser->graph;
my $ie = new OBO::InferenceEngine::GAFInferenceEngine(graph=>$ontg);

my $fh = new FileHandle("t/data/test-fb.gaf");
my $gafparser = new OBO::Parsers::GAFParser(fh=>$fh);

my @ics = ();
while ($gafparser->parse_chunk(10000)) {
    push(@ics, @{$ie->infer_annotations($gafparser->graph->annotations)});
    # clear
    $gafparser->graph(new OBO::Graph);
}
my $icgraph = new OBO::Graph();
$icgraph->annotations(\@ics);
ok(@ics == 1);
my $w = new OBO::Writers::GAFWriter;
$w->graph($icgraph);
$w->write;
