use Test;
plan tests => 6;
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
    printf "inferring annotations for %s\n", scalar(@{$gafparser->graph->annotations});
    push(@ics, @{$ie->infer_annotations($gafparser->graph->annotations)});
    # clear
    $gafparser->graph(new OBO::Graph);
}

print "Inferred ICs:\n";
my $icgraph = new OBO::Graph();
$icgraph->annotations(\@ics);
my $w = new OBO::Writers::GAFWriter;
$w->graph($icgraph);
$w->write;

ok(@ics == 1);
# FAKE link added to ftp.obo GO:0005525 gtp binding --> GO:0046123-purine deoxyribonucleoside biosynthetic process
my $ic = shift @ics;
ok($ic->node->label eq '128up');
ok($ic->target->label eq 'purine deoxyribonucleoside biosynthetic process');
ok($ic->evidence->supporting_entities->[0]->id eq 'GO:0005525');
ok($ic->source->id eq 'GOC');

# now add it back in and attempt to do inference again..

$fh = new FileHandle("t/data/test-fb.gaf");
$gafparser = new OBO::Parsers::GAFParser(fh=>$fh);
my $agraph = $gafparser->graph;
$gafparser->parse;
printf "num annots orig: %d\n", scalar(@{$agraph->annotations});
print "Adding inferred annotation back:\n";
$agraph->add_annotation($ic);
printf "num annots orig: %d\n", scalar(@{$agraph->annotations});

print "Inferring ICs, second pass (expecting none):\n";
$ie = new OBO::InferenceEngine::GAFInferenceEngine(graph=>$ontg);
$ontg->add_annotations($agraph->annotations);
@ics = @{$ie->infer_annotations($gafparser->graph->annotations)};
$icgraph = new OBO::Graph();
$icgraph->annotations(\@ics);
my $w = new OBO::Writers::GAFWriter;
$w->graph($icgraph);
$w->write;
ok(@ics == 0);


