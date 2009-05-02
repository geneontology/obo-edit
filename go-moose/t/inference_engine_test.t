use Test;
use strict;
use OBO::Graph;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::NegatedStatement;
use OBO::Node;
use OBO::Parsers::OBOParser;
use OBO::Writers::OBOWriter;
use OBO::InferenceEngine;
use FileHandle;


my $fh = new FileHandle("t/data/cell.obo");
my $parser = new OBO::Parsers::OBOParser(fh=>$fh);
$parser->parse;
my $g = $parser->graph;

my $ie = new OBO::InferenceEngine(graph=>$g);
my $neuron = $g->noderef('CL:0000540');
#my $links = $ie->backward_chain($neuron);
my $links = $ie->get_inferred_target_links($neuron);
foreach (@$links) {
    printf "inferred: $_\n";
}

printf "cached links: %d\n", scalar(@{$ie->inferred_graph->links});
print "trying again (should be cached)\n";
$links = $ie->get_inferred_target_links($neuron);
foreach (@$links) {
    printf "inferred: $_\n";
}

#use Moose::Autobox;
# print 'Print squares from 1 to 10 : ';
#  print [ 1 .. 10 ]->map(sub { $_ * $_ })->join(', ');
