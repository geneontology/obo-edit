use Test;
plan tests => 6;
use strict;
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

use GOBO::Graph;
#use GOBO::Statement;
#use GOBO::LinkStatement;
#use GOBO::NegatedStatement;
#use GOBO::Node;
use GOBO::Parsers::OBOParserDispatchHash;

$ENV{VERBOSE} = 1;

my $parser = new GOBO::Parsers::OBOParserDispatchHash(file=>"t/data/obo_file.obo");
my $verbose = 1;
$parser->parse;

my $g = $parser->graph;

$Data::Dumper::Maxdepth = 2;
# print
# "node index: " . Dumper( $g->node_index )
# . "\nnode links for GO:0000004:\n" . join("\n", # @{$g->get_outgoing_ontology_links("GO:0000004")}) . "\n\nRemoving GO:0000004...\n\n\n";

my $t = $g->get_term("GO:0000004");
ok ($t->id eq 'GO:0000004');

#$g->node_index->remove_node( $g->noderef("GO:0000004") );
$g->remove_node( $t ); # NO CASCADE

#print
#"node index now: " . Dumper( $g->node_index )
#. "\nnode links for GO:0000004:\n" . join("\n", #@{$g->get_outgoing_ontology_links("GO:0000004")}) . "\nnode index after getting links for GO:0000004: " . Dumper( $g->node_index )
#. "\n";

ok ( ! $g->get_term("GO:0000004") );

ok ( scalar(@{$g->get_outgoing_ontology_links("GO:0000004")}) == 3);

$g->remove_node( "GO:0000007", 1); # WITH CASCADE

ok ( ! $g->get_term("GO:0000007") );

ok ( scalar(@{$g->get_outgoing_ontology_links("GO:0000007")}) == 0);

my $parser = new GOBO::Parsers::OBOParserDispatchHash( file => 't/data/so-xp.obo');
$parser->parse;
my $graph = $parser->graph;
print STDERR "n terms: " . scalar (@{$graph->terms}) . "\n";
print STDERR " statements: "
. scalar (@{$graph->statements}) . "\n edges: "
. scalar (@{$graph->edges}) . "\n ontology links: "
. scalar (@{$graph->ontology_links}) . "\n annotations: "
. scalar (@{$graph->annotations}) . "\n";

$graph->clear_all_ontology_links;
ok ( scalar @{$graph->ontology_links} == 0 );
print STDERR "links remaining: " . join("\n", @{$graph->statements}) . "\n";