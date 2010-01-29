use Test::More;
plan tests => 10;
use strict;
use GOBO::Graph;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::OBOParserDispatchHash;
use GOBO::Writers::OBOWriter;
use GOBO::InferenceEngine;
#use GOBO::InferenceEngine::CustomEngine;
use FileHandle;
use Data::Dumper;

use Storable;

my $fh = new FileHandle("t/data/cell.obo");
my $parser = new GOBO::Parsers::OBOParser(fh=>$fh);
$parser->parse;
my $g = $parser->graph;

my $ie = new GOBO::InferenceEngine(graph=>$g);
$ie->save_ix('inf_engine_test');
my $neuron = $g->noderef('CL:0000540');

printf "neuron = $neuron\n";
my $develops_from = $g->relation_noderef('develops_from');

printf "df = $develops_from\n";

# 1
ok($develops_from->transitive, "develops from is transitive");
# 2
ok($develops_from->propagates_over_is_a, "develops from propagates over is a");
my $result = $ie->relation_composition($develops_from, $develops_from);
#print "$develops_from . $develops_from => $result, " .ref($result) . "\n";

# 3
ok( ($result->id eq 'develops_from' && ref($result) eq 'GOBO::RelationNode'), "$develops_from . $develops_from => GOBO::RelationNode develops from");

my $xlinks =
    $ie->extend_link(link =>
        new GOBO::LinkStatement(node=>'CL:0000540',
                               relation=>$develops_from,
                               target=>'CL:0000047'));

foreach (@$xlinks) {
    printf "x: $_\n";
}

# 4 - 6
check();

printf "cached links: %d\n", scalar(@{$ie->graph->get_all_statements_in_ix('inf_engine_test')});
print "trying again (should be cached)\n";
# 7 - 9
check();

sub check {

    my $links = $g->get_outgoing_ontology_links($neuron);
    foreach (@$links) {
        printf "asserted: $_ [REL=%s t:%s]\n", $_->relation, ( $_->relation->transitive || '0' );
    }
    $links = $ie->get_inferred_outgoing_edges(node=>$neuron);
    foreach (@$links) {
        printf "inferred: $_\n";
    }
    ok(@$links > 0, "n links > 0");
# "link xxx develops_from CL:0000031 exists"
    ok(grep {$_->matches(relation=>'develops_from', target=>'CL:0000031')} @$links);
# "link xxx develops_from CL:0000133 exists"
    ok(grep {$_->matches(relation=>'develops_from', target=>'CL:0000133')} @$links);
}

my $clear = `clear`;
print $clear;
$parser = new GOBO::Parsers::OBOParser(file=>"t/data/obo_file.obo");
$parser->parse;
$g = $parser->graph;
my $g1 = Storable::dclone $g;
my $g2 = Storable::dclone $g;
$ie = new GOBO::InferenceEngine(graph=>$g1);
foreach (@{$ie->graph->terms})
{	$ie->get_inferred_outgoing_edges(node=>$_);
}
$g1 = $ie->graph;

$ie = new GOBO::InferenceEngine(graph=>$g2);
foreach (@{$ie->graph->terms})
{	$ie->get_inferred_incoming_edges(target=>$_);
}
$g2 = $ie->graph;
## check we get the same results from reasoning up or reasoning down the graph
ok( join("||", sort { $a->node->id cmp $b->node->id || $a->relation->id cmp $b->relation->id || $a->target->id cmp $b->target->id } @{$g1->statements}) eq join("||", sort { $a->node->id cmp $b->node->id || $a->relation->id cmp $b->relation->id || $a->target->id cmp $b->target->id } @{$g2->statements}));


$parser = new GOBO::Parsers::OBOParserDispatchHash( file => 't/data/slimmer_test_3.obo');
$parser->parse;
$g1 = $parser->graph;
$ie = new GOBO::InferenceEngine(graph=>$g1);
$ie->__create_edge_matrix(from_ix=>'statements', save_ix => 'generated', input_ids => [ map { $_->id } @{$g1->terms} ] );
$ie->__trim_edge_matrix;




exit(0);

## let's checking the fetching of cached links works correctly...
$g1 = Storable::dclone $g;
$ie = new GOBO::InferenceEngine(graph=>$g1);

sleep 5;

$ie->get_inferred_outgoing_edges(node=>$g1->get_term('GO:0000008'));

exit(0);

$ie->get_inferred_outgoing_edges(node=> $g1->get_term('GO:0000013') );
$ie->get_inferred_outgoing_edges(node=> $g1->get_term('GO:0000015') );
#print STDERR "child links: " . join("\n", @kids) . "\n";

my @more_kids = sort map { $_->as_string } @{$ie->get_inferred_outgoing_edges( node => $g1->get_term('GO:0000004') ) };
print STDERR "kids:\n" . join("\n", @more_kids) . "\n\n";

$g2 = Storable::dclone $g;
$ie = new GOBO::InferenceEngine( graph => $g2 );
$ie->use_cache(0);

my @more_kids2 = sort map { $_->as_string } @{$ie->get_inferred_outgoing_edges( node=>$g2->get_term('GO:0000004'))};

print STDERR "kids, no caching:\n" . join("\n", @more_kids2) . "\n\n";



exit(0);


$ie = new GOBO::InferenceEngine( graph => $g );
#my $ie->generate_simple_combined_rel_h;

print STDERR "Combined rel_h:\n";
$ie->print_relation_composition;
print STDERR "indir regs: " . Dumper($ie->graph->get_relation('indirectly_regulates')) . "\n";

=cut

$ie->__create_edge_matrix;
$ie->__populate_all_edge_matrices;
print STDERR "edge matrix:\n";
$ie->dump_edge_matrix('N_T_R');
print STDERR "\n";

$ie->__remove_redundant_relationships;
$ie->__populate_all_edge_matrices;
print STDERR "edge matrix now:\n";
$ie->dump_edge_matrix('N_T_R');
print STDERR "\n";
$ie->__trim_edge_matrix;
$ie->dump_edge_matrix('trimmed');


=cut
#my $gene = new GOBO::Gene( label => 'test_gene', id => 'geneious' );
#$g->add_node($gene);


=cut

## OK, let's test the custom engine a bit...
my $link_h;
$parser = new GOBO::Parsers::OBOParserDispatchHash(file=>'t/data/cell.obo');
$parser->parse;
my $graph = $parser->graph;

$link_h->{inf_outgoing} = get_inferred_outgoing_links( $graph );
$link_h->{inf_incoming} = get_inferred_incoming_links( $graph );
$link_h->{inf_graph} = get_inferred_graph( $graph );
$link_h->{inf_graph_leaves} = get_inferred_graph_leaves( $graph );
$link_h->{inf_outgoing_old} = get_inferred_outgoing_links_old( $graph );

my %tally;
foreach my $x qw( inf_outgoing inf_incoming inf_graph inf_graph_leaves inf_outgoing_old)
{	map { $tally{ $_->node->id . "." . $_->relation->id . "." . $_->target->id } .= " $x"; } @{$link_h->{$x}->links};
}

foreach (sort keys %tally)
{	if ($tally{$_} ne " inf_outgoing inf_incoming inf_graph inf_graph_leaves inf_outgoing_old")
	{	print STDERR "$_: " . $tally{$_} . "\n";
	}
	else
	{	delete $tally{$_};
#		print STDERR "$_\n";
	}
}

ok( ! defined keys %tally, "Checking all forms of graph inference work" );
if (scalar keys %tally > 0)
{	print STDERR "Error with the following links:\n" . join("\n", keys %tally) . "\n";
}

sub get_inferred_outgoing_links {
	my $graph = shift;
	print STDERR "starting get_inferred_outgoing_links...\n";
	## testing get_inferred_incoming_links
	my $ie = new GOBO::InferenceEngine::CustomEngine( graph => $graph );
	foreach my $t (@{$graph->terms})
	{	# get all the links between the input nodes and those in the subset
		$ie->get_inferred_outgoing_links($t);
	}
	return $ie->inferred_graph;
}


sub get_inferred_incoming_links {
	my $graph = shift;
	print STDERR "starting get_inferred_incoming_links...\n";
	## testing get_inferred_incoming_links
	my $ie = new GOBO::InferenceEngine::CustomEngine( graph => $graph );
	foreach my $t (@{$graph->terms})
	{	# get all the links between the input nodes and those in the subset
		$ie->get_inferred_incoming_links($t);
	}
	return $ie->inferred_graph;
}

sub get_inferred_outgoing_links_old {
	my $graph = shift;
	## testing get_inferred_incoming_links
	my $ie = new GOBO::InferenceEngine( graph => $graph );
	foreach my $t (@{$graph->terms})
	{	# get all the links between the input nodes and those in the subset
		$ie->get_inferred_outgoing_links($t);
	}
	return $ie->inferred_graph;
}

sub get_inferred_graph {
	my $graph = shift;
	print STDERR "starting get_inferred_graph...\n";
	## testing get_inferred_incoming_links
	my $ie = new GOBO::InferenceEngine::CustomEngine( graph => $graph );
	$ie->get_inferred_graph;
	return $ie->inferred_graph;
}

sub get_inferred_graph_leaves {
	my $graph = shift;
	print STDERR "starting get_inferred_graph...\n";
	## testing get_inferred_incoming_links
	my $ie = new GOBO::InferenceEngine::CustomEngine( graph => $graph );
	$ie->get_inferred_graph('from_leaves');
	return $ie->inferred_graph;
}





#use Moose::Autobox;
# print 'Print squares from 1 to 10 : ';
#  print [ 1 .. 10 ]->map(sub { $_ * $_ })->join(', ');
