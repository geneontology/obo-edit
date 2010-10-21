#use Test;
use Test::More;
use Test::Deep;
use Test::Output;
use Test::File;

plan tests => 5;
use strict;
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

use GOBO::Graph;
use GOBO::Util::GraphFunctions;
use GOBO::Parsers::OBOParserDispatchHash;
use GOBO::InferenceEngine;
use GOBO::InferenceEngine::CustomEngine;
use GOBO::Writers::TreeWriter;
use FileHandle;

use Storable qw(dclone);

my $data;
my $ss;
my $fh = new FileHandle("t/data/relation_test.obo");
my $parser = new GOBO::Parsers::OBOParserDispatchHash(fh=>$fh);
$parser->parse;

my $other_h = GOBO::Util::GraphFunctions::create_bucket_terms(graph => $parser->graph, ix => 'ontology_links', subset_ids => [ $parser->graph->get_term('GO:0000001'), $parser->graph->get_term('GO:0000002'), $parser->graph->get_term('GO:0000004') ]);

## we should have key: GO:0000000, value: GO:0000003 OR GO:0000005 OR GO:0000006
my $expr;
foreach (values %$other_h)
{	$expr = $_->{class_expr};
}

$expr->arguments( [ sort { $a->id cmp $b->id } @{$expr->arguments} ] );
ok( join(", ", keys %$other_h) eq 'GO:0000000', "Checking expression parent" );
ok( $expr->as_string eq 'NOT (GO:0000001 OR GO:0000002 OR GO:0000004)', "Checking expression" );

print STDERR "reading in t/data/bucket_test.obo...\n";
$fh = new FileHandle("t/data/bucket_test.obo");
$parser = new GOBO::Parsers::OBOParserDispatchHash(fh=>$fh);
$parser->parse;
$data = GOBO::Util::GraphFunctions::get_subset_nodes(graph=>$parser->graph, options => { subset => [ 'goslim_test' ] } );
$ss = [ keys %{$data->{subset}{goslim_test}} ];

#$other_h = GOBO::Util::GraphFunctions::create_bucket_terms(graph => $parser->graph, ix => 'ontology_links', subset_ids => $ss);

my $results = GOBO::Util::GraphFunctions::add_bucket_terms_to_graph(graph=>dclone $parser->graph, ix=>'ontology_links', subset_ids => $ss);

ok( grep { $_ eq '-(GO:0000005|GO:0000004)' } @{$results->{subset_ids}}, "Checking for the complement term");

$fh = new FileHandle("t/data/bucket_test.obo");
$parser = new GOBO::Parsers::OBOParserDispatchHash(fh=>$fh);
$parser->parse;
$data = GOBO::Util::GraphFunctions::get_subset_nodes(graph=>$parser->graph, options => { subset => [ 'goslim_test' ] } );


## Make sure that subset_ids and subset give the same results
system ('rm', 't/data/test_output.txt', 't/data/test_output_1.txt');
my $slimmed = GOBO::Util::GraphFunctions::slim_annotations(options =>  { ga_input => 't/data/tiny_gaf.gaf' }, subset_ids=> [ keys %{$data->{subset}{goslim_test}} ], graph => dclone $parser->graph );

my $writer = new GOBO::Writers::TreeWriter( graph=>$slimmed->{graph}, show_gp_counts => 1, ontology_link_ix => 'direct_ontology_links', annotation_ix => 'all_annotations', file => 't/data/test_output.txt' );
$writer->write;

my $slimmed_1 = GOBO::Util::GraphFunctions::slim_annotations(options =>  { ga_input => 't/data/tiny_gaf.gaf' }, subset => 'goslim_test', graph => dclone $parser->graph );

$writer = new GOBO::Writers::TreeWriter( graph=>$slimmed_1->{graph}, show_gp_counts => 1, ontology_link_ix => 'direct_ontology_links', annotation_ix => 'all_annotations', file => 't/data/test_output_1.txt' );
$writer->write;

my $status = `diff t/data/test_output.txt t/data/test_output_1.txt`;

ok( ! $status, "Checking subset_ids vs subset options");


## as above, but this time with buckets enabled.
system ('rm', 't/data/test_output.txt', 't/data/test_output_1.txt');

$results = GOBO::Util::GraphFunctions::add_bucket_terms_to_graph(graph=>dclone $parser->graph, ix=>'ontology_links', subset_ids => $ss);
$slimmed = GOBO::Util::GraphFunctions::slim_annotations(options =>  { ga_input => 't/data/tiny_gaf.gaf' }, subset_ids=> $results->{subset_ids}, graph => $results->{graph} );
$writer = new GOBO::Writers::TreeWriter( graph=>$slimmed->{graph}, show_gp_counts => 1, ontology_link_ix => 'direct_ontology_links', annotation_ix => 'all_annotations', file => 't/data/test_output.txt' );
$writer->write;

$slimmed_1 = GOBO::Util::GraphFunctions::slim_annotations(options =>  { ga_input => 't/data/tiny_gaf.gaf', buckets => 1 }, subset => 'goslim_test', graph => dclone $parser->graph );

$writer = new GOBO::Writers::TreeWriter( graph=>$slimmed_1->{graph}, show_gp_counts => 1, ontology_link_ix => 'direct_ontology_links', annotation_ix => 'all_annotations', file => 't/data/test_output_1.txt' );
$writer->write;

$status = `diff t/data/test_output.txt t/data/test_output_1.txt`;

ok( ! $status, "Checking bucket mode switch works");

system ('rm', 't/data/test_output.txt', 't/data/test_output_1.txt');

exit(0);


=cut

my $graph = $slimmed_1->{graph};

$writer->show_gp_names(1);
$writer->write;

$writer->show_gp_names(0);

## ok, let's do the same test but this time using the built-in bucket mode

$parser->reset_parser;
$parser->set_fh($fh);
$parser->parse;

my $slimmed_2 = GOBO::Util::GraphFunctions::slim_annotations(options => { ga_input=>'t/data/tiny_gaf.gaf', buckets => 1 }, subset => 'goslim_test', graph => $parser->graph );

$writer = new GOBO::Writers::TreeWriter( graph => $slimmed_2->{graph}, show_gp_counts => 1, ontology_link_ix => 'direct_ontology_links', annotation_ix => 'all_annotations' );
$writer->write;



exit(0);
$parser->reset_parser;
$parser->set_fh("t/data/obo_file.obo");
$parser->parse;

$data = GOBO::Util::GraphFunctions::get_subset_nodes(graph=>$parser->graph, options => { subset => [ 'test_goslim' ] } );

$ss = [ keys %{$data->{subset}{test_goslim}} ];

$other_h = GOBO::Util::GraphFunctions::create_bucket_terms(graph =>$parser->graph, ix=>'ontology_links', subset_ids => $ss);

#print STDERR "other h now: " . Dumper($other_h) . "\n";
print STDERR "statements:\n" . join("\n", @{$parser->graph->statements}) . "\n\n";
$parser->graph->duplicate_statement_ix('ontology_links', 'asserted_links');

my $ie = new GOBO::InferenceEngine( graph => $parser->graph);

my @arr;
foreach (keys %$other_h)
{	print STDERR "looking at: " . join(", ",  (%{$other_h->{$_}}) ) . "\n\n";
	print STDERR "id: " . $other_h->{$_}{class_expr}->id . "\n";

	$other_h->{$_}{class_expr}->add_subsets( 'test_goslim' );
	$other_h->{$_}{class_expr}->label( $other_h->{$_}{class_expr}->as_string );
	$ie->graph->add_node( $other_h->{$_}{class_expr} );
	my $l = $ie->create_link_statement( node => $other_h->{$_}{class_expr}, relation => $ie->graph->noderef('is_a'), target => $ie->graph->noderef( $_) );
	print STDERR "Added $l\n";
	push @arr, $l;
	## remove the existing links
	foreach my $x (@{$other_h->{$_}{replaces}})
	{
		my $l2 = $ie->create_link_statement( node => $ie->graph->noderef( $x->node->id ), relation => $ie->graph->noderef( $x->relation->id ), target => $other_h->{$_}{class_expr} );
		print STDERR "replacing $x with $l2\n";
		push @arr, $l2;
	}
	print STDERR "statements to remove:\n" . join("\n", @{$other_h->{$_}{replaces}}) . "\n\n";
	$ie->graph->remove_statements( $other_h->{$_}{replaces} );
	push @$ss, $other_h->{$_}{class_expr}->id;
}
foreach (@arr)
{	print STDERR "adding: $_\n";
}

$ie->graph->add_statements_to_ix(ix=>'asserted_links', statements=>\@arr);

#undef $g2;

$parser->reset_parser;
$parser->set_fh("t/data/obo_file.obo");
$parser->parse;

$data = GOBO::Util::GraphFunctions::get_subset_nodes(graph=>$parser->graph, options => { subset => [ 'test_goslim' ] } );

$ss = [ keys %{$data->{subset}{test_goslim}} ];

$data =
GOBO::Util::GraphFunctions::add_bucket_terms_to_graph(graph =>$parser->graph, ix=>'ontology_links', subset_ids => $ss);

print STDERR "graph subset terms: " . join(", ", sort @{$data->{subset_ids}}) . "\n";
print STDERR "ont links:\n" . join("\n", @{$data->{graph}->ontology_links}) . "\n\n";

#my $writer = new GOBO::Writers::TreeWriter( graph => $data->{graph}, ontology_link_ix => 'inferred_links' );
#$writer->write;







exit(0);

## clone the graph
$ie = new GOBO::InferenceEngine::CustomEngine(graph => $ie->graph);

# slim down the graph...
# in these slims, the input set is the same as the mapping set
$ie->slim_graph( subset_ids => $ss, input_ids => $ss, from_ix => 'asserted_links', save_ix => 'inferred_links', options => { return_as_graph => 1 });

my $roots = $ie->graph->get_roots;
print STDERR "roots: " . join(", ", sort { $a->id cmp $b->id } @$roots ) . "\n";

print STDERR "statements in inferred graph:\n" . join("\n", @{$ie->graph->get_all_statements_in_ix('inferred_links')} ) . "\n\n";

$writer = new GOBO::Writers::TreeWriter( graph => $ie->graph, ontology_link_ix => 'inferred_links' );
$writer->write;


exit(0);

my $g = dclone($parser->graph);

$ENV{VERBOSE} = 1;
my $link = new GOBO::LinkStatement( node => 'GO:0000044', relation => 'negatively_regulates', target => 'GO:0000004');
stderr_like(sub { $g->add_ontology_link($link) }, qr/^Link .*? already exists/, "Checking duplicate link can't be added");

$g->add_term('GO:lonely_term');

my $orphans = $g->get_orphan_terms;
ok( scalar @$orphans == 1 && $orphans->[0]->id eq "GO:lonely_term", "Checking for orphan term" );

$g->add_term('GO:another_lonely_term');

$orphans = $g->get_orphan_terms;

ok( scalar @$orphans == 2 && $orphans->[0]->id eq "GO:lonely_term" && $orphans->[1]->id eq "GO:another_lonely_term", "Checking for orphan terms" );

$link = new GOBO::LinkStatement( node => $g->get_term('GO:lonely_term'), target => $g->get_term('GO:another_lonely_term'), relation => 'is_akin_to' );

$g->add_ontology_link($link);
#undef $ENV{VERBOSE};

#exit(0);
#print STDERR "graph: " . $g->dump(4);
#print STDERR "ontology links: " . join("\n", @{$g->ontology_links}) . "\n\n";

$orphans = $g->get_orphan_terms;

#print STDERR "orphans: " . Dumper($orphans) . "\n\n";
ok(ref($orphans) eq 'ARRAY' && scalar @$orphans == 0, "No orphans found after adding links");


$link = new GOBO::LinkStatement( node => 'GO:terminal', target => 'GO:terminal_2', relation => 'very_much_like' );
## clear existing links with $g->links
my $stts = $g->statements($link);

ok(scalar @$stts == 1, "Checking we can reset the links as planned");

$parser = new GOBO::Parsers::OBOParserDispatchHash( file => 't/data/inf_engine_test.obo' );
$parser->parse;
$g = new GOBO::Graph;

GOBO::Util::GraphFunctions::copy_attributes( from => $parser->graph, to => $g );

is_deeply( $parser->graph, $g, "Checking graphs are the same");

my $inf_eng = new GOBO::InferenceEngine( graph=>$g );
my $link_h;

print STDERR "starting to backward chain...\n";

print STDERR "statements: " . join("\n", @{$inf_eng->graph->statements}) . "\n";

$g->duplicate_statement_ix('ontology_links', 'asserted_links');

foreach (@{$g->terms})
{	$inf_eng->get_inferred_outgoing_ontology_links(node_id => $_->id, from_ix => 'ontology_links', save_ix => 'inferred_links');
}

print STDERR "finished backward chaining...\n";

print STDERR "statements: " . join("\n", @{$inf_eng->graph->get_all_statements_in_ix( ix=>'inferred_links') }) . "\n";

map { $link_h->{$_}++ } @{$inf_eng->graph->get_all_statements_in_ix( ix => 'inferred_links' ) };

map { delete $link_h->{$_} } @{$inf_eng->graph->get_all_statements_in_ix( ix => 'asserted_links' ) };

print STDERR "inferred ONLY links: " . join("\n", sort keys %$link_h) . "\n\n";

exit(0);
=cut