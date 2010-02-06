#!/usr/bin/perl -w
# tests for go-slim-annotations.pl script

use strict;
use FileHandle;
use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use Storable qw( dclone );
use GOBO::Parsers::OBOParserDispatchHash;
use GOBO::Parsers::QuickGAFParser;
use GOBO::Util::GraphFunctions;
use GOBO::Writers::TreeWriter;
use GOBO::Graph;
use GOBO::DataArray;
use GOBO::InferenceEngine::CustomEngine;

use Test::More;
use Test::File;
use Test::Output;

$ENV{EXPERIMENTAL} = 1;

plan tests => 13;

my $verbose = $ENV{GO_VERBOSE} || 0;
my $status;

## a set of commands that should all fail.

## not enough subset terms found
$status = `perl bin/go-slim-annotations.pl -i t/data/caro.obo -g t/data/tiny_gaf.gaf -o t/data/tiny_gaf_remapped.gaf -t t/data/test_goslim_termlist.txt 2>&1 1>/dev/null`;
like( $status, qr/None of the terms specified in t\/data\/test_goslim_termlist.txt were found in the ontology file. Please try again./ , 'Subset terms not found');

$status = `perl bin/go-slim-annotations.pl -i t/data/tiny_obo.obo -g t/data/tiny_gaf.gaf -o t/data/tiny_gaf_remapped.gaf -s goslim_monster 2>&1 1>/dev/null`;
like( $status, qr/Error: no nodes were found in any of the subsets specified./, 'Subset terms not found');

## no annotations
$status = `perl bin/go-slim-annotations.pl -i t/data/tiny_obo.obo -g t/data/caro.obo -o t/data/tiny_gaf_remapped.gaf -s goslim_test  2>&1 1>/dev/null`;
like( $status, qr/No annotations were found! Dying/, 'No annotations at all');

## annotations to terms not in the ontology
$status = `perl bin/go-slim-annotations.pl -i t/data/tiny_obo.obo -g t/data/128up.gaf -o t/data/tiny_gaf_remapped.gaf -s goslim_test  2>&1 1>/dev/null`;
like( $status, qr/None of the terms in the annotation file matched those in the ontology file./, 'No matching annotations');

## as above, some annots found
$status = `perl bin/go-slim-annotations.pl -i t/data/transporters.obo -g t/data/AT1G49810.gaf -o t/data/tiny_gaf_remapped.gaf -s goslim_generic  2>&1 1>/dev/null`;
like( $status, qr/The following terms were not found in the ontology file/, 'Some annotations');


## test output options!!





## read in a file and see if the tree writer works...
my $parser = new GOBO::Parsers::OBOParserDispatchHash(file=>'t/data/tiny_obo.obo');
$parser->parse;
## get the subset terms
my $ss_data = GOBO::Util::GraphFunctions::get_subset_nodes( graph => $parser->graph, options => { subset => { 'goslim_test' => 1 } }  );
my $subset = $ss_data->{subset}{goslim_test};

my $results = GOBO::Util::GraphFunctions::slim_annotations(options =>  { ga_input => 't/data/tiny_gaf.gaf' }, subset => $subset, graph => dclone $parser->graph );

my $graph = $results->{graph};
my $assoc_data = $results->{assoc_data};

## a coupla little checks. The annotations should have moved down to the
## slim terms now; let's see if they have!
## everything that was annotated to GO:0000001-3 should now be annotated to
## GO:0000003
my $annots = $graph->statements_in_ix_by_target_id('annotations', 'GO:0000003');
my $str = join("\n", sort map { map { $_->{id} } @{$_->node->data_arr} } @$annots);
my $str2 = join("\n", sort ( @{$assoc_data->{by_t}{'GO:0000001'}} , @{$assoc_data->{by_t}{'GO:0000002'}} , @{$assoc_data->{by_t}{'GO:0000003'}} ) );

#my $str3 = join("\n", sort map { map { $_->{id} } @{$_->node->data_arr} } @{$graph->statements_in_ix_by_target_id('new_da_object', 'GO:0000003')});

#print STDERR "str3: $str3\n";

## 6
ok( $str eq $str2, "Checking inference is working correctly" );
if ($str ne $str2)
{	print STDERR "str: $str\nstr2: $str2\n\n";
}

## all the assocs should be annotated to the root, node 08
my $links_to_root = $graph->statements_in_ix_by_target_id('transitive_closure', 'GO:0000008');

my @annots = map { map { $_->{id} } @{$_->node->data_arr} } grep { $_->isa('GOBO::Annotation') } @$links_to_root;
#@annots = @annots;

## 7
ok( (scalar @annots == 16), "Checking we have all annots to root" );
## should only have two direct annots to the root
$links_to_root = $graph->statements_in_ix_by_target_id('transitive_reduction', 'GO:0000008');
@annots = map { map { $_->{id} } @{$_->node->data_arr} } grep { $_->isa('GOBO::Annotation') } @$links_to_root;
## 8
ok( scalar @annots == 2, "Checking there are only two direct annots to root" );

# make sure that there isn't already a mapping file in place...
if (-e "t/data/tiny_gaf_remapped.gaf")
{	system("rm", "t/data/tiny_gaf_remapped.gaf");
	die "Could not delete existing file t/data/tiny_gaf_remapped.gaf" if -e "t/data/tiny_gaf_remapped.gaf";
}

## 9
$status = system("perl", qw( bin/go-slim-annotations.pl -i t/data/tiny_obo.obo -g t/data/tiny_gaf.gaf -o t/data/tiny_gaf_remapped.gaf -s goslim_test ) );
ok($status == 0, "Running bin/go-slim-annotations.pl -i t/data/tiny_obo.obo -g t/data/tiny_gaf.gaf -s goslim_test -o t/data/tiny_gaf_remapped.gaf");

## some kind of comparison between tiny_gaf_remapped.gaf and tiny_gaf?

if (-e "t/data/count_file.txt")
{	system("rm", "t/data/count_file.txt");
	die "Could not delete existing file t/data/count_file.txt" if -e "t/data/count_file.txt";
}

system("perl", qw( bin/go-slim-annotations.pl -i t/data/tiny_obo.obo -g t/data/tiny_gaf.gaf -s goslim_test -c -o t/data/count_file.txt) );
## open the count file and check the results
my $arr = read_file('t/data/count_file.txt');

$str2 =
q/GO:0000003: direct: 6	inferred: 0	total: 6
GO:0000004: direct: 2	inferred: 6	total: 8
GO:0000005: direct: 4	inferred: 2	total: 6
GO:0000007: direct: 2	inferred: 0	total: 2
GO:0000008: direct: 2	inferred: 14	total: 16/;

$str = join("\n", @$arr);

# print STDERR "str:\n$str\nwanted:\n$str2\n\n";

## 10
ok( $str2 =~ /$str/sm, "Checking count results are correct");
system("rm", "t/data/count_file.txt");
die "Could not delete existing file t/data/count_file.txt" if -e "t/data/count_file.txt";

if (-e "t/data/tree_file.txt")
{	system("rm", "t/data/tree_file.txt");
	die "Could not delete existing file t/data/tree_file.txt" if -e "t/data/tree_file.txt";
}
system("perl", qw( bin/go-slim-annotations.pl -i t/data/tiny_obo.obo -g t/data/tiny_gaf.gaf -s goslim_test --show_tree -o t/data/tree_file.txt) );

$str = join("\n", @{read_file('t/data/tree_file.txt')});
$str2 =
q!GO:0000008 : test term GO:0000008 (2 / 16 total)
   [i] GO:0000004 : test term GO:0000004 (2 / 8 total)
      [i] GO:0000003 : test term GO:0000003 (6 / 6 total)
   [i] GO:0000005 : test term GO:0000005 (4 / 6 total)
      [i] GO:0000007 : test term GO:0000007 (2 / 2 total)!;

## 11
ok( $str eq $str2, "Checking tree is correct");


die "got:\n$str\nwanted:\n$str2\n\n" if $str ne $str2;

system("rm", "t/data/tree_file.txt");
die "Could not delete existing file t/data/tree_file.txt" if -e "t/data/tree_file.txt";

## let's change the graph and make sure the annotations are still in place.

undef $graph;
$graph = dclone $parser->graph;
## add some links and remove some links
## we now have 0001 -- r --> 0002 -- p --> 0003
$graph->remove_node('GO:0000002', 1);
$graph->add_term( new GOBO::TermNode( id => 'GO:0000002', label => 'GO:0000002' ) );

$graph->add_statements( new GOBO::LinkStatement( node=>$graph->get_term('GO:0000002'), relation=>$graph->get_relation('part_of'), target=>$graph->get_term('GO:0000003') ) );

$graph->add_statements( new GOBO::LinkStatement( node=>$graph->get_term('GO:0000001'), relation=>$graph->get_relation('regulates'), target=>$graph->get_term('GO:0000002') ) );

## we now have 0002 -- p --> 0004
$graph->add_statements( new GOBO::LinkStatement( node=>$graph->get_term('GO:0000002'), relation=>$graph->get_relation('part_of'), target=>$graph->get_term('GO:0000004')));

## we now have 0007 -- p --> 0006 -- r --> 0005

$graph->remove_node('GO:0000006', 1);
$graph->add_term( new GOBO::TermNode( id => 'GO:0000006', label => 'GO:0000006' ) );
$graph->add_statements( new GOBO::LinkStatement( node=>$graph->get_term('GO:0000007'), relation=>$graph->get_relation('part_of'), target=>$graph->get_term('GO:0000006') ) );
$graph->add_statements( new GOBO::LinkStatement( node=>$graph->get_term('GO:0000006'), relation=>$graph->get_relation('regulates'), target=>$graph->get_term('GO:0000005') ) );

## run the annotation slimmer again
undef $results;
$results = GOBO::Util::GraphFunctions::slim_annotations(options =>  { ga_input => 't/data/tiny_gaf.gaf' }, subset => $subset, graph => dclone $graph );

## OK, we should find that we have lost the annotations that were attached to GO:0000001, but we've kept those attached to GO:0000002.

$annots = $results->{graph}->get_incoming_annotations('GO:0000003');
$str = join("\n", sort map { map { $_->{id} } @{$_->node->data_arr} } @$annots);
$str2 = join("\n", sort ( @{$results->{assoc_data}->{by_t}{'GO:0000002'}} , @{$results->{assoc_data}->{by_t}{'GO:0000003'}} ) );

## 12
ok( $str eq $str2, "Checking we lost the regulates annotations" );

$annots = $results->{graph}->get_incoming_annotations('GO:0000004');
$str = join("\n", sort map { map { $_->{id} } @{$_->node->data_arr} } @$annots);
$str2 = join("\n", sort ( @{$results->{assoc_data}->{by_t}{'GO:0000002'}} , @{$results->{assoc_data}->{by_t}{'GO:0000003'}}, @{$results->{assoc_data}->{by_t}{'GO:0000004'}} ) );
## 13
ok( $str eq $str2, "Checking we have no doubled annotations" );

## 
$results = GOBO::Util::GraphFunctions::slim_annotations(options =>  { ga_input => 't/data/tiny_gaf2.gaf' }, subset => $subset, graph => dclone $graph );


exit(0);

## now let's make a new graph where slimming produces a new root.
$parser = new GOBO::Parsers::OBOParserDispatchHash(file=>'t/data/tiny_obo2.obo');
$parser->parse;
## get the subset terms
$ss_data = GOBO::Util::GraphFunctions::get_subset_nodes( graph => $parser->graph, options => { subset => { 'goslim_test' => 1 } }  );
$subset = $ss_data->{subset}{goslim_test};

$results = GOBO::Util::GraphFunctions::slim_annotations(options =>  { ga_input => 't/data/tiny_gaf.gaf' }, subset => $subset, graph => dclone $parser->graph );

$results = GOBO::Util::GraphFunctions::slim_annotations(options =>  { ga_input => 't/data/tiny_gaf.gaf', delete_new_roots => 1 }, subset => $subset, graph => dclone $parser->graph );

$graph = $results->{graph};
$assoc_data = $results->{assoc_data};

print STDERR "\n\nSLIMMED:\n";
my $pr1 = new GOBO::Writers::TreeWriter( graph => $graph, show_annotation_counts => 1, ontology_link_ix => 'direct_ontology_links', annotation_ix => 'all_annotations' );
$pr1->write;
print STDERR "\n\n";


#print STDERR "orphans: " .

exit(0);
#(GO:0000003 --[is_a]-->GO:0000004)
#(GO:0000004 --[is_a]-->GO:0000008)
#(GO:0000005 --[is_a]-->GO:0000008)
#(GO:0000007 --[is_a]-->GO:0000005)


print STDERR "Closest links:\n" . join("\n", @{$graph->get_all_statements_in_ix('transitive_reduction') || [] } )
. "\nAll links\n" . join("\n", @{$graph->get_all_statements_in_ix('transitive_closure') || [] } ) . "\n\n";

exit(0);
=cut

my $struct;
foreach (@{$graph->terms})
{	push @{$struct->{arr}}, $_;
	$struct->{by_id}{ $_->id } = \$struct->{arr}[-1];
	print STDERR "id: " . $_->id . "; arr pos: " . (scalar @{$struct->{arr}} - 1) . "\n";
}

print STDERR "struct->{by_id}:\n" . join("\n", map { "$_: " . $struct->{by_id}{$_} } keys %{$struct->{by_id}} ) . "\n\n";

my $ref = \$struct->{arr}[2];
foreach (keys %{$struct->{by_id}})
{	if ($struct->{by_id}{$_} eq $ref)
	{	print STDERR "found $ref! It was $_\n";
	}
}

undef $struct->{arr}[1];

#$struct->{arr}[0]->full_name('blib blob blab!');
${$struct->{by_id}{'GO:0000003'}}->full_name('blib blab blob!');

foreach (keys %{$struct->{by_id}})
{	if ($$struct->{by_id}{$_}->label eq 'test term GO:0000002')
	{

foreach my $id (keys %{$struct->{by_id}})
{	print STDERR "id: $id; stored: " . $struct->{by_id}{$id} . "; " . Dumper($struct->{by_id}{$id}) . "; deref'd: " . Dumper( ${$struct->{by_id}{$id}} ) . "\n\n";
}

exit(0);
## testing get_inferred_incoming_links
$ie = new GOBO::InferenceEngine::CustomEngine( graph => $parser->graph );

foreach my $t (@{$graph->terms})
{	# get all the links between the input nodes and those in the subset
	$ie->get_inferred_outgoing_links(node_id => $t->id);
}
#print STDERR "links:\n" . join("\n", sort { $a->node->id cmp $b->node->id || $a->target->id cmp $b->target->id } @{$out_g->links}) . "\n\n";
my $h;
foreach (@{$ie->inferred_graph->links})
{	$h->{$_}++;
}

undef $ie;
$ie = new GOBO::InferenceEngine::CustomEngine( graph => $parser->graph );
foreach my $t (@{$graph->terms})
{	# get all the links between the input nodes and those in the subset
	$ie->get_inferred_incoming_links($t);
}
foreach (@{$ie->inferred_graph->links})
{	$h->{$_} += 10;
}

undef $ie;
$ie = new GOBO::InferenceEngine::CustomEngine( graph => $parser->graph );
$ie->get_inferred_graph;
foreach (@{$ie->inferred_graph->links})
{	$h->{$_} += 100;
}


foreach (sort keys %$h)
{	if ($h->{$_} != 111)
	{	if ($h->{$_} == 10 || $h->{$_} == 11 || $h->{$_} == 1)
		{	$status->{$_}{graph}++;
		}
		if ($h->{$_} == 110 || $h->{$_} == 100 || $h->{$_} == 10)
		{	$status->{$_}{outgoing}++;
		}
		if ($h->{$_} == 101 || $h->{$_} == 100 || $h->{$_} == 1)
		{	$status->{$_}{incoming}++;
		}
	}
}

ok( ! $status, "Checking incoming/outgoing versions of inf engine");

if ($status)
{	print STDERR "Missing the following links:\n"
	. join("\n", map { $_ . ": " . join(", ", keys %{$status->{$_}} ) } sort keys %$status) . "\n\n";
}

foreach (sort keys %$h)
{	if ($h->{$_} != 111)
	{	$status++;
		if ($h->{$_} == 11)
		{	print STDERR "inferred graph is missing $_\n";
		}
		else
		{	print STDERR "inferred graph has extra link $_\n";
		}
	}
}

my $answer_h = {
		"GO:0000001" => { "is_a" => 1, },
		"GO:0000002" => { "part_of" => 1, },
		"GO:0000003" => { "regulates" => 1, },
		"GO:0000004" => { "negatively_regulates" => 1, "regulates" => 1, },
		"GO:0000005" => { "positively_regulates" => 1, "regulates" => 1, },
		"GO:0000006" => { "has_part" => 1, },

		"GO:0000011" => { "is_a" => 1, },
		"GO:0000012" => { "part_of" => 1, },
		"GO:0000013" => { "regulates" => 1, },
		"GO:0000014" => { "negatively_regulates" => 1, "regulates" => 1, },
		"GO:0000015" => { "positively_regulates" => 1, "regulates" => 1, },
		"GO:0000016" => { "has_part" => 1, },

		"GO:0000021" => { "part_of" => 1, },
		"GO:0000022" => { "part_of" => 1, },
	#	"GO:0000023" => "?",
	#	"GO:0000024" => "?",
	#	"GO:0000025" => "?",
	#	"GO:0000026" => "?",

		"GO:0000031" => { "regulates" => 1, },
		"GO:0000032" => { "regulates" => 1, },
	#	"GO:0000033" => "?",
	#	"GO:0000034" => "?",
	#	"GO:0000035" => "?",
	#	"GO:0000036" => "?",

		"GO:0000041" => { "negatively_regulates" => 1, "regulates" => 1, },
		"GO:0000042" => { "regulates" => 1, },
	#	"GO:0000043" => "?",
	#	"GO:0000044" => "?",
	#	"GO:0000045" => "?",
	#	"GO:0000046" => "?",

		"GO:0000051" => { "positively_regulates" => 1, "regulates" => 1, },
		"GO:0000052" => { "regulates" => 1, },
	#	"GO:0000053" => "?",
	#	"GO:0000054" => "?",
	#	"GO:0000055" => "?",
	#	"GO:0000056" => "?",

		"GO:0000061" => { "has_part" => 1, },
	#	"GO:0000062" => "?",
	#	"GO:0000063" => "?",
	#	"GO:0000064" => "?",
	#	"GO:0000065" => "?",
		"GO:0000066" => { "has_part" => 1, },
};

my $summary;
foreach my $t (sort { $a->id cmp $b->id } @{$parser->graph->terms})
{	next if $t->id =~ /GO:PAD/;
	my @links = @{ $ie->get_inferred_outgoing_links($t) };

	foreach (sort { $a->target->id cmp $b->target->id } @links)
	{	next unless $_->target->id eq 'GO:0000000';

		print  "\nnode: " . $_->node->id . ", target: " . $_->target->id . "\n" if $verbose;

		if ($answers->{$_->node->id})
		{	if ($answers->{$_->node->id}{$_->relation->id})
			{	# found the correct answer :D
#				ok(1, "Checking ". $_->node->id . " " . $_->relation->id);

				print  $_->node->id .": looking for ". join(" or ", keys %{$answers->{$_->node->id}} ) . ", found " . $_->relation->id . "\n" if $verbose;

				delete $answers->{$_->node->id}{$_->relation->id};

			}
			elsif ($answers->{$_->node->id})
			{	# found a relation, but it was wrong. Sob!
#				ok(0, "Checking ". $_->node->id . " " . $_->relation->id);

				print  $_->node->id .": looking for ". join(" or ", keys %{$answers->{$_->node->id}} ) . ", found " . $_->relation->id . "\n" if $verbose;

				$summary->{$_->node->id}{$_->relation->id}++;
			}
			if (! keys %{$answers->{$_->node->id}})
			{	delete $answers->{$_->node->id};
			}
		}
		else
		{	# shouldn't have found a relation
			print  $_->node->id .": found " . $_->relation->id . ", should not be one\n" if $verbose;
#			ok(0, $_->node->id .": incorrectly inferred relation " . $_->relation->id . " (none expected)");
			$summary->{$_->node->id}{$_->relation->id}++;
		}
	}
}

ok(! keys %$answers, "Checking we have no results left");
ok(! keys %$summary, "Checking we made no incorrect inferences");
if ($verbose)
{	if (keys %$answers)
	{	print  "Missing the following inferences:\n" . Dumper($answers);
	}
	if (keys %$summary)
	{	print  "Made the following incorrect inferences:\n" . Dumper($summary);
	}
}

$parser->reset_parser;
$parser->parse_file('t/data/obo_file.obo');
## testing get_inferred_incoming_links
$ie = new GOBO::InferenceEngine::CustomEngine( graph => $parser->graph );
$ie->get_inferred_graph;
print STDERR "\n\n\n\nobo_file.obo inferred links:\n";
foreach (@{$ie->inferred_graph->links})
{	print STDERR "$_\n";
}

## ok, let's try the reduction now...

$ie = new GOBO::InferenceEngine::CustomEngine( graph => $parser->graph );
$ie->get_reduced_graph;
print STDERR "obo_file.obo reduced graph inferred links:\n";
foreach (@{$ie->inferred_graph->links})
{	print STDERR "$_\n";
}




exit(0);

my $output = 't/data/test_output.txt';

if (-e $output)
{	system("rm", $output);
}

## a tiny test
$status = system("perl", qw( bin/go-slim-annotations.pl -i t/data/tiny_obo.obo -s test_goslim -g t/data/tiny_gaf.gaf -o t/data/test_output.txt -v -f ) );
ok($status == 0, "Checking go-slim-annotations.pl with valid args");
file_not_empty_ok($output, "Results file exists");

{	open(FH, "< $output") or die "Could not open file $output: $!";
	my @contents;
	while (<FH>) {
		chomp;
		push @contents, $_;
	}
	close(FH);
	my @arr = (
	'FB	FBgn0010466	128up		GO:0000003	FB:FBrf0174215	IC	GO:my_ref	C	upstream of RpIII128	CG8340|GTP-bp|X71866	gene	taxon:7227	20090124	FlyBase',
	'FB	FBgn0010467	128up		GO:0000003	FB:FBrf0174215	IC	GO:my_ref	C	upstream of RpIII128	CG8340|GTP-bp|X71866	gene	taxon:7227	20090124	FlyBase');
	my $str = join("\n", @arr);
	ok( join("\n", @contents)  =~ /$str/, "Found the lines we needed");

	system("rm", $output);
}

if (-e $output)
{	system("rm", $output);
}
## do some slimming
$status = system("perl", qw( bin/go-slim-annotations.pl -i t/data/slimmer_test_3.obo -s test_goslim -g t/data/test_gaf.gaf -v -c -o ), $output );
file_not_empty_ok($output, "Checking go-slim-annotations.pl with valid args");

## do some slimming
$status = system("perl", qw( bin/go-slim-annotations.pl -v -i t/data/slimmer_test_3.obo -s test_goslim -g t/data/test_gaf.gaf --show_tree -o t/data/tree_file.txt ) );
file_not_empty_ok("t/data/tree_file.txt", "Checking go-slim-annotations.pl with valid args");

exit(0);
=cut


=cut
## parse file and load the graph
my $parser = new GOBO::Parsers::OBOParserDispatchHash(file=>$options->{input});
$parser->parse;
my $graph = $parser->graph;

## get the GA data
my $gaf_parser = GOBO::Parsers::QuickGAFParser->new(fh=>$options->{ga_input}, graph => $parser->graph);
$gaf_parser->parse;

$graph = $gaf_parser->graph;
print STDERR "Done quickparse_gaf!\n" if $options->{verbose};


#file_not_empty_ok("t/data/count_file.txt", "Results file exists");



=cut



## do some slimming
$status = system("perl", qw( bin/go-slim-annotations.pl -i t/data/tiny_obo.obo -s test_goslim -g t/data/tiny_gaf.gaf -o t/data/outfile.txt -c -v -f ) );
ok($status == 0, "Checking go-slim-annotations.pl with valid args");
file_not_empty_ok("t/data/outfile.txt", "Results file exists");



## do some slimming
#$status = system("perl", qw( bin/go-slim-annotations.pl --mapping_file t/data/mapping_file.txt -i t/data/slimmer_test_3.obo -g t/data/test_gaf.gaf -c -o t/data/count_file2.txt ) );
#ok($status == 0, "Checking go-slim-annotations.pl with mapping file");

#file_not_empty_ok("t/data/count_file2.txt", "Results file exists");

## check that the results files are the same




# key: GO:00000XY
# X and Y encode the relations; graph structure is term1 X term2 Y GO:0000000
# 1 => is_a
# 2 => part_of
# 3 => regulates,
# 4 => negatively_regulates,
# 5 => positively_regulates,
# 6 => has_part
# so GO:0000052 in the graph would have this relation:
# GO:0000052 positively_regulates GO:0000002 part_of GO:0000000





=cut
# 1
$status = `perl bin/go-slimdown.pl -i t/data/obofile.obo -s test_goslim -o t/slimmer_test_results.obo 2>&1 1>/dev/null`;
like( $status, qr/Error: /, "Checking go-slimdown.pl with invalid input");

# 2
$status = `perl bin/go-slimdown.pl -i t/data/obo_file.obo -s test_goslim 2>&1 1>/dev/null`;
like( $status, qr/Error: /, "Checking go-slimdown.pl with no output");

# 3
$status = `perl bin/go-slimdown.pl -i t/data/obo_file.obo -o t/slimmer_test_results.obo 2>&1 1>/dev/null`;
like( $status, qr/Error: /, "Checking go-slimdown.pl with no slim");

# 4
$status = `perl bin/go-slimdown.pl -i t/data/obo_file.obo -s test_goslim -b t/data/myslimfile 2>&1 1>/dev/null`;
like( $status, qr/Error: /, "Checking go-slimdown.pl with incorrectly specified output");


# goslim not in file
# 5
$status = `perl bin/go-slimdown.pl -i t/data/obo_file.obo -s goslim_monster -o t/slimmer_test_results.obo 2>&1 1>/dev/null`;
like( $status, qr/Error: /, "Checking go-slimdown.pl with invalid subset");

# 6
$status = system("perl", qw( bin/go-slimdown.pl -i t/data/obo_file.obo -s test_goslim -o t/slimmer_test_results.obo) );
ok($status == 0, "Checking go-slimdown.pl with valid args");

die "go-slimdown.pl exited funny: $?" unless $status == 0;

## read in the graph, check it is ok
my $parser = new GOBO::Parsers::OBOParser(file=>"t/slimmer_test_results.obo");
$parser->parse;

# 7
cmp_ok(testme($parser->graph, 1), "==", 1, "Checking slimdown results");
system("rm", "t/slimmer_test_results.obo");

die ("Did not remove t/slimmer_test_results.obo properly!") if -e "t/slimmer_test_results.obo";

# 8
# OK, let's try a different slim now...
$status = system("perl", qw( bin/go-slimdown.pl -i t/data/obo_file.obo -s test_next_goslim -o t/slimmer_test_results.obo) );
ok($status == 0, "Checking go-slimdown.pl with valid args -i t/data/obo_file.obo -s test_next_goslim -o t/slimmer_test_results.obo");

die "go-slimdown.pl exited funny: $?" unless $status == 0;

## read in the graph, check it is ok
undef $parser;
$parser = new GOBO::Parsers::OBOParser(file=>"t/slimmer_test_results.obo");
$parser->parse;

# 9
cmp_ok(testme($parser->graph, 2), "==", 1, "Checking slimdown results");
system("rm", "t/slimmer_test_results.obo");

die ("Did not remove t/slimmer_test_results.obo properly!") if -e "t/slimmer_test_results.obo";

my $args = {
1 => [ qw(-s test_next_goslim -s test_goslim) ],
2 => [ qw(-s test_next_goslim test_goslim) ],
3 => [ '-a' ],
4 => [ qw(-r test) ],
5 => [ qw(-r test.+goslim) ],
};

# 10 - 44
# 7 tests per arg
foreach my $a (values %$args)
{	my $cmd;
	# invalid inputs
	# xxx, combined, no output
	$cmd = 'perl bin/go-slimdown.pl -i t/data/obo_file.obo ' . join(" ", @$a) . " --combined -b t/obo_file_SLIM_NAME.obo 2>&1 1>/dev/null";
	$status = `$cmd`;

	like( $status, qr/Error: /, "Checking go-slimdown.pl with invalid params");

	# xxx, not combined, no basefile
	$cmd = 'perl bin/go-slimdown.pl -i t/data/obo_file.obo ' . join(" ", @$a) . " -o t/slimmer_test_results.obo 2>&1 1>/dev/null";
	$status = `$cmd`;

	like( $status, qr/Error: /, "Checking go-slimdown.pl with invalid params");

	# valid inputs
	$status = system("perl", qw( bin/go-slimdown.pl -i t/data/obo_file.obo -b t/obo_file_SLIM_NAME.obo), @$a );

	ok($status == 0, "Checking go-slimdown.pl with args -i t/data/obo_file.obo -b t/obo_file_SLIM_NAME.obo " . join(" ", @$a) );

	die "go-slimdown.pl exited funny: $?" unless $status == 0;

	## read in the graph, check it is ok
	undef $parser;
	$parser = new GOBO::Parsers::OBOParser(file=>"t/obo_file_test_goslim.obo");
	$parser->parse;

	cmp_ok(testme($parser->graph, 1), "==", 1, "Checking slimdown results");

	## read in the graph, check it is ok
	undef $parser;
	$parser = new GOBO::Parsers::OBOParser(file=>"t/obo_file_test_next_goslim.obo");
	$parser->parse;

	cmp_ok(testme($parser->graph, 2), "==", 1, "Checking slimdown results");

	system("rm", "t/obo_file_test_goslim.obo");
	system("rm", "t/obo_file_test_next_goslim.obo");

	# now test a combination of slims
	$status = system("perl", qw( bin/go-slimdown.pl -i t/data/obo_file.obo -o t/slimmer_test_results.obo --combined) , @$a );

	ok($status == 0, "Checking go-slimdown.pl with valid args -i t/data/obo_file.obo -o t/slimmer_test_results.obo --combined " . join(" ", @$a) );

	die "go-slimdown.pl exited funny: $?" unless $status == 0;

	## read in the graph, check it is ok
	undef $parser;
	$parser = new GOBO::Parsers::OBOParser(file=>"t/slimmer_test_results.obo");
	$parser->parse;

	cmp_ok(testme($parser->graph, 3), "==", 1, "Checking slimdown results");

	system("rm", "t/slimmer_test_results.obo");
	die ("Did not remove t/slimmer_test_results.obo properly!") if -e "t/slimmer_test_results.obo";

}
=cut
exit(0);



=cut
01 is_a 08
01 part_of 08
01 regulates 08
02 is_a 06
02 is_a 07
03 part_of 07
04 is_a 12
04 positively_regulates 15
04 negatively_regulates 16
05 regulates 08
06 is_a 09
07 part_of 11
08 negatively_regulates 10
09 is_a 10
10 is_a 18
11 is_a 10
12 is_a 13
13 is_a 14
14 is_a 18
15 part_of 14
16 is_a 14
17 is_a 19
18 is_a 19
21 is_a 19
22 is_a 21
23 is_a 22
24 is_a 23
24 part_of 25
25 part_of 19

negatively_regulates is_a regulates
positively_regulates is_a regulates

GS terms:
01
02
03
04
05
06
07
10
14
15
19
24
25

rlns we should therefore have:
01 is_a 08 negatively_regulates 10       neg regs
01 part_of 08 negatively_regulates 10    regs
01 regulates 08 negatively_regulates 10  no rln
02 is_a 06                                       is a
02 is_a 07                                       is a
02 is_a 07 part_of 11 is_a 10    part of
03 part_of 07                                    part of
04 is_a 12 is_a 13 is_a 14       is a
04 pos_regs 15                                   pos regs
04 neg_regulates 16 is_a 14              neg regs
05 regulates 08 negatively_regulates 10  no rln
06 is_a 10                                       is a
07 part_of 11 is_a 10                    part of
10 is_a 18 is_a 19                       is a
14 is_a 18 is_a 19                       is a
15 part_of 14                                    part of
24 is_a ... is_a 21 is_a 19              is a
24 part_of 25                                    part of
25 part_of 19                                    part of

negatively_regulates is_a regulates
positively_regulates is_a regulates

=cut

sub testme {
	my $g = shift;
	my $n = shift;  # test number

my $answers;

$answers->{1}{"01"}{negatively_regulates}{"10"} = 1,
#$answers->{"01"}{regulates}{"10"} = 1,
$answers->{1}{"02"}{is_a}{"06"} = 1;
$answers->{1}{"02"}{is_a}{"07"} = 1;
$answers->{1}{"02"}{part_of}{"10"} = 1;
$answers->{1}{"03"}{part_of}{"07"} = 1;
$answers->{1}{"04"}{is_a}{"14"} = 1;
$answers->{1}{"04"}{positively_regulates}{"15"} = 1;
# $answers->{"04"}{regulates}{"15"} = 1;
$answers->{1}{"04"}{negatively_regulates}{"14"} = 1;
# $answers->{"04"}{regulates}{"14"} = 1;
$answers->{1}{"06"}{is_a}{"10"} = 1;
$answers->{1}{"07"}{part_of}{"10"} = 1;
$answers->{1}{"10"}{is_a}{"19"} = 1;
$answers->{1}{"14"}{is_a}{"19"} = 1;
$answers->{1}{"15"}{part_of}{"14"} = 1;
$answers->{1}{"24"}{is_a}{"19"} = 1;
$answers->{1}{"24"}{part_of}{"25"} = 1;
$answers->{1}{"25"}{part_of}{"19"} = 1;
#$answers->{negatively_regulates}{is_a}{regulates} = 1;
#$answers->{positively_regulates}{is_a}{regulates} = 1;

$answers->{2}{"01"}{regulates}{"08"} = 1,
$answers->{2}{"01"}{is_a}{"08"} = 1,
$answers->{2}{"01"}{part_of}{"08"} = 1,
$answers->{2}{"01"}{negatively_regulates}{"19"} = 1,
$answers->{2}{"02"}{is_a}{"19"} = 1,
$answers->{2}{"02"}{part_of}{"19"} = 1,
$answers->{2}{"03"}{part_of}{"19"} = 1,
$answers->{2}{"05"}{regulates}{"08"} = 1,
$answers->{2}{"08"}{negatively_regulates}{"19"} = 1,
$answers->{2}{"13"}{is_a}{"19"} = 1,
$answers->{2}{"21"}{is_a}{"19"} = 1,


$answers->{3}{"01"}{regulates}{"08"} = 1,
$answers->{3}{"01"}{is_a}{"08"} = 1,
$answers->{3}{"01"}{part_of}{"08"} = 1,
$answers->{3}{"01"}{negatively_regulates}{"10"} = 1,
$answers->{3}{"02"}{is_a}{"06"} = 1;
$answers->{3}{"02"}{is_a}{"07"} = 1;
$answers->{3}{"02"}{part_of}{"10"} = 1;
$answers->{3}{"03"}{part_of}{"07"} = 1;
$answers->{3}{"04"}{is_a}{"13"} = 1;
$answers->{3}{"04"}{positively_regulates}{"15"} = 1;
$answers->{3}{"04"}{negatively_regulates}{"14"} = 1;
$answers->{3}{"05"}{regulates}{"08"} = 1,
$answers->{3}{"06"}{is_a}{"10"} = 1;
$answers->{3}{"07"}{part_of}{"10"} = 1;
$answers->{3}{"08"}{negatively_regulates}{"10"} = 1,
$answers->{3}{"10"}{is_a}{"19"} = 1;
$answers->{3}{"13"}{is_a}{"14"} = 1,
$answers->{3}{"14"}{is_a}{"19"} = 1;
$answers->{3}{"15"}{part_of}{"14"} = 1;
$answers->{3}{"21"}{is_a}{"19"} = 1,
$answers->{3}{"24"}{is_a}{"21"} = 1;
$answers->{3}{"24"}{part_of}{"25"} = 1;
$answers->{3}{"25"}{part_of}{"19"} = 1;

	my $summary;
	my $ans;
	map {
		my $t = $_;
		map {
			my $r = $_;
			map {
				$ans->{$t}{$r}{$_} = 1
				} keys %{$answers->{$n}{$t}{$r}};
			} keys %{$answers->{$n}{$t}};
		} keys %{$answers->{$n}};

	foreach my $t (sort { $a->id cmp $b->id } @{$g->terms})
	{	#my @links = @{ $infeng->get_inferred_outgoing_links($t) };
		my @links = @{ $g->get_outgoing_links(node=>$t) };

#		print STDERR "links for " . $t->id . ": " . Dumper( \@links );

		foreach (sort { $a->target->id cmp $b->target->id } @links)
		{
			print STDERR "\nnode: " . $_->node->id . ", target: " . $_->target->id . "\n" if $verbose;

			if ($ans->{$_->node->id}
				&& $ans->{$_->node->id}{$_->relation->id}
				&& $ans->{$_->node->id}{$_->relation->id}{$_->target->id} )
			{	# found the correct answer :D
			#	ok(1, "Checking ". $_->node->id . " " . $_->relation->id . " " . $_->target->id);

				print STDERR $_->node->id .": looking for ". join(" or ", keys %{$ans->{$_->node->id}} ) . ", found " . $_->relation->id . "\n" if $verbose;

				delete $ans->{$_->node->id}{$_->relation->id}{$_->target->id};

				if (! keys %{$ans->{$_->node->id}{$_->relation->id}})
				{	delete $ans->{$_->node->id}{$_->relation->id};
				}

				if (! keys %{$ans->{$_->node->id}})
				{	delete $ans->{$_->node->id};
				}
			}
			else
			{	# shouldn't have found a relation
				print STDERR $_->node->id .": found " . $_->relation->id . " " . $_->target->id . ", incorrect!\n" if $verbose;
			#	ok(0, $_->node->id .": incorrectly inferred relation " . $_->relation->id . " (none expected)");
				$summary->{$_->node->id}{$_->relation->id}{$_->target->id}++;
			}
		}
	}

#	ok(! keys %{$ans->}, "Checking we have no results left");

#	if ($verbose)
	if (keys %$ans || keys %$summary)
	{	if (keys %$ans)
		{	print STDERR "Missing the following inferences:\n" . Dumper($ans);
		}
		if (keys %$summary)
		{	print STDERR "Made the following incorrect inferences:\n" . Dumper($summary);
		}
		return 2;
	}
	return 1;
}


sub read_file {
	my $file = shift;
	local $/ = "\n";
	open(IN, '<'.$file) or die "The file ".$file." could not be opened: $!\nDying";
	my @arr;
	while (<IN>)
	{	if (/^[^!]/ && /\S+/)
		{	chomp;
			push @arr, $_;
		}
	}
	close(IN);
	return [ @arr ];
}


