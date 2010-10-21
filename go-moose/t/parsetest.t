#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
use Test::Deep;
use Test::Deep::NoTest;
#plan tests => ;
use GOBO::Parsers::GAFParser;
use GOBO::Parsers::OBOParser;
use GOBO::Parsers::OBOParserDispatchHash;
use Carp::Always;

use FileHandle;
use Data::Dumper;

my $gaf_parser;
my $obo_parser;
my $dh_parser;
=cut
open(FH, '< t/data/obo_file_2.obo') or die "Could not open test file";
while (<FH>)
{	print STDERR "$_";
	if ($_ !~ /\w/)
	{	print STDERR "reached header end. Changing separator!\n";
		$/ = "\n\n";
	}
}
exit(0);

=cut
my $tags = {
	has_x => [ qw( nodes terms relations statements edges ontology_links annotations declared_subsets instances formulae) ],
	body_only => [ qw( nodes terms relations statements edges ontology_links annotations instances formulae ) ],
	header_only => [ qw(default_namespace date comment format_version version property_value) ],
	both => [ qw(declared_subsets) ],
};

# tests 1 - 10
$gaf_parser = new GOBO::Parsers::GAFParser(file=>new FileHandle("t/data/128up.gaf"));
ok($gaf_parser->has_fh);
$gaf_parser->parse;
my $g_128up = $gaf_parser->graph;
ok($g_128up);


$gaf_parser = new GOBO::Parsers::GAFParser(file=>"t/data/AT1G49810.gaf");
ok($gaf_parser->has_fh);
$gaf_parser->parse;
my $g_AT1G = $gaf_parser->graph;
ok($g_AT1G);


$gaf_parser = new GOBO::Parsers::GAFParser(fh=>"t/data/128up.gaf");
ok($gaf_parser->has_fh);
$gaf_parser->parse;
my $g2_128up = $gaf_parser->graph;
ok($g2_128up);

is_deeply($g_128up, $g2_128up, "Checking that the GAF parsers returned the same results");

$gaf_parser = new GOBO::Parsers::GAFParser(fh=>new FileHandle("t/data/AT1G49810.gaf") );
ok($gaf_parser->has_fh);
$gaf_parser->parse;
my $g2_AT1G = $gaf_parser->graph;
ok($g2_AT1G);
is_deeply($g_AT1G, $g2_AT1G, "Checking that the GAF parsers returned the same results");


## setting the file using set_file
$gaf_parser = new GOBO::Parsers::GAFParser;
$gaf_parser->set_file("t/data/128up.gaf");
ok($gaf_parser->has_fh, "set_file used to initialize the fh");  #11
$gaf_parser->parse;
undef $g2_128up;
$g2_128up = $gaf_parser->graph;
is_deeply($g_128up, $g2_128up, "set_file: GAF parsers returned the same results"); # 12

my $fh = new FileHandle("t/data/AT1G49810.gaf");
$gaf_parser = new GOBO::Parsers::GAFParser;
$gaf_parser->set_file($fh);
ok($gaf_parser->has_fh); # 13
$gaf_parser->parse;
undef $g2_AT1G;
$g2_AT1G = $gaf_parser->graph;
is_deeply($g_AT1G, $g2_AT1G, "set_file: GAF parsers returned the same results"); # 14

# using parse_file
$gaf_parser = new GOBO::Parsers::GAFParser;
$gaf_parser->parse_file(file => new FileHandle("t/data/128up.gaf"));
ok($gaf_parser->has_fh); # 15
undef $g2_128up;
$g2_128up = $gaf_parser->graph;
is_deeply($g_128up, $g2_128up, "parse_file: GAF parsers returned the same results"); # 16

$gaf_parser = new GOBO::Parsers::GAFParser;
#$gaf_parser->parse_file(file => "t/data/AT1G49810.gaf");
$gaf_parser->parse_file("t/data/AT1G49810.gaf");
ok($gaf_parser->has_fh); # 17
undef $g2_AT1G;
$g2_AT1G = $gaf_parser->graph;
is_deeply($g_AT1G, $g2_AT1G, "parse_file: GAF parsers returned the same results"); # 18


## testing the OBO parsers...

eval { $obo_parser = new GOBO::Parsers::OBOParser(file=>'/a/load/of/bollox'); };
ok( defined $@ );

#check the OBOParser quickly...
$obo_parser = new GOBO::Parsers::OBOParser(file=>'t/data/gtp.obo', parse_method => 'if_else');
$dh_parser = new GOBO::Parsers::OBOParserDispatchHash(file=>'t/data/gtp.obo');
ok($obo_parser->has_fh && $dh_parser->has_fh);
$obo_parser->parse;
$dh_parser->parse;
ok( $obo_parser->graph->has_terms && $dh_parser->graph->has_terms, "Checking there are terms in the graph");
cmp_deeply( $obo_parser->graph, $dh_parser->graph, "Comparing OBO and DH parser graphs");

## OK, basics done. Let's try a bit of parsing...
# this is a graph with everything in the known (obo) world in it.
$obo_parser = new GOBO::Parsers::OBOParser(file=>'t/data/obo_file_2.obo', parse_method => 'if_else');
$dh_parser =

my $create_p_sub = {
	dh => sub { return new GOBO::Parsers::OBOParserDispatchHash(file=>'t/data/obo_file_2.obo'); },
	ie => sub { return new GOBO::Parsers::OBOParser(file=>'t/data/obo_file_2.obo', parse_method => 'if_else') },
};

my $results;
my $errs;
# 23 onwards
foreach my $x (keys %$create_p_sub)
{	my $p = &{$create_p_sub->{$x}};
	$p->parse;
	my $graph = $p->graph;
	# check that we have these entities in our graph
	foreach my $e (@{$tags->{has_x}})
	{	my $fn = "has_$e";
	#	print STDERR "fn: $fn; result of graph->$fn: ". Dumper( $graph->$fn ) . "\n";
		push @$errs, $e if ! $graph->$fn;
	}

#	foreach (@{$graph->statements})
#	{	print STDERR "$_\n";
#	}

	# 23, 32
	ok( ! defined $errs, "Checking entities in the graph" );
	print STDERR "Did not find the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;

	push @{$results->{ $x }}, $graph;

	my $g_keys;  # store the non-body stuff in g_keys
	foreach my $k (keys %$graph)
	{	next if grep { $k eq $_ } @{$tags->{body_only}};
		$g_keys->{$k} = $graph->{$k};
	}

	## let's try a few options now...

#	print STDERR "\n\n\nStarting options testing!\n";

	# ignore body and header
	$p = &{$create_p_sub->{$x}};
#	$p->reset_parser;
	$p->parse_file
	(file=>'t/data/obo_file_2.obo', options => { body => { ignore => '*' }, header => { ignore => '*' } });

	my $new_graph = $p->graph;
	$new_graph->remove_node( $new_graph->get_relation('is_a'), 1 );

	#print STDERR "ignore body and header graph: " . Dumper($new_graph);
	# 24, 33
	isa_ok( $new_graph, "GOBO::Graph", "Checking graph is_a GOBO::Graph" );

	push @{$results->{ $x }}, $new_graph;


	undef $errs;
	foreach my $e (@{$tags->{has_x}})
	{	my $fn = "has_$e";
		push @$errs, $e if $new_graph->$fn;
	}
	# 25, 34
	ok( ! $errs, "Checking entities in the graph" );
	print STDERR "Found the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;

	if ($errs && @$errs)
	{	## check out what's happened to the options...
		print STDERR "parser options: " . Dumper($p->options) . "\n\n";
	}


	#$p->reset_parser;
	$p = &{$create_p_sub->{$x}};
	$p->parse_file(file=>'t/data/obo_file_2.obo', options => { header => { ignore => '*' } });
	$new_graph = $p->graph;
	print STDERR "ignoring headers\n";

	push @{$results->{ $x }}, $new_graph;
	#foreach my $e (@{$tags->{has_x}})
	#{	print STDERR "graph->$e: " . Dumper( $graph->$e ) . "\n";
	#}

	undef $errs;
	foreach my $e (@{$tags->{has_x}})
	{	push @$errs, $e if scalar @{ $new_graph->$e } != scalar @{ $graph->$e };
	}
	# 26, 35
	ok( ! defined $errs, "Ignored header: checking body elements" );
	if ($errs && @$errs)
	{	print STDERR "Discrepancies in the following: " . join(", ", @$errs ) . "\n";
	}

	undef $errs;
	foreach my $e (@{$tags->{header_only}})
	{	push @$errs, $e if $new_graph->{$e};
	}

	# 27, 36
	ok( ! defined $errs, "Ignored header: checking header elements" );
	print STDERR "Found the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;


	#$p->reset_parser;
	$p = &{$create_p_sub->{$x}};
	$p->parse_file(file=>'t/data/obo_file_2.obo', options => { body => { ignore => '*' } });
	$new_graph = $p->graph;
	push @{$results->{ $x }}, $new_graph;
	$new_graph->remove_node( $new_graph->get_relation('is_a'), 1 );

	undef $errs;
	foreach my $e (@{$tags->{body_only}})
	{	my $fn = "has_$e";
		push @$errs, $e if $new_graph->$fn;
	}

	# 28, 37
	ok( ! defined $errs, "Ignored body: checking body elements" );
	print STDERR "Found the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;

	if ($errs && @$errs)
	{	## check out what's happened to the options...
		print STDERR "parser options: " . Dumper($p->options) . "\n\n";
	}

	undef $errs;
	foreach my $e (@{$tags->{header_only}}, @{$tags->{both}})
	{	if (! eq_deeply( $graph->{$e}, $new_graph->{$e} ))
		{	push @$errs, $e;
		}
	}
	# 29, 38
	ok( ! defined $errs, "Ignored body: checking header elements" );
	print STDERR "Found the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;


	## ignore everything except the instance and typedef stanza
	#$p->reset_parser;
	$p = &{$create_p_sub->{$x}};
	$p->parse_file(file=>'t/data/obo_file.obo', options => { body => { parse_only => { instance => '*', annotation => '*' } } });

	$new_graph = $p->graph;
	push @{$results->{ $x }}, $new_graph;
	$new_graph->remove_node( $new_graph->get_relation('is_a'), 1 );

	foreach my $e (@{$tags->{body_only}})
	{	my $fn = "has_$e";
		push @$errs, $e if $new_graph->$fn;
	}

	# 30, 39
	ok( ! defined $errs, "Parse only instances and annotations: checking body elements" );
	print STDERR "Found the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;


	## ignore everything except the instance and typedef stanza
	#$p->reset_parser;
	$p = &{$create_p_sub->{$x}};
	$p->parse_file(file=>'t/data/transporters.obo', options => { body => { parse_only => { term => ['id', 'namespace', 'synonym'] } } });

	$new_graph = $p->graph;
	$new_graph->remove_node( $new_graph->get_relation('is_a'), 1 );
	push @{$results->{ $x }}, $new_graph;

	foreach my $e (@{$tags->{body_only}})
	{	next if $e eq 'terms' || $e eq 'nodes';
		my $fn = "has_$e";
		push @$errs, $e if $new_graph->$fn;
	}
	# 31, 40
	ok( ! defined $errs, "Parse only term ids, names and namespaces: checking body elements" );
	print STDERR "Found the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;


	## ignore everything except the instance and typedef stanza
	#$p->reset_parser;
	$p = &{$create_p_sub->{$x}};
	$p->parse_file(file=>'t/data/obo_file_2.obo', options => { body => { ignore => { term => ['is_a', 'relationship', 'synonym' ] } } });

}

# 41 - 46
my @p_types = ( keys %$results );
# check that both parsers got the same results
while (@{$results->{$p_types[0]}})
{	#my $g1 = pop @{$results->{$p_types[0]}}
	cmp_deeply( pop @{$results->{$p_types[0]}}, pop @{$results->{$p_types[1]}}, "Comparing results...");
}

# 47
## try using the Dispatch Hash parser
$dh_parser = new GOBO::Parsers::OBOParserDispatchHash;
$dh_parser->parse_file(file => 't/data/obo_file_2.obo');

$obo_parser = new GOBO::Parsers::OBOParser;
$obo_parser->parse_method('if_else');
$obo_parser->parse_file(file=>'t/data/obo_file_2.obo');

ok($dh_parser->graph, "Checking parser produced a graph");

undef $errs;
foreach my $e (@{$tags->{has_x}})
{	push @$errs, $e if scalar @{ $dh_parser->graph->$e } != scalar @{ $obo_parser->graph->$e };
}

# 48
ok( ! defined $errs, "Ignored header: checking body elements" );
if ($errs && @$errs)
{	print STDERR "Discrepancies in the following: " . join(", ", @$errs ) . "\n";
	foreach (@$errs)
	{	if ($_ eq 'nodes')
		{	print STDERR "$_: got:\n" . join("\n", sort map { $_->id } @{$dh_parser->graph->$_}) . "\n\n\n$_: expected:\n" . join("\n", sort map { $_->id } @{$obo_parser->graph->$_}) . "\n\n\n\n";
		}
		elsif ($_ eq 'ontology_links')
		{	print STDERR "$_: got:\n" . join("\n", @{$dh_parser->graph->$_}) . "\n\n\n$_: expected:\n" . join("\n", @{$obo_parser->graph->$_}) . "\n\n\n\n";
		}
	}
}

# 49
cmp_deeply($dh_parser->graph, $obo_parser->graph, "Checking the dispatch hash parser");

## let's try parse_header_from_arr
my @header_arr;
my @body_arr;
{	local $/ = "\n[";
	open(FH, "<" . 't/data/obo_file_2.obo') or die("Could not open t/data/obo_file_2.obo: $!");
	@header_arr = split("\n", <FH> );
	close FH;

	local $/ = "\n";
	open(FH, "<" . 't/data/obo_file_2.obo') or die("Could not open t/data/obo_file_2.obo: $!");

	while (<FH>)
	{	push @body_arr, $_;
	}


	my $i = 1;
	while ($i == 1)
	{	if ( $body_arr[0] =~ /^\[\S+/ )
		{	$i = 0;
			last;
		}
		shift @body_arr;
	}
#	print STDERR "first in array body_arr: " . $body_arr[0] . "\n";
}
my $new_parser = new GOBO::Parsers::OBOParser;
$new_parser->parse_header_from_array( array => [ @header_arr ] );

$dh_parser = new GOBO::Parsers::OBOParser( file=>'t/data/obo_file_2.obo' );
$dh_parser->parse_header;

cmp_deeply($new_parser->graph, $dh_parser->graph, "Checking parse_header_from_array");

## let's try parse_body_from_arr
# delete the graph
$dh_parser = new GOBO::Parsers::OBOParser( file=>'t/data/obo_file_2.obo' );
$dh_parser->parse_body;

$new_parser = new GOBO::Parsers::OBOParser;
$new_parser->parse_body_from_array( array => [ @body_arr ] );

cmp_deeply($new_parser->graph, $dh_parser->graph, "Checking parse_body_from_array");

## check the whole caboodle
$dh_parser = new GOBO::Parsers::OBOParser( file=>'t/data/obo_file_2.obo' );
$dh_parser->parse;

$new_parser = new GOBO::Parsers::OBOParser;
$new_parser->parse_from_array( array => [ @header_arr, @body_arr ] );

cmp_deeply($new_parser->graph, $dh_parser->graph, "Checking parse_from_array");

undef $dh_parser;
#=cut

$dh_parser = new GOBO::Parsers::OBOParser( file => 't/data/roundtripme.obo' );
$dh_parser->strict_mode(0);
print STDERR "strict mode is " . ( $dh_parser->strict_mode ? "ON\n" : "OFF\n" );
$dh_parser->parse;

# id: id-x
# def: "Ready or not, here I come!!" [PMID:1, foo:bar "bleh", DB:key "Encyclopaedia, isn't it?"]
my $t = $dh_parser->graph->get_term('id-x');
ok($t->definition eq 'Ready or not, here I come!!', "Checking def parsing");
ok(scalar @{$t->definition_xrefs} == 3, "Term has three def xrefs");
#undef $errs;
my $h;
foreach (@{$t->definition_xrefs})
{	$h->{ lc($_->id) } = "ID: " . $_->id . "; LABEL: " . ($_->label || "none");
}
ok(join("ZZZ", map { $h->{$_} } sort keys %$h) eq 'ID: DB:key; LABEL: Encyclopaedia, isn\'t it?ZZZID: foo:bar; LABEL: blehZZZID: PMID:1; LABEL: none', "Checking dbxref parsing");

# synonym: "s1-exact" EXACT []
# synonym: "s1-exact-cited" EXACT [PMID:2 "My favourite paper"]
# synonym: "s1-exact-t" EXACT st2 [XREF:1, XREF:2 "My \"favourite\" database", XREF:3]
=cut
foreach (@{$t->synonyms})
{	if ($_->label eq 's1-exact')
	{	ok(scalar @{$_->xrefs} == 0, "Synonym with no xrefs");
	}
	elsif ($_->label eq 's1-exact-cited')
	{	ok(scalar @{$_->xrefs} == 1 && @{$_->xrefs}[0]


	}
	elsif ($_->label eq 's1-exact-t')
	{

=cut
foreach (@{$dh_parser->graph->terms})
{	## check that funny def has been parsed correctly
	if ($_->definition)
	{	print STDERR "def: " . $_->definition. "\n";
		if ($_->definition_xrefs)
		{	print STDERR "xrefs: {" . join("}{", @{$_->definition_xrefs} ) . "}\n\n";
		}
	}
	if ($_->synonyms)
	{	foreach my $s (@{$_->synonyms})
		{	print STDERR "syn: " . $s->label .
			", scope: " . ($s->scope || "undef") .
			", type: " . ($s->synonym_type || "undef") .
			", xrefs: " . (join(", ", map { $_->id . ($_->label ? " " . $_->label : "" ) } @{$s->xrefs}) || "none!") . "\n";
		}
	}
}



# id: id-y
# def: "His name was "Skippy" the bush kangaroo. Fancy that." [REF:100 "The 100th Ref"]

# id: obs-1
# formula: "Que sera, sera" Italian [ ISBN:0123456789 "That ol' book o' tricks", SO:cjm ]




exit(0);




=cut
#print STDERR "term: " . Dumper( [ @{$new_graph->terms}[0-5] ] ) . "\n";
system("clear");

## ignore everything except the instance and typedef stanza
$obo_parser->reset_parser;
$obo_parser->parse_file(file=>'t/data/obo_file_2.obo', options => { body => { ignore => { term => ['is_a', 'relationship', 'synonym' ], typedef => '*', 'annotation' => [ '*' ] } } });

system("clear");
system("clear");
system("clear");

print STDERR "terms: " . Dumper($obo_parser->graph->terms);
#$obo_parser->graph->terms->dump(3);


# print $parser->graph;
=cut
