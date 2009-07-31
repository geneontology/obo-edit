#!/usr/bin/perl -w

use strict;
use Test::More;
plan tests => 42;
use GOBO::Graph;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::GAFParser;
use GOBO::Parsers::OBOParser;
use FileHandle;
use Data::Dumper;


# tests 1 - 10
my $parser = new GOBO::Parsers::GAFParser(file=>new FileHandle("t/data/128up.gaf"));
ok($parser->has_fh);
$parser->parse;
my $g_128up = $parser->graph;
ok($g_128up);

$parser = new GOBO::Parsers::GAFParser(file=>"t/data/AT1G49810.gaf");
ok($parser->has_fh);
$parser->parse;
my $g_AT1G = $parser->graph;
ok($g_AT1G);


$parser = new GOBO::Parsers::GAFParser(fh=>"t/data/128up.gaf");
ok($parser->has_fh);
$parser->parse;
my $g2_128up = $parser->graph;
ok($g2_128up);

is_deeply($g_128up, $g2_128up, "Checking that the GAF parsers returned the same results");

$parser = new GOBO::Parsers::GAFParser(fh=>new FileHandle("t/data/AT1G49810.gaf") );
ok($parser->has_fh);
$parser->parse;
my $g2_AT1G = $parser->graph;
ok($g2_AT1G);
is_deeply($g_AT1G, $g2_AT1G, "Checking that the GAF parsers returned the same results");


## setting the file using set_file
$parser->clear_graph;
$parser = new GOBO::Parsers::GAFParser;
$parser->set_file("t/data/128up.gaf");
ok($parser->has_fh, "set_file used to initialize the fh");  #11
$parser->parse;
undef $g2_128up;
$g2_128up = $parser->graph;
is_deeply($g_128up, $g2_128up, "set_file: GAF parsers returned the same results"); # 12

my $fh = new FileHandle("t/data/AT1G49810.gaf");
$parser->clear_graph;
$parser->set_file($fh);
ok($parser->has_fh); # 13
$parser->parse;
undef $g2_AT1G;
$g2_AT1G = $parser->graph;
is_deeply($g_AT1G, $g2_AT1G, "set_file: GAF parsers returned the same results"); # 14

# using parse_file
$parser->clear_graph;
$parser->parse_file(file => new FileHandle("t/data/128up.gaf"));
ok($parser->has_fh); # 15
$parser->parse;
undef $g2_128up;
$g2_128up = $parser->graph;
is_deeply($g_128up, $g2_128up, "parse_file: GAF parsers returned the same results"); # 16

undef $parser;
$parser = new GOBO::Parsers::GAFParser;
#$parser->parse_file(file => "t/data/AT1G49810.gaf");
$parser->parse_file("t/data/AT1G49810.gaf");
ok($parser->has_fh); # 17
$parser->parse;
undef $g2_AT1G;
$g2_AT1G = $parser->graph;
is_deeply($g_AT1G, $g2_AT1G, "parse_file: GAF parsers returned the same results"); # 18



eval { $parser = new GOBO::Parsers::OBOParser(file=>'/a/load/of/bollox'); };
ok( defined $@ );

#check the OBOParser quickly...
$parser = new GOBO::Parsers::OBOParser(file=>'t/data/gtp.obo');
ok($parser->has_fh);
$parser->parse;
ok( $parser->graph->has_terms, "Checking there are terms in the graph");



## OK, basics done. Let's try a bit of parsing...
# this is the standard graph from so-xp.obo
my $obo_parser = new GOBO::Parsers::OBOParser(file=>'t/data/obo_file_2.obo');
$obo_parser->parse;
my $graph = $obo_parser->graph;

my @known_arr = qw( annotation_ix formulae instance_h link_ix node_index relation_h subset_index term_h );

my $g_keys;
foreach my $k (keys %$graph)
{	next if grep { $k eq $_ } @known_arr;
	print STDERR "$k: " . $graph->{$k} . "\n";
	$g_keys->{$k} = $graph->{$k};
}


foreach my $e qw( relations terms links subsets nodes annotations instances ) #formulae )
{	my $fn = "has_$e";
#	print STDERR "fn: $fn; result of graph->fn: ". Dumper( $graph->$fn ) . "\n";
	cmp_ok($graph->$fn, '==', 1, "Checking for $e");
}

## let's try a few options now...

# ignore body and header
$obo_parser->reset_parser;
#$obo_parser = new GOBO::Parsers::OBOParser
$obo_parser->parse_file
(file=>'t/data/obo_file_2.obo', options => { body => { ignore => '*' }, header => { ignore => '*' } });
print STDERR "parser: " . Dumper($obo_parser);

$obo_parser->parse;

my $new_graph = $obo_parser->graph;
print STDERR "ignore body and header graph: " . Dumper($new_graph);

foreach my $e qw( relations terms instances links annotations nodes subsets formulae )
{	my $fn = "has_$e";
	ok(! defined $new_graph->$fn, "Checking for $e");
}

$obo_parser->reset_parser;
$obo_parser->parse_file(file=>'t/data/obo_file_2.obo', options => { header => { ignore => '*' } });
$obo_parser->parse;
$new_graph = $obo_parser->graph;

foreach my $e qw( relations terms links nodes annotations instances ) # subsets formulae )
{	cmp_ok(scalar @{$new_graph->$e}, '==', scalar @{$graph->$e}, "Checking for $e");
}

print STDERR "graph: " . $new_graph->dump . "\n";

foreach my $k (keys %$new_graph)
{	next if grep { $k eq $_ } @known_arr;
	print STDERR "$k: " . $new_graph->{$k} . "\n";
	ok(0, "Found $k in the new graph");
}

$obo_parser->reset_parser;
$obo_parser->parse_file(file=>'t/data/obo_file_2.obo', options => { body => { ignore => '*' } });
$obo_parser->parse;
$new_graph = $obo_parser->graph;

foreach my $k (keys %$new_graph)
{	next if grep { $k eq $_ } @known_arr;
	print STDERR "$k: " . $new_graph->{$k} . "\n";
	ok(0, "Found $k in the new graph");
}

exit(0);


# print $parser->graph;
