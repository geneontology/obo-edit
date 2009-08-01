#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
use Test::Deep::NoTest;
#plan tests => ;
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
$parser->reset_parser;
$parser = new GOBO::Parsers::GAFParser;
$parser->set_file("t/data/128up.gaf");
ok($parser->has_fh, "set_file used to initialize the fh");  #11
$parser->parse;
undef $g2_128up;
$g2_128up = $parser->graph;
is_deeply($g_128up, $g2_128up, "set_file: GAF parsers returned the same results"); # 12

my $fh = new FileHandle("t/data/AT1G49810.gaf");
$parser->reset_parser;
$parser->set_file($fh);
ok($parser->has_fh); # 13
$parser->parse;
undef $g2_AT1G;
$g2_AT1G = $parser->graph;
is_deeply($g_AT1G, $g2_AT1G, "set_file: GAF parsers returned the same results"); # 14

# using parse_file
$parser->reset_parser;
$parser->parse_file(file => new FileHandle("t/data/128up.gaf"));
ok($parser->has_fh); # 15
undef $g2_128up;
$g2_128up = $parser->graph;
is_deeply($g_128up, $g2_128up, "parse_file: GAF parsers returned the same results"); # 16

undef $parser;
$parser = new GOBO::Parsers::GAFParser;
#$parser->parse_file(file => "t/data/AT1G49810.gaf");
$parser->parse_file("t/data/AT1G49810.gaf");
ok($parser->has_fh); # 17
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
# this is a graph with everything in the known (obo) world in it.
my $obo_parser = new GOBO::Parsers::OBOParser(file=>'t/data/obo_file_2.obo');
$obo_parser->parse;
my $graph = $obo_parser->graph;
my $tags = {
	has_x => [ qw( nodes terms relations links declared_subsets annotations instances formulae ) ],
	body_only => [ qw( nodes terms relations links annotations instances formulae ) ],
	header_only => [ qw(default_namespace date comment format_version version property_value) ],
	both => [ qw(declared_subsets) ],
};

	
my $errs;
# check that we have these entities in our graph
foreach my $e (@{$tags->{has_x}})
{	my $fn = "has_$e";
#	print STDERR "fn: $fn; result of graph->fn: ". Dumper( $graph->$fn ) . "\n";
	push @$errs, $e if ! $graph->$fn;
}
ok( ! defined $errs, "Checking entities in the graph" );
print STDERR "Did not find the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;

my $g_keys;  # store the non-body stuff in g_keys
foreach my $k (keys %$graph)
{	next if grep { $k eq $_ } @{$tags->{body_only}};
	$g_keys->{$k} = $graph->{$k};
}

## let's try a few options now...

print STDERR "\n\n\nStarting options testing!\n";

# ignore body and header
$obo_parser->reset_parser;
$obo_parser->parse_file
(file=>'t/data/obo_file_2.obo', options => { body => { ignore => '*' }, header => { ignore => '*' } });

my $new_graph = $obo_parser->graph;
#print STDERR "ignore body and header graph: " . Dumper($new_graph);
isa_ok( $new_graph, "GOBO::Graph", "Ignoring body and header" );

undef $errs;
foreach my $e (@{$tags->{has_x}})
{	my $fn = "has_$e";
	push @$errs, $e if $new_graph->$fn;
}

ok( ! $errs, "Checking entities in the graph" );
print STDERR "Found the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;

$obo_parser->reset_parser;
$obo_parser->parse_file(file=>'t/data/obo_file_2.obo', options => { header => { ignore => '*' } });
$new_graph = $obo_parser->graph;
print STDERR "ignoring headers\n";

#foreach my $e (@{$tags->{has_x}})
#{	print STDERR "graph->$e: " . Dumper( $graph->$e ) . "\n";
#}

undef $errs;
foreach my $e (@{$tags->{has_x}})
{	push @$errs, $e if scalar @{ $new_graph->$e } != scalar @{ $graph->$e };
}
ok( ! defined $errs, "Ignored header: checking body elements" );
if ($errs && @$errs)
{	print STDERR "Discrepancies in the following: " . join(", ", @$errs ) . "\n";
}



undef $errs;
foreach my $e (@{$tags->{header_only}})
{	push @$errs, $e if $new_graph->{$e};
}

ok( ! defined $errs, "Ignored header: checking header elements" );
print STDERR "Found the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;


$obo_parser->reset_parser;
$obo_parser->parse_file(file=>'t/data/obo_file_2.obo', options => { body => { ignore => '*' } });
$new_graph = $obo_parser->graph;

undef $errs;
foreach my $e (@{$tags->{body_only}})
{	my $fn = "has_$e";
	push @$errs, $e if $new_graph->$fn;
}

ok( ! defined $errs, "Ignored body: checking body elements" );
print STDERR "Found the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;

undef $errs;
foreach my $e (@{$tags->{header_only}}, @{$tags->{both}})
{	if (! eq_deeply( $graph->{$e}, $new_graph->{$e} ))
	{	push @$errs, $e;
	}
}
ok( ! defined $errs, "Ignored body: checking header elements" );
print STDERR "Found the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;


## ignore everything except the instance and typedef stanza
$obo_parser->reset_parser;
$obo_parser->parse_file(file=>'t/data/obo_file.obo', options => { body => { parse_only => { instance => '*', annotation => '*' } } });

$new_graph = $obo_parser->graph;

foreach my $e (@{$tags->{body_only}})
{	my $fn = "has_$e";
	push @$errs, $e if $new_graph->$fn;
}

ok( ! defined $errs, "Parse only instances and annotations: checking body elements" );
print STDERR "Found the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;


## ignore everything except the instance and typedef stanza
$obo_parser->reset_parser;
$obo_parser->parse_file(file=>'t/data/transporters.obo', options => { body => { parse_only => { term => ['id', 'namespace', 'synonym'] } } });

$new_graph = $obo_parser->graph;

foreach my $e (@{$tags->{body_only}})
{	next if $e eq 'terms' || $e eq 'nodes';
	my $fn = "has_$e";
	push @$errs, $e if $new_graph->$fn;
}
ok( ! defined $errs, "Parse only term ids, names and namespaces: checking body elements" );
print STDERR "Found the following entities: " . join(", ", @$errs) . "\n" if $errs && @$errs;


## ignore everything except the instance and typedef stanza
$obo_parser->reset_parser;
$obo_parser->parse_file(file=>'t/data/obo_file_2.obo', options => { body => { ignore => { term => ['is_a', 'relationship', 'synonym' ] } } });


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