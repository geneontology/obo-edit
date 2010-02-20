use Test;
plan tests => 4;
use strict;
use GOBO::Graph;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::OBOParser;
use GOBO::Writers::OBOWriter;
use GOBO::InferenceEngine;
use FileHandle;

my $fh = new FileHandle("t/data/negreltest.obo");
my $parser = new GOBO::Parsers::OBOParser(fh=>$fh);
$parser->parse;
my $g = $parser->graph;

my $ie = new GOBO::InferenceEngine(graph=>$g);
my $a = $g->noderef('a');

print "links for $a\n";
# expected:
#(a --[part_of]-->e)
#(a --[is_a]-->b)
#(a --[is_a]-->c)
#(a --[never_in_taxon "never_in_taxon"]-->U2)
#(a --[only_in_taxon "only_in_taxon"]-->V1)
#(a --[part_of]-->f)
#(a --[is_a]-->d)
#(a --[only_in_taxon "only_in_taxon"]-->W1)
#(a --[only_in_taxon "only_in_taxon"]-->U1)
my @links = @{$ie->get_inferred_target_links($a)};
ok(@links ==9);
foreach my $link (@links) {
    print $link,"\n";
    if ($link->relation->id eq 'only_in_taxon') {
	if ($link->target->id eq 'W1') {
	    ok(1);
	}
	if ($link->target->id eq 'V1') {
	    ok(1);
	}
    }
    if ($link->relation->id eq 'never_in_taxon') {
	ok($link->target->id ne 'V2');
    }
}
