use Test;
plan tests => 1;
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

# NOT YET A TRUE TEST

my $fh = new FileHandle("t/data/so-xp.obo");
my $parser = new OBO::Parsers::OBOParser(fh=>$fh);
$parser->parse;
my $g = $parser->graph;

$g->convert_intersection_links_to_logical_definitions();

my $n = 0;
foreach my $term (@{$g->terms}) {
    if ($term->logical_definition) {
        printf "%s equivalent_to [ %s ]\n", $term, $term->logical_definition;
        $n++;
    }
}
printf "total: %d\n", $n;
ok($n == 193);

#use Moose::Autobox;
# print 'Print squares from 1 to 10 : ';
#  print [ 1 .. 10 ]->map(sub { $_ * $_ })->join(', ');
