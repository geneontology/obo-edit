use Test;
plan tests => 3;
use strict;

use OBO::Graph;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::NegatedStatement;
use OBO::Node;
use OBO::Parsers::OBOParser;
use OBO::Writers::OBOWriter;
use FileHandle;


my $fh = new FileHandle("t/data/cell.obo");
my $parser = new OBO::Parsers::OBOParser(fh=>$fh);

$parser->parse;

my $g = $parser->graph;
my $neuron = $g->noderef('CL:0000540');
ok($neuron->label eq 'neuron');

my $n_links = scalar(@{$g->links});
print "links: $n_links\n";
ok($n_links > 0);
#print $g;

print "neuron: $neuron\n";
use Data::Dumper;
print Dumper $g->link_ix->ixN->{$neuron->id};

my $pls = $g->get_target_links($neuron);
print "pls=@$pls\n";
ok(@$pls>1);

my $writer = new OBO::Writers::OBOWriter;
$writer->graph($parser->graph);
$writer->write();


#use Moose::Autobox;
# print 'Print squares from 1 to 10 : ';
#  print [ 1 .. 10 ]->map(sub { $_ * $_ })->join(', ');
