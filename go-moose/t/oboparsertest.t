use Test::More tests => 4;
use strict;

use GOBO::Graph;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::OBOParser;
use GOBO::Writers::OBOWriter;
use FileHandle;


my $fh = new FileHandle("t/data/cell.obo");
my $parser = new GOBO::Parsers::OBOParser(fh=>$fh);

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

my $writer = new GOBO::Writers::OBOWriter;
$writer->graph($parser->graph);
$writer->write();

ok(1);

#use Moose::Autobox;
# print 'Print squares from 1 to 10 : ';
#  print [ 1 .. 10 ]->map(sub { $_ * $_ })->join(', ');
