use Test;

use OBO::Graph;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::NegatedStatement;
use OBO::Node;
use OBO::Parsers::OBOParser;
use FileHandle;


my $g = new OBO::Graph;
my $rn = new OBO::Node(id=>'part_of');
#my $s = new OBO::Statement(node=>$n);
#my $s = new OBO::Statement(node=>"x",relation=>'part_of');
my $s = new OBO::LinkStatement(node=>"x",relation=>$rn,target=>'y');
my $ns = new OBO::NegatedStatement(statement=>$s);

push(@{$g->statements}, $s);

print $g;
#use Data::Dumper;
#die Dumper $g;

my $fh = new FileHandle("t/data/cell.obo");
my $parser = new OBO::Parsers::OBOParser(fh=>$fh);
$parser->parse;
print $parser->graph;

#use Moose::Autobox;
# print 'Print squares from 1 to 10 : ';
#  print [ 1 .. 10 ]->map(sub { $_ * $_ })->join(', ');
