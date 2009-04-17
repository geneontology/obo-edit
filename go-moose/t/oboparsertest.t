use Test;

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
print $parser->graph;

my $writer = new OBO::Writers::OBOWriter;
$writer->graph($parser->graph);
$writer->write();


#use Moose::Autobox;
# print 'Print squares from 1 to 10 : ';
#  print [ 1 .. 10 ]->map(sub { $_ * $_ })->join(', ');
