use Test;

use OBO::Graph;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::NegatedStatement;
use OBO::Node;
use OBO::Parsers::GAFParser;
use FileHandle;

my $fh = new FileHandle("t/data/test-fb.gaf");
my $parser = new OBO::Parsers::GAFParser(fh=>$fh);
$parser->parse;
print $parser->graph;
