use Test;

use GOBO::ClassExpression;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::GAFParser;
use GOBO::Graph;
use FileHandle;

my $g = new GOBO::Graph;
my $x = GOBO::ClassExpression->parse_idexpr($g, 'a^foo(bar)');
print "x=$x\n";
