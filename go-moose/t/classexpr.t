use Test;

use OBO::ClassExpression;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::NegatedStatement;
use OBO::Node;
use OBO::Parsers::GAFParser;
use OBO::Graph;
use FileHandle;

my $g = new OBO::Graph;
my $x = OBO::ClassExpression->parse_idexpr($g, 'a^foo(bar)');
print "x=$x\n";
