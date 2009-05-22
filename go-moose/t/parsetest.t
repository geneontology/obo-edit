use Test;

use GOBO::Graph;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::GAFParser;
use GOBO::Parsers::OBOParser;
use FileHandle;

my $parser = new GOBO::Parsers::GAFParser(fh=>new FileHandle("t/data/test-fb.gaf"));
$parser->parse;
my $g = $parser->graph;
$parser = new GOBO::Parsers::OBOParser(fh=>new FileHandle("t/data/gene_ontology_write.obo"));
$parser->graph($g);
$parser->parse;
print $parser->graph;
