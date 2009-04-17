use Test;

use OBO::Graph;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::NegatedStatement;
use OBO::Node;
use OBO::Parsers::GAFParser;
use OBO::Parsers::OBOParser;
use FileHandle;

my $parser = new OBO::Parsers::GAFParser(fh=>new FileHandle("t/data/test-fb.gaf"));
$parser->parse;
my $g = $parser->graph;
$parser = new OBO::Parsers::OBOParser(fh=>new FileHandle("t/data/gene_ontology_write.obo"));
$parser->graph($g);
$parser->parse;
print $parser->graph;
