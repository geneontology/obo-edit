use Test;
plan tests => 4;
use strict;
use GOBO::Graph;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::OBOParser;
use GOBO::Writers::OBOWriter;
use GOBO::InferenceEngine;
use FileHandle;

my $g = new GOBO::Graph;
my $dbh; # connect to a test db TODO
#bless $g->link_ix, 'GOBO::Amigo::Indexes::AmigoStatementIndex';
bless $g, 'GOBO::Amigo::Indexes::AmigoStatementIndex';
$g->link_ix->schema( new Amigo::Model::Schema($dbh) );




# TODO...
