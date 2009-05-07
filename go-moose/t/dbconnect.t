use Test;
plan tests => 4;
use strict;
use OBO::Graph;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::NegatedStatement;
use OBO::Node;
use OBO::Parsers::OBOParser;
use OBO::Writers::OBOWriter;
use OBO::InferenceEngine;
use FileHandle;

my $g = new OBO::Graph;
bless $g->link_ix, 'OBO::Amigo::Indexes::AmigoStatementIndex';
$g->link_ix->schema( new Amigo::Model::Schema($dbh) );

# TODO...
