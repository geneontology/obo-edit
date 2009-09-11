#!/usr/bin/perl

use GOBO::Graph;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::OBOParser;
use GOBO::Writers::CouchDBWriter;
use FileHandle;
use Data::Dumper;

my $db_url = shift;
my $f = shift;
my $parser = new GOBO::Parsers::OBOParser(file=>$f);
$parser->parse;
my $writer = new GOBO::Writers::CouchDBWriter(graph=>$parser->graph);
$writer->db_url($db_url);
$writer->write;
