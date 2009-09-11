#!/usr/bin/perl

use GOBO::Graph;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::OBOParser;
use GOBO::Writers::JSONWriter;
use FileHandle;
use Data::Dumper;

my $f = shift;
my $parser = new GOBO::Parsers::OBOParser(file=>$f);
$parser->parse;
my $writer = new GOBO::Writers::JSONWriter(graph=>$parser->graph);
$writer->write;
