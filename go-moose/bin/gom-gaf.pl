#!/usr/bin/perl

use OBO::Graph;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::NegatedStatement;
use OBO::Node;
use OBO::Parsers::GAFParser;
use OBO::Writers::GAFWriter;
use FileHandle;

my $f = shift;
#my $fh = new FileHandle("gzip -dc $f|");
my $parser = new OBO::Parsers::GAFParser(file=>$f);
$parser->parse;
#print $parser->graph;
my $writer = new OBO::Writers::GAFWriter(graph=>$parser->graph);
$writer->write;
