#!/usr/bin/perl

use strict;
use GOBO::Graph;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::OBOParser;
use GOBO::Writers::OBOWriter;
use GOBO::Util::LuceneIndexer;
use FileHandle;

my $f = shift;
my $parser = new GOBO::Parsers::OBOParser(file=>$f);
$parser->parse;

my $loo = new GOBO::Util::LuceneIndexer();
$loo->target_dir('lucene_index');
$loo->init();
$loo->index_terms($parser->graph->terms);
