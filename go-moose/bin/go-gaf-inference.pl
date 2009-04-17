#!/usr/bin/perl

use OBO::Graph;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::Annotation;
use OBO::Node;
use OBO::Parsers::GAFParser;
use OBO::Parsers::OBOParser;
use OBO::Writers::GAFWriter;
use DateTime;
use FileHandle;

my $ontf;
my %relh = ();
while ($ARGV[0] =~ /^\-/) {
    my $opt = shift @ARGV;
    if ($opt eq '-i' || $opt eq '--ontology') {
        $ontf = shift;
    }
    elsif ($opt eq '-r' || $opt eq '--relation') {
        $relh{shift @ARGV} = 1;
    }
    else {
        die "no such opt: $opt";
    }
}
if (!$ontf) {
    $ontf = shift;
}
my $obo_parser = new OBO::Parsers::OBOParser(file=>$ontf);
$obo_parser->parse;
my $ontg = $obo_parser->graph;
my %iolink = ();
foreach my $link (@{$ontg->links}) {
    my $node = $link->node;
    my $target = $link->target;
    if ($link->node->namespace ne $link->target->namespace) {
        if (%relh) {
            if (!$relh{$link->relation->id}) {
                next;
            }
        }
        $iolink{$node->id} = $target->id;
        #printf STDERR "%s -> %s\n", $node->namespace, $target->namespace;
    }
}

my @ics = ();
my $got_h = ();
foreach my $f (@ARGV) {
    my $parser = new OBO::Parsers::GAFParser(file=>$f);
    while ($parser->parse_chunk(10000)) {
        push(@ics, infer_annotations($parser->graph->annotations));
        $parser->graph(new OBO::Graph);
    }
    my $icgraph = new OBO::Graph();
    my @ic_anns =
        map {
            new OBO::Annotation(node => $_->[0],
                                target => $ontg->term_noderef($iolink{$_->[1]}),
                                provenance => $_->[2],
                                evidence => new OBO::Evidence(type=>$ontg->term_noderef('IC'),
                                                              supporting_entities => [$_->[1]]),
                                source=>'GOC',
                                date=>DateTime->today)
    } @ics;
    $icgraph->annotations(\@ic_anns);
    my $w = new OBO::Writers::GAFWriter;
    $w->graph($icgraph);
    $w->write;
}
exit 0;

sub infer_annotations {
    my $anns = shift;
    my @ics = ();
    foreach my $ann (@$anns) {
        my $t = $ann->target;
        my $gene = $ann->node;
        if ($iolink{$t}) {
            if (!$got_h{$gene}{$t}) {
                push(@ics, [$gene,$t,$ann->provenance]);
                #printf STDERR "$gene $t\n";
            }
            $got_h{$gene}{$t} = 1;
        }
    }
    return @ics;
}
