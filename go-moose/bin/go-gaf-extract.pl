#!/usr/bin/perl
use strict;
use OBO::Graph;
use OBO::Statement;
use OBO::LinkStatement;
use OBO::Annotation;
use OBO::Node;
use OBO::Parsers::GAFParser;
use OBO::Parsers::OBOParser;
use OBO::Writers::GAFWriter;
use OBO::Writers::CustomizableWriter;
use OBO::InferenceEngine;
use OBO::InferenceEngine::GAFInferenceEngine;
use List::MoreUtils;
use Set::Object;
use DateTime;
use FileHandle;

my $ontf;
my %relh = ();
my @ids = ();
my $writer = new OBO::Writers::CustomizableWriter;
while ($ARGV[0] =~ /^\-/) {
    my $opt = shift @ARGV;
    if ($opt eq '-i' || $opt eq '--ontology') {
        $ontf = shift;
    }
    elsif ($opt eq '-r' || $opt eq '--relation') {
        $relh{shift @ARGV} = 1;
    }
    elsif ($opt eq '-t' || $opt eq '--term') {
        push(@ids, shift @ARGV);
    }
    elsif ($opt eq '-h' || $opt eq '--help') {
        system("perldoc $0");
        exit(0);
    }
    elsif ($opt eq '--gaf') {
        bless $writer, 'OBO::Writers::GAFWriter';
    }
    else {
        die "no such opt: $opt";
    }
}
if (!$ontf) {
    $ontf = shift;
}
if (!$ontf) {
    system("perldoc $0");
    exit(1);
}

#if (!@ids) {
#    print "Enter IDs:\n";
#    @ids = split(/\n/, <STDIN>);
#}
my $idset = new Set::Object;
$idset->insert(@ids);

my $obo_parser = new OBO::Parsers::OBOParser(file=>$ontf);
$obo_parser->parse;
my $ontg = $obo_parser->graph;
my $ie = new OBO::InferenceEngine::GAFInferenceEngine(graph=>$ontg);


# iterate through annotations writing new ICs
my @ics = ();
foreach my $f (@ARGV) {
    my $gafparser = new OBO::Parsers::GAFParser(file=>$f);
    $gafparser->graph($ontg);
    $ontg->annotations([]);
    # iterate through one chunk at a time
    while ($gafparser->parse_chunk(10000)) {
        foreach my $ann (@{$gafparser->graph->annotations}) {
            if (@ids) {
                my $rset = new Set::Object;
                $rset->insert(map {$_->id} @{$ie->get_inferred_target_nodes($ann->target)});
                if ($rset->intersection($idset)->size) {
                    show_ann($ann);
                }
            }
            else {
                show_ann($ann);
            }
        }
        # clear
        $gafparser->graph(new OBO::Graph);
    }
}
exit 0;

# TODO: use a general customizable writer class
sub show_ann {
    my $ann = shift;
    #printf "%s %s %s %s\n", $ann->node, $ann->evidence, $ann->target->id, $ann->target->label;
    $writer->write_annotation($ann);
}



=head1 NAME

go-gaf-extract.pl

=head1 SYNOPSIS

  # extract nucleotide binding terms
  go-gaf-extract.pl -t GO:0000166 t/data/gtp.obo t/data/test-fb.gaf 

=head1 DESCRIPTION

Extracts all annotations to a term, including annotations inferred from graph

=cut
