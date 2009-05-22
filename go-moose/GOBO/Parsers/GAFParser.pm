package GOBO::Parsers::GAFParser;
use Moose;
use strict;
extends 'GOBO::Parsers::Parser';
use GOBO::Node;
use GOBO::Gene;
use GOBO::Evidence;
use GOBO::Annotation;
use GOBO::ClassExpression;

sub parse_header {
    my $self = shift;
    my $g = $self->graph;

    while($_ = $self->next_line) {
        if (/^\!.*/) {
        }
        else {
            $self->unshift_line($_);
            return;
        }
    }
    # odd..
    return;
}

sub parse_body {
    my $self = shift;
    my $g = $self->graph;

    while($_ = $self->next_line) {
        chomp;
        my @vals = split(/\t/);
        my ($genedb,
            $geneacc,
            $genesymbol,
            $qualifier,
            $termacc,
            $ref,
            $evcode,
            $with,
            $aspect,
            $genename,
            $genesyn,
            $genetype,
            $genetaxa,
            $assocdate,
            $source_db,
            $annotxp,   # experimental! 
            $geneproduct
            ) = @vals;

        my $geneid = "$genedb:$geneacc";
        my $gene = $g->noderef($geneid);
        my @taxa = split(/[\|\;]/,$genetaxa);
        my $taxon = shift @taxa;
        if (!$gene->label) {
            bless $gene, 'GOBO::Gene';
            $gene->label($genesymbol);
            $gene->add_synonyms(split(/\|/,$genesyn));
            # TODO; split
            $gene->taxon($g->noderef($taxon));
            $gene->type($g->noderef($genetype));
        }
        my $cnode = $g->term_noderef($termacc);
        if (!$cnode->namespace) {
            $cnode->namespace(_aspect2ns($aspect));
        }

        my %qualh = map {lc($_)=>1} (split(/[\|]\s*/,$qualifier || ''));
        my $ev = new GOBO::Evidence(type=>$g->term_noderef($evcode));
        # TODO: discriminate between pipes and commas
        # (semicolon is there for legacy reasons - check if this can be removed)
        my @with_objs = map {$g->noderef($_)} split(/\s*[\|\;\,]\s*/, $with);
        $ev->supporting_entities(\@with_objs);
        my @refs = split(/\|/,$ref);
        my $provenance = $g->noderef(pop @refs); # last is usually PMID
        $provenance->add_xrefs([@refs]);
        my $annot = 
            new GOBO::Annotation(node=>$gene,
                                target=>$cnode,
                                provenance=>$provenance,
                                source=>$g->noderef($source_db),
                                date=>$assocdate,
            );
        if ($geneproduct) {
            $annot->specific_node($g->noderef($geneproduct));
        }
        $annot->evidence($ev);
        if ($qualh{not}) {
            # TODO
        }
        if ($annotxp) {
            my $xp = GOBO::ClassExpression->parse_idexpr($g,$annotxp);
            # TODO:
        }
        $g->add_annotation($annot);
        #push(@{$g->annotations},$annot);
    }
    return;
}

# the following is specific to GO
sub _aspect2ns {
    my $aspect = shift;
    return 'molecular_function' if $aspect eq 'F';
    return 'biological_process' if $aspect eq 'P';
    return 'cellular_component' if $aspect eq 'C';
}

1;
