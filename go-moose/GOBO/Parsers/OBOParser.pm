package GOBO::Parsers::OBOParser;
use Moose;
use strict;
extends 'GOBO::Parsers::Parser';
use GOBO::Node;
use GOBO::InstanceNode;
use GOBO::Synonym;
use GOBO::LinkStatement;
use GOBO::LiteralStatement;
use GOBO::ClassExpression;
use GOBO::ClassExpression::Union;

has default_namespace => (is=>'rw', isa=>'Str');

sub parse_header {
    my $self = shift;
    my $g = $self->graph;

    $/ = "\n";
    while($_ = $self->next_line) {
        if (/^\[/) {
            $self->unshift_line($_);
            return;
        }
        chomp;
        if (/^(\S+):\s*(.*)/) {
            if ($1 eq 'default-namespace') {
                $self->default_namespace($2);
            }
        }
    }
    # odd..
    return;
}

sub parse_body {
    my $self = shift;
    my $g = $self->graph;

    my $stanzaclass;
    my $id;
    my $n;
    my %union_h = ();
    my @anns = ();
    while($_ = $self->next_line) {
        chomp;
        s/\!.*//; # TODO
        s/\s+$//;
        my $vals = [];
        if (/^\[(\S+)\]/) {
            $stanzaclass = lc($1);
            if ($stanzaclass eq 'annotation') {
                $n = new GOBO::Annotation;
                push(@anns, $n);
            }
        }
        elsif (/^id:\s*(.*)/) {
            $id = $1;
            if ($stanzaclass eq 'term') {
                $n = $g->term_noderef($id);
                $g->add_term($n);
            }
            elsif ($stanzaclass eq 'typedef') {
                $n = $g->relation_noderef($id);
                $g->add_relation($n);
            }
            elsif ($stanzaclass eq 'instance') {
                $n = $g->instance_noderef($id);
                $g->add_instance($n);
            }
            elsif ($stanzaclass eq 'annotation') {
                # TODO
            }
            else {
            }

            if (!$n) {
                die "cannot parse: $_";
            }
            
            $n->namespace($self->default_namespace)
                if (!$n->namespace &&
                    $self->default_namespace);
        }
        elsif (/^name:\s*(.*)/) {
            $n->label($1);
        }
        elsif (/^namespace:\s*(.*)/) {
            $n->namespace($1);
        }
        elsif (/^def:\s*(.*)/) {
            _parse_vals($1,$vals);
            $n->definition($vals->[0]); # TODO
        }
        elsif (/^synonym:\s*(.*)/) {
            _parse_vals($1,$vals);
            my $syn = new GOBO::Synonym(label=>shift @$vals);
            $n->add_synonym($syn);
            my $xrefs = pop @$vals;
            if (@$vals) {
                $syn->scope(shift @$vals);
            }
            else {
                warn "no scope specified: $_";
            }
            if ($vals->[0] && !ref($vals->[0])) {
                $syn->type(shift @$vals);
            }
            $syn->xrefs($xrefs);
        }
        elsif (/^xref:\s*(\S+)/) {
            $n->add_xrefs($1);
        }
        elsif (/^is_a:\s*(\S+)(.*)/) {
            #my $tn = $stanzaclass eq 'typedef' ? $g->relation_noderef($1) : $g->term_noderef($1);
            my $tn = $self->getnode($1, $stanzaclass eq 'typedef' ? 'r' : 'c');
            my $s = new GOBO::LinkStatement(node=>$n,relation=>'is_a',target=>$tn);
            $self->add_metadata($s,$2);
            $g->add_link($s);
        }
        elsif (/^complement_of:\s*(\S+)/) {
            my $tn = $self->getnode($1, $stanzaclass eq 'typedef' ? 'r' : 'c');
            $n->complement_of($tn);
        }
        elsif (/^relationship:\s*(\S+)\s+(\S+)/) {
            my $rn = $g->relation_noderef($1);
            #my $tn = $stanzaclass eq 'typedef' ? $g->relation_noderef($2) : $g->term_noderef($2);
            my $tn = $self->getnode($2, $stanzaclass eq 'typedef' ? 'r' : 'c');
            #my $tn = $g->term_noderef($2);
            my $s = new GOBO::LinkStatement(node=>$n,relation=>$rn,target=>$tn);
            $g->add_link($s);
        }
        elsif (/^intersection_of:/) {
            # TODO: generalize
            if (/^intersection_of:\s*(\S+)\s+(\S+)/) {
                my $rn = $g->relation_noderef($1);
                #my $tn = $g->term_noderef($2);
                my $tn = $self->getnode($2, $stanzaclass eq 'typedef' ? 'r' : 'c');
                #my $tn = $stanzaclass eq 'typedef' ? $g->relation_noderef($2) : $g->term_noderef($2);
                my $s = new GOBO::LinkStatement(node=>$n,relation=>$rn,target=>$tn, is_intersection=>1);
                $g->add_link($s);
            }
            elsif (/^intersection_of:\s*(\S+)/) {
                #my $tn = $g->term_noderef($1);
                #my $tn = $stanzaclass eq 'typedef' ? $g->relation_noderef($1) : $g->term_noderef($1);
                my $tn = $self->getnode($1, $stanzaclass eq 'typedef' ? 'r' : 'c');
                my $s = new GOBO::LinkStatement(node=>$n,relation=>'is_a',target=>$tn, is_intersection=>1);
                $g->add_link($s);
            }
            else {
                $self->throw("badly formatted intersection: $_");
            }
        }
        elsif (/^union_of:\s*(\S+)/) {
            my $u = $self->getnode($1, $stanzaclass eq 'typedef' ? 'r' : 'c');
            my $ud = $n->union_definition;
            if (!$ud) {
                $ud = new GOBO::ClassExpression::Union;
                $n->union_definition($ud);
            }
            $ud->add_argument($u);
        }
        elsif (/^is_(\w+):\s*(\w+)/) {
            my $att = $1;
            my $val = $2 eq 'true';
            $n->$att($val); # TODO : check
            #$n->{$att} = $val; # TODO : check
        }
        elsif (/^transitive_over:\s*(\W+)/) {
            my $rn = $g->relation_noderef($1);
            $n->transitive_over($rn);
        }
        elsif (/^(holds_over_chain|equivalent_to_chain):\s*(.*)/) {
            my $ct = $1;
            my @rels  = map { $self->getnode($_,'r') } split(' ',$2);
            $ct eq 'holds_over_chain' ? $n->add_holds_over_chain(\@rels) : $n->add_equivalent_to_chain(\@rels);
        }
        # following for annotation stanzas only
        elsif (/^subject:\s*(.*)/) {
            $n->node($self->getnode($1));
        }
        elsif (/^relation:\s*(.*)/) {
            $n->relation($self->getnode($1,'r'));
        }
        elsif (/^object:\s*(.*)/) {
            $n->target($self->getnode($1));
        }
        elsif (/^description:\s*(.*)/) {
            $n->description($1);
        }
        elsif (/^source:\s*(.*)/) {
            $n->provenance($self->getnode($1));
        }
        elsif (/^assigned_by:\s*(.*)/) {
            $n->source($self->getnode($1));
        }
        else {
            # ...
        }
    }
    if (@anns) {
        $g->add_annotations(\@anns);
    }
    return;
}

sub getnode {
    my $self = shift;
    my $id = shift;
    my $metatype = shift || '';
    my $g = $self->graph;
    my $n;
    if ($metatype eq 'c') {
        $n = $g->term_noderef($id);
    }
    elsif ($metatype eq 'r') {
        $n = $g->relation_noderef($id);
    }
    elsif ($metatype eq 'i') {
        $n = $g->instance_noderef($id);
    }
    else {
        $n = $g->noderef($id);
    }
    return $n;
}

sub add_metadata {
    my $self = shift;
    my $s = shift;
    my $v = shift;
    if ($v =~ /^\s*\{(.*)\}/) {
        my $tq = $1;
        my @tvs = ();
        while ($tq) {
            if ($tq =~ /(\w+)=\"([^\"]*)\"(.*)/) {
                push(@tvs,[$1,$2]);
                $tq = $3;
            }
            elsif ($tq =~ /(\w+)=(\w+)(.*)/) {
                push(@tvs,[$1,$2]);
                $tq = $3;
            }
            else {
                $self->throw($v);
            }
            if ($tq =~ /^s*\,\s*(.*)/) {
                $tq = $1;
            }
            elsif ($tq =~ /^\s*$/) {
                # ok
            }
            else {
                $self->throw($v);
            }
        }
        my @sub_statements = ();
        foreach (@tvs) {
            my ($t,$v) = @$_;
            my $ss = new GOBO::LiteralStatement(relation=>$t,target=>$v);
            push(@sub_statements,$ss);
        }
        $s->sub_statements(\@sub_statements);
    }
    return;
}

sub _parse_vals {
    my $s = shift;
    my $vals = shift;

    # optionally leads with quoted sections
    if ($s =~ /^(\".*)/) {
        $s = _parse_quoted($s,$vals);
    }

    # follows with optional list of atoms
    while ($s =~ /^([^\{\[]\S*)\s*(.*)/) {
        push(@$vals,$1);
        $s = $2;
    }

    # option xrefs
    if ($s =~ /^(\[)/) {
        $s = _parse_xrefs($s,$vals);
    }
}

sub _parse_quoted {
    my $s = shift;
    my $vals = shift;
    if ($s =~ /^\"(([^\"\\]|\\.)*)\"\s*(.*)/) {
        push(@$vals,$1);
        return $3;
    }
    else {
        die "$s";
    }
}

sub _parse_xrefs {
    my $s = shift;
    my $vals = shift;
    if ($s =~ /^\[(([^\]\\]|\\.)*)\]\s*(.*)/) {
        $s = $2;
        push(@$vals, [split(/,/,$1)]); # TODO
    }
    else {
        die "$s";
    }
}

1;

=head1 NAME

GOBO::Parsers::OBOParser

=head1 SYNOPSIS

  my $fh = new FileHandle("t/data/cell.obo");
  my $parser = new GOBO::Parsers::OBOParser(fh=>$fh);
  $parser->parse;
  print $parser->graph;

  my $writer = new GOBO::Writers::OBOWriter;
  $writer->graph($parser->graph);
  $writer->write();

=head1 DESCRIPTION

An GOBO::Parsers::Parser that parses OBO Files.

The goal is to be obof1.3 compliant:

http://www.geneontology.org/GO.format.obo-1_3.shtml

however, obof1.2 and obof1.0 are also supported

=head2 Term stanzas

These are converted to GOBO::TermNode objects

=head2 Typedef stanzas

These are converted to GOBO::RelationNode objects

=head2 Instance stanzas

These are converted to GOBO::InstanceNode objects

=head2 Statements

is_a and relationship tags are converted to GOBO::LinkStatement objects and added to the graph

=head2 intersection_of tags

These are added to the graph as GOBO::LinkStatement objects, with is_intersection=>1

You can call 

  $g->convert_intersection_links_to_logical_definitions

To move these links from the graph to $term->logical_definition

TBD: do this as the default?
TBD: generalize for all links? sometimes it is convenient to have the links available in the Node object...?

=cut
