package OBO::Parsers::OBOParser;
use Moose;
use strict;
extends 'OBO::Parsers::Parser';
use OBO::Node;
use OBO::LinkStatement;

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

    $/ = "\n\n";
    while(my $stanza = $self->next_line) {
        my @lines = split(/\n/,$stanza);
        my $stanzaclass;
        my $id;
        my $n;
        foreach  (@lines) {
            chomp;
            s/\!.*//; # TODO
            s/\s+$//;
            my $vals = [];
            if (/^\[(\S+)\]/) {
                $stanzaclass = lc($1);
            }
            elsif (/^id:\s*(.*)/) {
                $id = $1;
                $n = $g->term_noderef($id);
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
            elsif (/^xref:\s*(\S+)/) {
                $n->add_xrefs($1);
            }
            elsif (/^is_a:\s*(\S+)/) {
                my $tn = $g->term_noderef($1);
                my $s = new OBO::LinkStatement(node=>$n,relation=>'is_a',target=>$tn);
                push(@{$g->links},$s);
            }
            elsif (/^relationship:\s*(\S+)\s+(\S+)/) {
                my $rn = $g->noderef($1);
                my $tn = $g->term_noderef($2);
                my $s = new OBO::LinkStatement(node=>$n,relation=>$rn,target=>$tn);
                push(@{$g->links},$s);
            }
            else {
                # ...
            }
        }
    }
    $/ = "\n";
    return;
}

sub _parse_vals {
    my $s = shift;
    my $vals = shift;
    if ($s =~ /^(\".*)/) {
        $s = _parse_quoted($s,$vals);
    }
    while ($s =~ /^([^\{\[]\S*)\s*(.*)/) {
        push(@$vals,$1);
        $s = $2;
    }
    if ($s =~ /^(\[)/) {
        $s = _parse_xrefs($s,$vals);
    }
}

sub _parse_quoted {
    my $s = shift;
    my $vals = shift;
    if ($s =~ /^\"(([^\"\\]|\\.)*)\"\s*(.*)/) {
        push(@$vals,$1);
        return $2;
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
        push(@$vals, {xrefs=>split(/,/,$1)}); # TODO
    }
    else {
        die "$s";
    }
}

1;
