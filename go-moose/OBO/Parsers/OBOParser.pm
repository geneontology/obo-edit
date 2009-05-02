package OBO::Parsers::OBOParser;
use Moose;
use strict;
extends 'OBO::Parsers::Parser';
use OBO::Node;
use OBO::Synonym;
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

    my $stanzaclass;
    my $id;
    my $n;
    while($_ = $self->next_line) {
        chomp;
        s/\!.*//; # TODO
        s/\s+$//;
        my $vals = [];
        if (/^\[(\S+)\]/) {
            $stanzaclass = lc($1);
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
            my $syn = new OBO::Synonym(label=>shift @$vals);
            $n->add_synonym($syn);
            $syn->scope(shift @$vals);
            if ($vals->[0] && !ref($vals->[0])) {
                $syn->type(shift @$vals);
            }
            my $xrefs = shift @$vals;
            $syn->xrefs($xrefs);
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
            my $rn = $g->relation_noderef($1);
            my $tn = $g->term_noderef($2);
            my $s = new OBO::LinkStatement(node=>$n,relation=>$rn,target=>$tn);
            push(@{$g->links},$s);
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
        else {
            # ...
        }
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
