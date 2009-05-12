package OBO::Writers::CustomizableWriter;
use Moose;
use strict;
extends 'OBO::Writers::Writer';
use OBO::Node;
use OBO::Gene;
use OBO::Evidence;
use OBO::Annotation;

has delimiter => ( is=>'rw', isa=>'Str', default=>sub{"\t"});
has column_writer_index => ( is=>'rw', isa=>'HashRef', 
                            default=>sub{{}});

sub write_header {
    my $self = shift;
    my $g = $self->graph;

    return;
}

sub write_annotation {
    my $self = shift;
    my $ann = shift;

    my @fields =
        qw(node target provenance evidence source);
    foreach (@fields) {
        $self->print($self->fmt_obj($_, $ann->$_()));
        $self->print($self->delimiter);
    }
    $self->nl;
    return;
}

sub fmt_obj {
    my $self = shift;
    my $tag = shift;
    my $obj = shift;
    my $w = $self->column_writer_index->{$tag};
    if ($w) {
        return $w->($obj);
    }
    else {
        if ($obj->isa('OBO::Evidence')) {
            return $obj->type .' '. join('|', map {$self->fmt_obj('with',$_)} @{$obj->supporting_entities || []});
        }
        if ($obj->isa('OBO::Node')) {
            return $obj->label ?
                sprintf('%s "%s"', $obj->id || '', $obj->label || '') : $obj->id;
        }
        else {
            return $obj;
        }
    }
}

sub write_body {
    my $self = shift;
    my $g = $self->graph;

    return;
}


1;
