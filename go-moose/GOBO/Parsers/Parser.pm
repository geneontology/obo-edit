package GOBO::Parsers::Parser;
use Moose;
use strict;
use GOBO::Graph;
use FileHandle;
use Carp;

has fh => (is=>'rw', isa=>'FileHandle');
has file => (is=>'rw', isa=>'Str');
has graph => (is=>'rw', isa=>'GOBO::Graph', default=>sub{new GOBO::Graph});
has lines => (is=>'rw', isa=>'ArrayRef',default=>sub{[]});
has max_chunk => (is=>'rw', isa=>'Int');
has line_no => (is=>'rw', isa=>'Int', default=>sub{0});
has parsed_header => (is=>'rw', isa=>'Bool');
has stalled => (is=>'rw', isa=>'Bool');

sub init_fh {
    my $self = shift;
    if (!$self->fh) {
        my $f = $self->file;
        my $fh;
        if ($f) {
            if ($f =~ /\.gz$/) {
                $fh = FileHandle->new("gzip -dc $f|");
            }
            else {
                $fh = FileHandle->new($f);
            }
        }
        else {
            confess "no file";
        }
        if (!$fh) {
            confess "no fh";
        }
        $self->fh($fh);
    }
}

sub parse {
    my $self = shift;
    $self->init_fh;
    $self->parse_header;
    $self->parsed_header(1);
    $self->parse_body;
}

sub parse_chunk {
    my $self = shift;
    if ($self->parsed_header &&
        !$self->stalled) {
        return 0;
    }
    my $size = shift;
    $self->max_chunk($size)
        if $size;
    $self->init_fh;
    $self->parse_header()
        unless $self->parsed_header;
    $self->parsed_header(1);
    $self->parse_body;
    return 1;
}




sub next_line {
    my $self = shift;
    my $fh = $self->fh;
    my $max_chunk = $self->max_chunk;
    my $line_no = $self->line_no + 1;
    $self->line_no($line_no);
    
    $self->stalled(0);
    if ($self->parsed_header && $max_chunk && $line_no > $max_chunk) {
        $self->line_no(0);
        $self->stalled(1);
        return undef;
    }
    my $lines = $self->lines;
    if (@$lines) {
        return shift @$lines;
    }
    
    my $line = <$fh>;
    return $line;
}

sub unshift_line {
    my $self = shift;
    $self->line_no($self->line_no - scalar(@_));
    unshift(@{$self->lines},@_);
    return;
}

1;


=head1 NAME

GOBO::Parsers::Parser

=head1 SYNOPSIS

=head1 DESCRIPTION

Base class for all parsers. Parsers take formats (e.g. GOBO::Parsers::OBOParser) and generate objects, typically some combination of GOBO::Node and GOBO::Statement objects

=cut
