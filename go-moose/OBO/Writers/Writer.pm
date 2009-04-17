package OBO::Writers::Writer;
use Moose;
use strict;
use OBO::Graph;
use FileHandle;

has fh => (is=>'rw', isa=>'FileHandle');
has file => (is=>'rw', isa=>'Str');
has graph => (is=>'rw', isa=>'OBO::Graph');

sub init_fh {
    my $self = shift;
    if (!$self->fh) {
        my $f = $self->file;
        my $fh;
        if ($f) {
            $fh = FileHandle->new(">$f");
        }
        if (!$fh) {
            $fh = FileHandle->new(">-");
        }
        $self->fh($fh);
    }
}

sub write {
    my $self = shift;
    $self->init_fh;
    $self->write_header;
    $self->write_body;
}

sub printrow {
    my $self = shift;
    my $row = shift;
    my $fh = $self->fh;
    print $fh join("\t",@$row),"\n";
    return;
}


1;
