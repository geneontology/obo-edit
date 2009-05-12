package OBO::Writers::Writer;
use Moose;
use strict;
use OBO::Graph;
use FileHandle;

has fh => (is=>'rw', isa=>'FileHandle', default=>sub {new FileHandle(">-")} );
#has fh => (is=>'rw', isa=>'FileHandle' );
#has file => (is=>'rw', isa=>'Str');
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

sub file {
    my $self = shift;
    if (@_) {
        my ($f) = @_;
        $self->{file} = $f;
        $self->fh(FileHandle->new(">$f"));
    }
    
    return $self->{file};
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

sub print {
    my $self = shift;
    my $fh = $self->fh;
    print $fh @_;
    return;
}

sub println {
    my $self = shift;
    my $fh = $self->fh;
    print $fh @_,"\n";
    return;
}

sub printf {
    my $self = shift;
    my $fmt = shift;
    my $fh = $self->fh;
    printf $fh $fmt, @_;
    return;
}

sub nl {
    my $self = shift;
    my $fh = $self->fh;
    print $fh "\n";
    return;
}


1;
