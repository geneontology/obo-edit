package AmiGO::Aid::PantherDB;
use strict;
use warnings;

use AmiGO::Aid::ReferenceGenome;
use lib $ENV{GO_ROOT} . '/go-perl';
use GO::Metadata::Panther qw/@species/;
use base 'GO::Metadata::Panther';

our @EXPORT_OK = qw/@species/;

sub color{
    my $s = shift;
    my $color = AmiGO::Aid::ReferenceGenome->taxid2color($s->{ncbi_taxa_id}->[0]);
    return (hex(substr($color,1)) ? $color : undef);
}

1;
