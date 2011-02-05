package AmiGO::Aid::PantherDB;
# Shold probably call this AmiGO::Aid::RefGenome
use warnings;
use strict;

use AmiGO::Aid::ReferenceGenome;
use lib $ENV{GO_DEV_ROOT} . '/go-perl';
use GO::Metadata::Panther;
use base 'GO::Metadata::Panther';

=head1 NAME

AmiGO::Aid::PantherDB - L<GO::Metadata::Panther> with color!

=head1 DESCRIPTION

Adds functionality to the L<GO::Metadata::Panther> object.

=over $s->color()

Returns a pretty color if the species is a reference genome.
Otherwise retuns C<undef>.

=cut
sub color{
    my $s = shift;
    my $color = AmiGO::Aid::ReferenceGenome->taxid2color($s->ncbi_taxon_id());
    return (hex(substr($color,1)) ? $color : undef);
}

=back

=head1 SEE ALSO

L<GO::Metadata::Panther>, L<AmiGO::Aid::ReferenceGenome>

=cut
1;
