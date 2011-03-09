package AmiGO::Aid::PantherDB;
# Shold probably call this AmiGO::Aid::RefGenome
use warnings;
use strict;
use Data::Dumper;

use AmiGO::Aid::ReferenceGenome;
use lib $ENV{GO_DEV_ROOT} . '/go-perl';
use GO::Metadata::Panther;
use base 'GO::Metadata::Panther';

=head1 NAME

AmiGO::Aid::PantherDB - L<GO::Metadata::Panther> with color!

=head1 DESCRIPTION

Adds functionality to the L<GO::Metadata::Panther> object.

=over

=item $s->color()

Returns a pretty color if the species is a reference genome.
Otherwise retuns C<undef>.

=cut
sub color{
    my $s = shift;
    my $color = AmiGO::Aid::ReferenceGenome->taxid2color($s->ncbi_taxon_id());
    return (hex(substr($color,1)) ? $color : undef);
}

=item $s->reference_genome()

Returns a list of AmiGO::Aid::PantherDB objects that are references
genomes sorted by scientific name.

=cut
sub reference_genome{
    my $c = shift;
    return sort {
	$a->scientific_name() cmp $b->scientific_name();
    } $c->new(@AmiGO::Aid::ReferenceGenome::RG_CODE_ORDER);
}

=back

=head1 SEE ALSO

L<GO::Metadata::Panther>, L<AmiGO::Aid::ReferenceGenome>

=cut
1;
