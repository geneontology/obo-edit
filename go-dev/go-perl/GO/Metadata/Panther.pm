package GO::Metadata::Panther;
use strict;
use warnings;
use Exporter;
use base qw/Exporter/;
use Memoize;
use List::Util qw/sum first/;
use Data::Dumper;

our @EXPORT_OK = qw/@species/;

=head1 NAME

GO::Metadata::Panther - Species info for data used by Panther Clusters

=head1 SYNOPSIS

 use GO::Metadata::Panther qw/@species/;

 for my $species (@species) {
  # do something
 }


Or

 use GO::Metadata::Panther;
 my $s = GO::Metadata::Panther->code('YEAST');

=head1 DESCRIPTION

Accesses information related to species in the Panther F<seq2pthr.gz>
file.  This file can be fetched from:
L<ftp://ftp.pantherdb.org/genome/pthr7.0/>

Each item in the exportable C<@species> array contains a hash
reference for each species.  The items in that hash are:

=over

=item code

A scalar or the UniProt species code.

=item ncbi_taxa_id

A scalar reference of NCBI taxa ids that items in the GO database
match.  This should only be one id, but sometimes it's useful to scan
multiple.

=back

For a complete list of every UniProt species matched to a NCBI taxa
L<http://www.uniprot.org/docs/speclist>



=cut

# These need to be in the order you wish to view them on the AmiGO
# dist png.
#
# For reference genomes, the first number is the ncbi_taxa_id list
# needs to be the reference in AmiGO::Aid::ReferenceGenome


our @species =
  (
   #
   # A
   #

   { # Anopheles gambiae
    code => 'ANOGA',
    ncbi_taxa_id =>  [ 7165 ],
    prefer => [ 'Gene' ],
   },

   { # Arabidopsis thaliana
    code => 'ARATH',
    ncbi_taxa_id => [ 3702 ],
   },

   { # Aquifex aeolicus
    code => 'AQUAE',
    ncbi_taxa_id => [ 63363 ],
   },

   { # Ashbya gossypii ATCC 10895
    code => 'ASHGO',
    ncbi_taxa_id => [ 33169 ],
   },

   #
   # B
   #

   { # Bacillus subtilis,
    code => 'BACSU',
    ncbi_taxa_id => [ 1423 ],
   },

   { # Bacteroides thetaiotaomicron
    code => 'BACTN',
    ncbi_taxa_id => [ 818 ],
   },

   { # Bos taurus
    code => 'BOVIN',
    ncbi_taxa_id => [ 9913 ],
   },

   { # Bradyrhizobium japonicum
    code => 'BRAJA',
    # matches the two UniProtKB items in GO
    ncbi_taxa_id => [ 375 ],
   },

   #
   # C
   #

   { # Caenorhabditis briggsae
    code => 'CAEBR',
    ncbi_taxa_id => [ 6238 ],
   },

   { # Caenorhabditis elegans
    code => 'CAEEL',
    ncbi_taxa_id => [ 6239 ],
    prefer => [ 'WB' ],
   },

   { # Canis lupus familiaris
    code => 'CANFA',
    ncbi_taxa_id => [ 9615 ],
    prefer => [ 'UniProtKB' ],
   },

   { # Chlamydia trachomatis
    code => 'CHLTA',
    ncbi_taxa_id => [ 315277 ],

   },

   { # Chlamydomonas reinhardtii
    code => 'CHLRE',
    ncbi_taxa_id => [ 3055 ],
    # found 11 of them
   },

   {
    code => 'CHLAA',
    ncbi_taxa_id => [ 324602 ],
   },

   { # Ciona intestinalis
    code => 'CIOIN',
    ncbi_taxa_id => [ 7719 ],
   },

   #
   # D
   #

   { # Danio rerio
    code => 'DANRE',
    ncbi_taxa_id => [ 7955 ],
    prefer => [ 'ZFIN' ],
   },

   { # Deinococcus radiodurans
    code => 'DEIRA',
    ncbi_taxa_id => [ 1299 ],
   },

   { # Dictyostelium discoideum
    code => 'DICDI',
    ncbi_taxa_id => [ 44689 ],
   },

   { # Drosophila melanogaster
    code => 'DROME',
    ncbi_taxa_id => [ 7227 ],
   },

   #
   # E
   #

   { # Emericella nidulans
    code => 'EMENI',
    ncbi_taxa_id => [ 162425 ],
   },

   { # Entamoeba histolytica
    code => 'ENTHI',
    ncbi_taxa_id => [ 5759 ],
   },

   { #  Escherichia coli str. K-12 substr. MG1655
    code => 'ECOLI',
    # I think the 511145 is more for reverse compatibility
    ncbi_taxa_id => [ 83333, 511145 ],
   },

   #
   # G
   #

   { # Gallus gallus
    code => 'CHICK',
    ncbi_taxa_id => [ 9031 ],
    prefer => [ 'UniProtKB' ],
   },

   { # Geobacter sulfurreducens
    code => 'GEOSL',
    ncbi_taxa_id => [ 35554 ],
   },

   { # Gloeobacter violaceus
    code => 'GLOVI',
    # Matched the one item in the database
    ncbi_taxa_id => [ 33072 ],
   },

   #
   # H
   #

   { # Homo sapiens
    code => 'HUMAN',
    ncbi_taxa_id => [ 9606 ],
    prefer => [ 'ENSEMBL', 'UniProtKB' ],
   },

   #
   # L
   #

   { # Leishmania major
    code => 'LEIMA',
    ncbi_taxa_id => [ 5664 ],
   },

   { # Leptospira interrogans
    code => 'LEPIN',
    # only gets one
    ncbi_taxa_id => [ 173 ],
   },

   #
   # M
   #

   { # Macaca mulatta
    code => 'MACMU',
    ncbi_taxa_id => [ 9544 ],
   },

   { # Methanosarcina acetivorans
    code => 'METAC',
    ncbi_taxa_id => [ 2214 ],
   },

   { # Monodelphis domestica
    code => 'MONDO',
    ncbi_taxa_id => [ 13616 ],
   },

   { # Mus musculus
    code => 'MOUSE',
    ncbi_taxa_id => [ 10090 ],
    prefer => [ 'MGI' ],
   },

   #
   # N
   #

   { # Neurospora crassa
    code => 'NEUCR',
    ncbi_taxa_id => [ 5141 ],
   },

   #
   # O
   #

   { # Ornithorhynchus anatinus
    code => 'ORNAN',
    ncbi_taxa_id => [ 9258 ],
   },

   { # Oryza sativa
    code => 'ORYSJ',
    ncbi_taxa_id => [ 39947 ], # sativa Japonica Group
   },

   #
   # P
   #

   { # Pan troglodytes
    code => 'PANTR',
    ncbi_taxa_id => [ 9598 ],
   },

   { # Plasmodium yoelii
    code => 'PLAYO',
    ncbi_taxa_id => [ 73239 ],
   },

   { # Pseudomonas aeruginosa
    code => 'PSEA7',
    ncbi_taxa_id => [ 381754 ],
   },

   #
   # R
   #

   { # Rattus norvegicus
    code => 'RAT',
    ncbi_taxa_id => [ 10116 ],
    prefer => [ 'RGD' ],
   },

   #
   # S
   #

   { # Saccharomyces cerevisiae
    code => 'YEAST',
    ncbi_taxa_id => [ 4932 ],
   },

   { # Schizosaccharomyces pombe
    code => 'SCHPO',
    ncbi_taxa_id => [ 4896 ],
   },

   { # Streptomyces coelicolor
    code => 'STRCO',
    ncbi_taxa_id => [ 1902 ],
   },

   { # Strongylocentrotus purpuratus
    code => 'STRPU',
    ncbi_taxa_id => [ 7668 ],
   },

   { # Sulfolobus solfataricus
    code => 'SULSO',
    ncbi_taxa_id => [ 2287 ],
   },

   #
   # T
   #

   { # Takifugu rubripes
    code => 'FUGRU',
    # matched all 11 items! Adding the non 31033 added no new items.
    ncbi_taxa_id => #[ 31033 ],
    [
     31033, # rubripes
     346655, # rubripes nervous necrosis virus
     47663, # rubripes rubripes
    ],
   },

   { # Tetrahymena thermophila
    code => 'TETTH',
    ncbi_taxa_id => [ 5911, 312017 ],
   },

   { # Thermotoga maritima
    code => 'THEMA',
    ncbi_taxa_id => [ 2336 ],
   },

   #
   # X
   #

   { # Xenopus', '(Silurana) tropicalis
    code => 'XENTR',
    ncbi_taxa_id => [ 8364 ],
   },

);



=head2 Constructors

The constructors scans C<@species> for the requested data and returns
the object that matches the data.  Otherwise it returns a false false.

=over

=item my $s = GO::Metadata::Panther->code(I<unicode_species_code>)

Return an object filled with the species reference from the UniProtKB
species code.

=cut
memoize('code');
sub code{
    my $class = shift;
    my $code = shift;

    for my $species (@species) {
	if ($species->{code} eq $code) {
	    return bless $species, $class;
	}
    }
    return undef;
}

=item my $s = GO::Metadata::Panther->ncbi(I<ncbi_taxa_id>)

Greate an object from the I<ncbi_taxa_id>.

=cut
sub ncbi{
    my $class = shift;
    my $ncbi = shift;

    for my $species (@species) {
	if (first {
	    $ncbi == $_;
	} @{ $species->{ncbi_taxa_id} }) {
	    return bless $species, $class;
	}
    }
    return undef
}

=back

=head2 Function

Functions that can be used outside of the OO interface.

=over

=item GO::Metadata::Panther::codes()

Returns a list of all UniProt species codes in C<@species>.

=cut
sub codes{
    return map { $_->{code} } @species;
}

=item GO::Metadata::Panther::valid_codes(I<unicode_species_code>)

Send it a list of panther Unicode codes, returns true if they are all
present in C<@species>.  Othewise returns false.

=cut
sub valid_codes{
    return scalar(@_) == sum(map {
	__PACKAGE__->code($_) ? 1 : 0;
    } @_);
}

=back

=head2 OO Function

=over

=item $s->ncbi_ids()

Returns the list of NCBI taxa identifiers associated with the UniProt
species code.  In a perfect word this will only every return one
value.  In any case, the first value will be the actual numeric
identifier associated.

=cut
sub ncbi_ids{
    my $s = shift;
    return @{ $s->{ncbi_taxa_id} };
}

=back

=cut

1;
