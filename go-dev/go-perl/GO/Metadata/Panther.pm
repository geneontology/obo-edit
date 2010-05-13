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

=head1 DESCRIPTION

It's ok exports C<@species>, a list of hash information about each
species.

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
    key => 'ANOGA',
    ncbi_taxa_id =>  [ 7165 ],
    prefer => [ 'Gene' ],
   },

   { # Arabidopsis thaliana
    key => 'ARATH',
    ncbi_taxa_id => [ 3702 ],
   },

   { # Aquifex aeolicus
    key => 'AQUAE',
    ncbi_taxa_id => [ 63363 ],
   },

   { # Ashbya gossypii ATCC 10895
    key => 'ASHGO',
    ncbi_taxa_id => [ 33169 ],
   },

   #
   # B
   #

   { # Bacillus subtilis,
    key => 'BACSU',
    ncbi_taxa_id => [ 1423 ],
   },

   { # Bacteroides thetaiotaomicron
    key => 'BACTN',
    ncbi_taxa_id => [ 818 ],
   },

   { # Bos taurus
    key => 'BOVIN',
    ncbi_taxa_id => [ 9913 ],
   },

   { # Bradyrhizobium japonicum
    key => 'BRAJA',
    # matches the two UniProtKB items in GO
    ncbi_taxa_id => [ 375 ],
   },

   #
   # C
   #

   { # Caenorhabditis briggsae
    key => 'CAEBR',
    ncbi_taxa_id => [ 6238 ],
   },

   { # Caenorhabditis elegans
    key => 'CAEEL',
    ncbi_taxa_id => [ 6239 ],
    prefer => [ 'WB' ],
   },

   { # Canis lupus familiaris
    key => 'CANFA',
    ncbi_taxa_id => [ 9615 ],
    prefer => [ 'UniProtKB' ],
   },

   { # Chlamydia trachomatis
    key => 'CHLTA',
    ncbi_taxa_id => [ 315277 ],

   },

   { # Chlamydomonas reinhardtii
    key => 'CHLRE',
    ncbi_taxa_id => [ 3055 ],
    # found 11 of them
   },

   {
    key => 'CHLAA',
    ncbi_taxa_id => [ 324602 ],
   },

   { # Ciona intestinalis
    key => 'CIOIN',
    ncbi_taxa_id => [ 7719 ],
   },

   #
   # D
   #

   { # Danio rerio
    key => 'DANRE',
    ncbi_taxa_id => [ 7955 ],
    prefer => [ 'ZFIN' ],
   },

   { # Deinococcus radiodurans
    key => 'DEIRA',
    ncbi_taxa_id => [ 1299 ],
   },

   { # Dictyostelium discoideum
    key => 'DICDI',
    ncbi_taxa_id => [ 44689 ],
   },

   { # Drosophila melanogaster
    key => 'DROME',
    ncbi_taxa_id => [ 7227 ],
   },

   #
   # E
   #

   { # Emericella nidulans
    key => 'EMENI',
    ncbi_taxa_id => [ 162425 ],
   },

   { # Entamoeba histolytica
    key => 'ENTHI',
    ncbi_taxa_id => [ 5759 ],
   },

   { #  Escherichia coli str. K-12 substr. MG1655
    key => 'ECOLI',
    ncbi_taxa_id => [ 83333 ],
   },

   #
   # G
   #

   { # Gallus gallus
    key => 'CHICK',
    ncbi_taxa_id => [ 9031 ],
    prefer => [ 'UniProtKB' ],
   },

   { # Geobacter sulfurreducens
    key => 'GEOSL',
    ncbi_taxa_id => [ 35554 ],
   },

   { # Gloeobacter violaceus
    key => 'GLOVI',
    # Matched the one item in the database
    ncbi_taxa_id => [ 33072 ],
   },

   #
   # H
   #

   { # Homo sapiens
    key => 'HUMAN',
    ncbi_taxa_id => [ 9606 ],
    prefer => [ 'ENSEMBL', 'UniProtKB' ],
   },

   #
   # L
   #

   { # Leishmania major
    key => 'LEIMA',
    ncbi_taxa_id => [ 5664 ],
   },

   { # Leptospira interrogans
    key => 'LEPIN',
    # only gets one
    ncbi_taxa_id => [ 173 ],
   },

   #
   # M
   #

   { # Macaca mulatta
    key => 'MACMU',
    ncbi_taxa_id => [ 9544 ],
   },

   { # Methanosarcina acetivorans
    key => 'METAC',
    ncbi_taxa_id => [ 2214 ],
   },

   { # Monodelphis domestica
    key => 'MONDO',
    ncbi_taxa_id => [ 13616 ],
   },

   { # Mus musculus
    key => 'MOUSE',
    ncbi_taxa_id => [ 10090 ],
    prefer => [ 'MGI' ],
   },

   #
   # N
   #

   { # Neurospora crassa
    key => 'NEUCR',
    ncbi_taxa_id => [ 5141 ],
   },

   #
   # O
   #

   { # Ornithorhynchus anatinus
    key => 'ORNAN',
    ncbi_taxa_id => [ 9258 ],
   },

   { # Oryza sativa
    key => 'ORYSJ',
    ncbi_taxa_id => [ 39947 ], # sativa Japonica Group
   },

   #
   # P
   #

   { # Pan troglodytes
    key => 'PANTR',
    ncbi_taxa_id => [ 9598 ],
   },

   { # Plasmodium yoelii
    key => 'PLAYO',
    ncbi_taxa_id => [ 73239 ],
   },

   { # Pseudomonas aeruginosa
    key => 'PSEA7',
    ncbi_taxa_id => [ 381754 ],
   },

   #
   # R
   #

   { # Rattus norvegicus
    key => 'RAT',
    ncbi_taxa_id => [ 10116 ],
    prefer => [ 'RGD' ],
   },

   #
   # S
   #

   { # Saccharomyces cerevisiae
    key => 'YEAST',
    ncbi_taxa_id => [ 4932 ],
   },

   { # Schizosaccharomyces pombe
    key => 'SCHPO',
    ncbi_taxa_id => [ 4896 ],
   },

   { # Streptomyces coelicolor
    key => 'STRCO',
    ncbi_taxa_id => [ 1902 ],
   },

   { # Strongylocentrotus purpuratus
    key => 'STRPU',
    ncbi_taxa_id => [ 7668 ],
   },

   { # Sulfolobus solfataricus
    key => 'SULSO',
    ncbi_taxa_id => [ 2287 ],
   },

   #
   # T
   #

   { # Takifugu rubripes
    key => 'FUGRU',
    # matched all 11 items! Adding the non 31033 added no new items.
    ncbi_taxa_id => #[ 31033 ],
    [
     31033, # rubripes
     346655, # rubripes nervous necrosis virus
     47663, # rubripes rubripes
    ],
   },

   { # Tetrahymena thermophila
    key => 'TETTH',
    ncbi_taxa_id => [ 5911 ],
   },

   { # Thermotoga maritima
    key => 'THEMA',
    ncbi_taxa_id => [ 2336 ],
   },

   #
   # X
   #

   { # Xenopus', '(Silurana) tropicalis
    key => 'XENTR',
    ncbi_taxa_id => [ 8364 ],
   },

);

=item key

Return an object filled with the species reference from the UniprotKB.
Only returns items that are in the C<@species> array.

=cut
memoize('key');
sub key{
    my $c = shift;
    my $key = shift;

    for my $species (@species) {
	if ($species->{key} eq $key) {
	    #warn "Blessing $species->{key}";
	    return bless $species, $c;
	}
    }
    return undef;
}


sub ncbi{
    my $c = shift;
    my $ncbi = shift;

    for my $species (@species) {
	if (first {
	    $ncbi == $_;
	} @{ $species->{ncbi_taxa_id} }) {
	    return bless $species, $c;
	}
    }
    return undef;
}


=item keys

Returns a list of all keys in C<@species>.

=cut
sub keys{
    return map { $_->{key} } @species;
}

=item valid_keys

Send it a list of keys, returns true if they are all valid.  Othewise
returns false.

=cut
sub valid_keys{
    return scalar(@_) == sum(map {
	__PACKAGE__->key($_) ? 1 : 0;
    } @_);
}


our $dbname_default = { protein => [ 'UniProtKB' ] };
sub dbname_hash{
    my $s = shift;
    return $s->{dbname} || $dbname_default;
}

sub id_types{
    my $s = shift;
    return map {
	@$_;
    } values %{ $s->dbname_hash };
}

sub ncbi_ids{
    my $s = shift;
    return @{ $s->{ncbi_taxa_id} };
}

1;
