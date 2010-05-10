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
    #common => 'mosquito',
    ncbi_taxa_id =>
    [
     7165,   # gambiae
     # 377271, # gambiae M
     # 180454, # gambiae str. PEST
     # 377270, # gambiae S
     # 487311, # gambiae densonucleosis virus
    ],

    # The pthr file only has ensembl.  GO has PDB and UniProtKB.
    # Didn't check the strands.
   },

   { # Arabidopsis thaliana
    key => 'ARATH',
    ncbi_taxa_id => [ 3702 ],
    dbname => { gene => [ 'TAIR' ] },
   },

   { # Aquifex aeolicus
    key => 'AQUAE',
    # This has no gene_product items
    ncbi_taxa_id =>
    [
     63363,  # aeolicus
     224324, # aeolicus VF5
    ],
   },

   { # Ashbya gossypii ATCC 10895
    key => 'ASHGO',
    ncbi_taxa_id => [ 284811 ],
    dbname => { protein => [ 'UniProtKB' ] },
    # This has no gene_product items
   },

   { # Aspergillus nidulans FGSC A4
    key => 'EMENI',
    ncbi_taxa_id => [ 227321 ],
    dbname => { protein => [ 'UniProtKB' ] },
    # Doesn't seem to have any entries in GO
   },

   #
   # B
   #

   { # Bacillus subtilis,
    key => 'BACSU',
    # Nothing in the database
    ncbi_taxa_id =>
    [
     565143, # subtilis subsp. subtilis str. AUSI98
     1483, # subtilis subsp. amylosacchariticus
     96241, # subtilis subsp. spizizenii
     536088, # subtilis subsp. subtilis str. L170
     564286, # subtilis str. 10
     483257, # subtilis subsp. qingdao
     86029, # subtilis subsp. natto
     483913, # subtilis subsp. inaquosorum
     653685, # subtilis group
     1423, # subtilis
     645657, # subtilis subsp. natto BEST195
     135461, # subtilis subsp. subtilis
     536089, # subtilis subsp. subtilis str. N170
     224308, # subtilis subsp. subtilis str. 168
     535026, # subtilis subsp. subtilis str. NCIB 3610
     186379, # subtilis subsp. endophyticus
     655816, # subtilis subsp. spizizenii str. W23
     118965, # subtilis subsp. chungkookjang
     448958, # subtilis subsp. lactipan
     703612, # subtilis subsp. spizizenii ATCC 6633
     535024, # subtilis subsp. subtilis str. SMY
     459650, # subtilis subsp. sadata
     535025, # subtilis subsp. subtilis str. JH642
    ],


   },

   { # Bacteroides thetaiotaomicron
    key => 'BACTN',
    ncbi_taxa_id => [ 818, 226186 ],
    # Neith ncbi_taxa_id hase gene_product entries
   },

   { # Bos taurus
    key => 'BOVIN',
    ncbi_taxa_id => [ 9913 ],
    dbname => { protein => [ 'UniProtKB', 'ENSEMBL' ] },
   },

   { # Bradyrhizobium japonicum
    key => 'BRAJA',
    # matches the two UniProtKB items in GO
    ncbi_taxa_id =>
    [
     305581, # japonicum bv. glycinearum
     476282, # japonicum SEMIA 5079
     306164, # japonicum bv. genistearum
     375,    # japonicum
     224911, # japonicum USDA 110
    ],
   },

   #
   # C
   #

   { # Caenorhabditis briggsae
    key => 'CAEBR',
    ncbi_taxa_id => [ 6238 ],
    dbname => { protein => [ 'UniProtKB' ] },
    # GO has UniProtKB items, pthr has Entrez and NCBI
   },

   { # Caenorhabditis elegans
    key => 'CAEEL',
    ncbi_taxa_id => [ 6239 ],
    dbname => { gene => [ 'WB' ] },
   },

   { # Canis lupus familiaris
    key => 'CANFA',
    ncbi_taxa_id => [ 9615 ],
    # Seems to of matched most of the UniProtKB items in the database
   },

   { # Chlamydia trachomatis
    key => 'CHLTA',
    # no matches, even with all the strains
    ncbi_taxa_id =>
    [
     813, # trachomatis
     471472, # trachomatis 434/Bu
     564416, # trachomatis 6276
     564417, # trachomatis 6276s
     564418, # trachomatis 70
     564419, # trachomatis 70s
     315277, # trachomatis A/HAR-13
     596775, # trachomatis A/SA-1
     580047, # trachomatis A2497
     580049, # trachomatis B/Jali20/OT
     634463, # trachomatis B/TW5/OT
     672161, # trachomatis B/TZ1A828/OT
     596776, # trachomatis Ba/Apache-2
     598584, # trachomatis C/308-06
     573237, # trachomatis D(s)2923
     598585, # trachomatis D/2s
     272561, # trachomatis D/UW-3/CX
     707183, # trachomatis E/11023
     707184, # trachomatis E/150
     598586, # trachomatis E/5s
     596777, # trachomatis E/Bour
     527018, # trachomatis F/IC-Cal3
     527017, # trachomatis F1
     707187, # trachomatis G/11074
     707186, # trachomatis G/11222
     718219, # trachomatis G/9301
     707185, # trachomatis G/9768
     596778, # trachomatis G/UW-57/CX
     598587, # trachomatis H/18s
     596779, # trachomatis H/UW-4
     596780, # trachomatis I/UW-12/UR
     596781, # trachomatis J/UW-36
     598588, # trachomatis Ja/26s
     471473, # trachomatis L2b/UCH-1/proctitis
     658599, # trachomatis L2tet1
     634464, # trachomatis Sweden2
     634465, # trachomatis Sweden3
     634466, # trachomatis Sweden4
     634467, # trachomatis Sweden5
    ],
   },

   { # Chlamydomonas reinhardtii
    key => 'CHLRE',
    ncbi_taxa_id => [ 3055 ],
    # found 11 of them
   },

   {
    key => 'CHLAA',
    # Found nothing in the database
    ncbi_taxa_id =>
    [
     1108,   # aurantiacus
     324602, # aurantiacus J-10-fl
    ],
   },

   { # Ciona intestinalis
    key => 'CIOIN',
    # Woohoo, found three of them!
    ncbi_taxa_id =>
    [
     7719,   # intestinalis
     413601, # intestinalis B CG-2006
    ],
   },

   #
   # D
   #

   { # Danio rerio
    key => 'DANRE',
    ncbi_taxa_id => [ 7955 ],
    dbname  => { gene => [ 'ZFIN', 'ENSEMBL' ] },
    # It looks like I'm going to need an ENSEMBL gene id to ZFIN id
    # mapping file to complete ZFIN.
   },

   { # Deinococcus radiodurans
    key => 'DEIRA',
    ncbi_taxa_id => [ 1299, 243230 ],
    dbname => { protein => [ 'UniProtKB' ] },
   },

   { # Dictyostelium discoideum
    key => 'DICDI',
    dbname  => { gene => [ 'dictyBase' ] },
    ncbi_taxa_id =>
    [
     44689,  # discoideum
     #366501, # discoideum AX2
     #352472, # discoideum AX4
    ],

   },

   { # Drosophila melanogaster
    key => 'DROME',
    dbname  => { gene => [ 'FB' ] },
    ncbi_taxa_id =>
    [
     7227, # melanogaster
     # 666363, # melanogaster sigma virus AP30
     # 671496, # melanogaster sigma virus Derby
     # 721817, # melanogaster x Drosophila sechellia
     # 663280, # melanogaster tetravirus SW-2009a
     # 663279, # melanogaster American nodavirus (ANV) SW-2009a
     # 348584, # melanogaster Zam virus
     # 698371, # cf. melanogaster JFC-2009
     # 666961, # melanogaster sigma virus HAP23
     # 671497, # melanogaster sigma virus DM113
     # 663282, # melanogaster totivirus SW-2009a
     # 671498, # melanogaster sigma virus U125
     # 348585, # melanogaster Idefix virus
     # 663281, # melanogaster birnavirus SW-2009a
    ],
   },

   #
   # E
   #

   { # Entamoeba histolytica
    key => 'ENTHI',
    ncbi_taxa_id => #[ 5759, 294381 ],
    [
     5759,   # histolytica
     294381, # histolytica HM-1:IMSS
    ],
    # Got nothing
   },

   { #  Escherichia coli str. K-12 substr. MG1655
    key => 'ECOLI',
    dbname => { protein => [ 'ECOLI' ] },
    ncbi_taxa_id =>
    [
     83333,  # coli K-12
     511145, # coli str. K-12 substr. MG1655
     # None of the others matched anything
    ],
   },

   #
   # G
   #

   { # Gallus gallus
    key => 'CHICK',
    dbname => { protein => [ 'NCBI', 'UniProtKB' ] },
    # getting just over half of these :(
    ncbi_taxa_id =>
    [
     9031, # gallus
     # 208525, # gallus bankiva
     # 208526, # gallus gallus
     # 405000, # gallus jabouillei
     # 400035, # gallus murghi
     # 208524, # gallus spadiceus
    ],
   },

   { # Geobacter sulfurreducens
    key => 'GEOSL',
    # Match 4 of 6.
    ncbi_taxa_id => #[ 35554 ],
    [
     35554,  # sulfurreducens
     # 663917, # sulfurreducens KN400
     # 243231, # sulfurreducens PCA
    ],
   },

   { # Gloeobacter violaceus
    key => 'GLOVI',
    # Matched the one item in the database
    ncbi_taxa_id => #[ 33072 ],
    [
     33072,  # violaceus
     251221, # violaceus PCC 7421
     102124, # violaceus PCC 8105
    ],
   },

   #
   # H
   #

   { # Homo sapiens
    key => 'HUMAN',
    ncbi_taxa_id => [ 9606 ],
    dbname => { protein => [ 'UniProtKB' ] },
   },

   #
   # L
   #

   { # Leishmania major
    key => 'LEIMA',
    # Found nothing
    ncbi_taxa_id =>
    [
     5664, # major
     38581, # major species complex
     214618, # cf. major
     347515, # major strain Friedlin
    ],
   },

   { # Leptospira interrogans
    key => 'LEPIN',
    # only gets one
    ncbi_taxa_id =>
    [
     173, # interrogans
     # I added the 81 strains and it didn't match any more
    ],
   },

   #
   # M
   #

   { # Macaca mulatta
    key => 'MACMU',
    # adding the other ncbi_ids didn't find any new items
    ncbi_taxa_id =>
    [
     9544, # mulatta
     # 10373, # mulatta cytomegalovirus
     # 221245, # mulatta lymphocryptovirus 1
     # 83534, # mulatta rhadinovirus 17577
     # 11829, # mulatta type C retrovirus
     # 703611, # mulatta rhadinovirus
    ],
   },

   { # Methanosarcina acetivorans
    key => 'METAC',
    # finds 1 item
    ncbi_taxa_id =>
    [
     2214, # acetivorans
     188937, # acetivorans C2A
    ]
   },

   { # Monodelphis domestica
    key => 'MONDO',
    ncbi_taxa_id => [ 13616 ],
   },

   { # Mus musculus
    key => 'MOUSE',
    dbname  => { gene => [ 'MGI' ] },
    ncbi_taxa_id =>
    [
     10090, # musculus
     # 590745, # musculus mobilized endogenous polytropic provirus
     # 57486, # musculus molossinus
     # 10092, # musculus domesticus
     # 46456, # musculus wagneri
     # 35531, # musculus bactrianus
     # 308714, # musculus rhadinovirus 1
     # 179238, # musculus homourus
     # 116058, # musculus brevirostris
     # 80274, # musculus gentilulus
     # 10091, # musculus castaneus
     # 39442, # musculus musculus
    ],
   },

   #
   # N
   #

   { # Neurospora crassa
    key => 'NEUCR',
    # Fonud 10 of them
    ncbi_taxa_id =>
    [
     5141,   # crassa
     367110, # crassa OR74A
    ],
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
    # found nothing
    ncbi_taxa_id =>
    [
     4530, # sativa
     39947, # sativa Japonica Group
     39946, # sativa Indica Group
     362693, # sativa endornavirus
    ],
   },

   #
   # P
   #

   { # Pan troglodytes
    key => 'PANTR',
    dbname => { protein => [ 'UniProtKB', 'ENSEMBL' ] },
    ncbi_taxa_id =>
    [
     9598, # troglodytes
     # 426751, # troglodytes adenovirus type 3
     # 426753, # troglodytes adenovirus type 63
     # 660664, # troglodytes cytomegalovirus 1.1
     # 660665, # troglodytes cytomegalovirus 1.2
     # 660666, # troglodytes cytomegalovirus 2.1
     # 660667, # troglodytes cytomegalovirus 2.2
     # 660668, # troglodytes cytomegalovirus 2.3
     # 298339, # troglodytes foamy virus
     # 254249, # troglodytes herpesvirus 6
     # 212727, # troglodytes lymphocryptovirus 1
     # 135725, # troglodytes rhadinovirus 1
     # 138896, # troglodytes rhadinovirus 1a
     # 138897, # troglodytes rhadinovirus 1b
     # 171371, # troglodytes rhadinovirus 2
     # 682884, # troglodytes rhadinovirus 3
     # 308725, # troglodytes roseolovirus 1
     # 37010, # troglodytes schweinfurthii
     # 37011, # troglodytes troglodytes
     # 399494, # troglodytes troglodytes foamy virus
     # 91950, # troglodytes vellerosus
     # 37012, # troglodytes verus
    ],
   },

   { # Plasmodium yoelii
    key => 'PLAYO',
    ncbi_taxa_id => [ 5861 ],
    dbname => {protein => [ 'UniProtKB' ] },
   },

   { # Pseudomonas aeruginosa
    key => 'PSEA7',
    # By adding all the not 287 items we now match 1 gene_product!
    ncbi_taxa_id =>
    [
     287, # aeruginosa
     350703, # aeruginosa 2192
     350704, # aeruginosa C3719
     646312, # aeruginosa DoWo1
     136841, # aeruginosa group
     557722, # aeruginosa LESB58
     497979, # aeruginosa OPPA8
     652611, # aeruginosa PA14
     381754, # aeruginosa PA7
     509633, # aeruginosa PAb1
     388272, # aeruginosa PACS2
     208964, # aeruginosa PAO1
     566548, # aeruginosa PAP7
     419110, # aeruginosa PKS6
     208963, # aeruginosa UCBPP-PA14
    ],
   },

   #
   # R
   #

   { # Rattus norvegicus
    key => 'RAT',
    dbname  => { gene => [ 'RGD' ], protein => [ 'UniProtKB' ] },
    # Adding non 10116 items didn't find any more items
    ncbi_taxa_id =>
    [
     10116, # norvegicus
     # 664730, # norvegicus papillomavirus 1 EES-2009
     # 425192, # norvegicus rhadinovirus 1
     # 425193, # norvegicus rhadinovirus 2
    ],
   },

   #
   # S
   #

   { # Saccharomyces cerevisiae
    key => 'YEAST',
    ncbi_taxa_id => [ 4932 ],
    dbname  => { gene => [ 'SGD' ] },
   },

   { # Schizosaccharomyces pombe
    key => 'SCHPO',
    ncbi_taxa_id => [ 4896 ],
    dbname  => { gene => [ 'GeneDB_Spombe' ] },
   },

   { # Streptomyces coelicolor
    key => 'STRCO',
    ncbi_taxa_id =>
    [
     1902, # coelicolor
     # 100226, # coelicolor A3(2
    ],
   },

   { # Strongylocentrotus purpuratus
    key => 'STRPU',
    ncbi_taxa_id => [ 7668 ],
   },

   { # Sulfolobus solfataricus
    key => 'SULSO',
    # With 2287 we find 2 gene_products, w/o it, 2.
    ncbi_taxa_id => #[ 2287 ],
    [
     2287, # solfataricus
     # 555311, # solfataricus 98/2
     # 273057, # solfataricus P2
    ],

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
    # adding non 5911 species added no new references
    ncbi_taxa_id =>
    [
     5911, # thermophila
     595534, # thermophila C3/SB3543
     175564, # thermophila II
     312017, # thermophila SB210
     175565, # thermophila VI
    ],
   },

   { # Thermotoga maritima
    key => 'THEMA',
    # adding 243274 helped not
    ncbi_taxa_id =>
    [
     2336, # maritima
     # 243274, # maritima MSB8
    ],
   },

   #
   # X
   #

   { # Xenopus', '(Silurana) tropicalis
    key => 'XENTR',
    # adding non 8364 items helped not
    ncbi_taxa_id =>
    [
     8364, # (Silurana) tropicalis
     # 288623, # (Silurana) cf. tropicalis BJE-2004
     # 224340, # (Silurana) epitropicalis
    ],
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
