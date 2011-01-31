package GO::CGI::NameMunger;

use strict;
#use GO::Utils qw(rearrange spell_greek);
use FreezeThaw qw(freeze thaw);
use Data::Dumper;
use HTML::Entities;
use Exporter;
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);
@ISA = qw(Exporter);
@EXPORT_OK = qw(case_replace);
%EXPORT_TAGS = (all => [@EXPORT_OK]);

=head1 GO::CGI::NameMunger

This is a helper module to take database abbreviations, 
and produce URLs, human readable names, etc.

Ideally this will soon be done with RDF.  For now
it remains a perl hack.

=head2 get_url

parameters: database_abbreviation, acc_no

returns: url to the entry

get_url takes a database abbreviation from GO and accession
and returns a url to get the page.

=cut

sub new {
	my $class = shift;
	my $self = {};
	bless $self, $class;
	return $self;
}

=not used
sub spel_greek {
	my $self = shift;
	my $string = shift;

	return spell_greek($string);
}

=cut
sub get_url {
  my $self = shift;
  my $database = shift;
  my $acc_no = shift;


  $database = lc($database);
  if ($database eq "tair") {
	  $acc_no =~ s/^TAIR://;
  } elsif ($database eq "dros cdna") {
	  $database =~ s/\ /\_/;
  }

  #is it safe to trim any thing after first word?
  $acc_no =~ s/^(\S+)\s+.*/$1/;

  my %db_hash =
(
	cgd=>"http://www.candidagenome.org/cgi-bin/locus.pl?locus=$acc_no",
	ddb=>"http://dictybase.org/db/cgi-bin/gene_page.pl?dictybaseid=$acc_no",
	dictybase=>"http://dictybase.org/db/cgi-bin/gene_page.pl?dictybaseid=$acc_no",
	dros_cdna=>"http://weasel.lbl.gov/cgi-bin/EST/community_query/ctgReport.pl?db=estlabtrack&amp;id_type=0&amp;id_value=$acc_no",
	ec=>"http://ca.expasy.org/cgi-bin/nicezyme.pl?$acc_no",
	embl=>"http://www.ebi.ac.uk/cgi-bin/emblfetch?style=html&amp;Submit=Go&amp;id=$acc_no",
	ensembl=>"http://www.ensembl.org/perl/protview?peptide=$acc_no",
	fb=>"http://flybase.bio.indiana.edu/reports/$acc_no.html",
	flybase=>"http://flybase.bio.indiana.edu/reports/$acc_no.html",
	genedb_lmajor=>"http://www.genedb.org/genedb/Search?organism=leish&amp;name=$acc_no",
	genedb_pfalciparum=>"http://www.genedb.org/genedb/Search?organism=malaria&amp;name=$acc_no",
	genedb_spombe=>"http://www.genedb.org/genedb/Search?organism=pombe&amp;name=$acc_no",
	genedb_tbrucei=>"http://www.genedb.org/genedb/Search?organism=tryp&amp;name=$acc_no",
	go=>"http://amigo.geneontology.org/cgi-bin/amigo/go.cgi?action=query&amp;view=query&amp;search_constraint=term&amp;query=$acc_no",
	gr=>"http://www.gramene.org/perl/protein_search?acc=$acc_no",
	gramene=>"http://www.gramene.org/perl/protein_search?acc=$acc_no",
	interpro=>"http://www.ebi.ac.uk/interpro/IEntry?ac=$acc_no",
	metacyc=>"http://biocyc.org/META/substring-search?type=NIL&amp;object=$acc_no",
	mgi=>"http://www.informatics.jax.org/searches/accession_report.cgi?id=$acc_no",
	ncbi_taxid => "http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=$acc_no",
	omim=>"http://www.ncbi.nlm.nih.gov/entrez/dispomim.cgi?id=$acc_no",
	pfam=>"http://www.sanger.ac.uk/cgi-bin/Pfam/getacc?$acc_no",
	pmid=>"http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&amp;db=PubMed&amp;dopt=Abstract&amp;list_uids=$acc_no",
	pseudocap=>"http://v2.pseudomonas.com/getAnnotation.do?locusID=$acc_no",
	pubmed=>"http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&amp;db=PubMed&amp;dopt=Abstract&amp;list_uids=$acc_no",
	reactome=>"http://www.reactome.org/cgi-bin/eventbrowser?DB=gk_current&amp;ID=$acc_no",
	rgd=>"http://rgd.mcw.edu/tools/genes/genes_view.cgi?id=$acc_no",
	sgd=>"http://db.yeastgenome.org/cgi-bin/locus.pl?sgdid=$acc_no",
	sp=>"http://srs.ebi.ac.uk/srs6bin/cgi-bin/wgetz?-e+[SWALL-acc:$acc_no]",
	sptr=>"http://srs.ebi.ac.uk/srs6bin/cgi-bin/wgetz?-e+[SWALL-acc:$acc_no]",
	'swiss-prot' => "http://www.ebi.uniprot.org/uniprot-srv/uniProtView.do?proteinac=$acc_no",
	tair=>"http://arabidopsis.org/servlets/TairObject?accession=$acc_no",
	tigr_ath1=>"http://www.tigr.org/tigr-scripts/euk_manatee/shared/ORF_infopage.cgi?db=ath1&amp;orf=$acc_no",
	tigr_cmr=>"http://cmr.tigr.org/tigr-scripts/CMR/shared/GenePage.cgi?locus=$acc_no",
	tigr_egad=>"http://www.tigr.org/tigr-scripts/CMR2/ht_report.spl?prot_id=$acc_no",
	tigr_pfa1=>"http://www.tigr.org/tigr-scripts/euk_manatee/shared/ORF_infopage.cgi?db=pfa1&amp;orf=$acc_no",
	tigr_tba1=>"http://www.tigr.org/tigr-scripts/euk_manatee/shared/ORF_infopage.cgi?db=tba1&amp;orf=$acc_no",
	tigr_tigrfams=>"http://cmr.tigr.org/tigr-scripts/CMR/HmmReport.cgi?hmm_acc=$acc_no",
	tigrfams=>"http://cmr.tigr.org/tigr-scripts/CMR/HmmReport.cgi?hmm_acc=$acc_no",
	'tr' => "http://srs.ebi.ac.uk/srs6bin/cgi-bin/wgetz?-e+[SWALL-acc:$acc_no]",
	uniprot => "http://www.ebi.uniprot.org/uniprot-srv/uniProtView.do?proteinac=$acc_no",
	wb => "http://www.wormbase.org/db/searches/basic?class=Any&amp;query=$acc_no",
	zfin => "http://zfin.org/cgi-bin/ZFIN_jump?record=$acc_no",
	);
  return $db_hash{$database} || undef;
}

=head2 get_ref_url

parameters: database_abbreviation, acc_no

returns: url to the entry

This gets a link to evidence for gene product associations.
This is to a reference database, which in some organizations
is separate from the main database (ie SGD).

Also, it does a little munging of IDs that dont come in right.

=cut

sub get_ref_url {
  my $self = shift;
  my $database = shift;
  my $acc_no = shift;

  $database = lc($database);

	if ($database eq "sgd") {
		$acc_no =~ s/\|.*$//;
	} elsif ($database eq "fb") {
		$acc_no =~ s/^fb/FB/;
	}
#		elsif (lc($database) eq "dros cdna" || $database eq "image") {
#		my $u_env = get_environment_param('ref_url');
#		$url = $u_env ? $u_env.$acc_no :
#			"http://toy.lbl.gov:8888/cgi-bin/ex/exgo_report.pl?image_dbxref=$acc_no";
#		return $url;
#	}

  my %db_hash = 
(	ddb=>"http://dictybase.org/db/cgi-bin/dictyBase/reference/reference.pl?refNo=$acc_no",
#	"ec"=>"http://ca.expasy.org/cgi-bin/nicezyme.pl?$acc_no",
#	"ensembl"=>"http://www.ensembl.org/perl/protview?peptide=$acc_no",
#	"fb"=>"http://flybase.bio.indiana.edu/.bin/fbidq.html?$acc_no",
	gr=>"http://www.gramene.org/perl/pub_search?ref_id=$acc_no",
	http=>"http:$acc_no",
#	"metacyc"=>"http://biocyc.org/META/substring-search?type=NIL&amp;object=$acc_no",
	mgi=>"http://www.informatics.jax.org/searches/accession_report.cgi?id=$acc_no",
#	"pseudocap"=>"http://v2.pseudomonas.com/getAnnotation.do?locusID=$acc_no",
	rgd=>"http://rgd.mcw.edu/tools/references/references_view.cgi?id=$acc_no",
	sgd=>"http://db.yeastgenome.org/cgi-bin/SGD/reference/reference.pl?refNo=$acc_no",
	sgd_ref=>"http://db.yeastgenome.org/cgi-bin/reference/reference.pl?dbid=$acc_no",
	swall=>"http://ca.expasy.org/cgi-bin/sprot-search-de?S=1&amp;T=1&amp;SEARCH=$acc_no",
	tc=>"http://www.tcdb.org/tcdb/index.php?tc=$acc_no",
	tigr_egad=>"http://www.tigr.org/tigr-scripts/CMR2/ht_report.spl?prot_id=$acc_no",
	tigr_ref=>"http://www.tigr.org/tdb/GO_REF/GO_REF.shtml",
#	"tigr_tigrfams"=>"http://cmr.tigr.org/tigr-scripts/CMR/HmmReport.cgi?hmm_acc=$acc_no",
#	"tigrfams"=>"http://cmr.tigr.org/tigr-scripts/CMR/HmmReport.cgi?hmm_acc=$acc_no",
	url=>"$acc_no",
#	"zfin"=>"http://zfin.org/cgi-bin/ZFIN_jump?record=$acc_no",
#	"swiss-prot"=>"http://www.ebi.uniprot.org/uniprot-srv/uniProtView.do?proteinac=$acc_no",
#	"uniprot"=>"http://www.ebi.uniprot.org/uniprot-srv/uniProtView.do?proteinac=$acc_no",
	);
	
	if (!$db_hash{$database})
	{	return $self->get_url($database, $acc_no);
	}
	return $db_hash{$database} || undef;
}

sub get_db_url {
my $self = shift;
my $db = lc(shift);

	my %db_hash = (
	agbase => "http://www.agbase.msstate.edu/",
	cgd => 'http://www.candidagenome.org/',
	dictybase => 'http://dictybase.org/',
	fb => 'http://flybase.bio.indiana.edu/',
	gdb => "http://www.gdb.org/",
	genedb_lmajor => 'http://www.genedb.org/',
	genedb_pfalciparum => 'http://www.genedb.org/',
	genedb_spombe => 'http://www.genedb.org/',
	genedb_tbrucei => 'http://www.genedb.org/',
	gr => 'http://www.gramene.org/',
	hgnc => "http://www.gene.ucl.ac.uk/nomenclature/",
	intact => "http://www.ebi.ac.uk/intact/",
	lifedb => "http://www.lifedb.de/",
	mgi => 'http://www.informatics.jax.org/',
	pinc => "http://www.proteome.com/",
	pseudocap => 'http://v2.pseudomonas.com/',
	reactome => 'http://www.reactome.org',
	rgd => 'http://rgd.mcw.edu/',
	roslin_institute => "http://www.roslin.ac.uk/",
	sgd => 'http://www.yeastgenome.org/',
	tair => 'http://www.tair.org/',
	tigr => 'http://www.tigr.org/',
	tigr_cmr => 'http://www.tigr.org/',
	tigr_tba1 => 'http://www.tigr.org/',
	uniprot => 'http://www.ebi.uniprot.org/',
	uniprotkb => 'http://www.ebi.uniprot.org/',
	wb => 'http://www.wormbase.org/',
	zfin => 'http://zfin.org/',
	);

	return $db_hash{$db} || undef;

}

=head2 get_human_name

parameters: database_abbreviation

returns: Human readable name

get_url takes a database abbreviation from GO and accession
and returns a human friendly name to the datasource.

=cut

sub get_human_name {
	my $self = shift;
	my $abbr = shift;
  
	my $human_name = {
		cgen => 'Compugen',
		ddb => 'dictyBase',
		ec => 'NiceZyme',
		egad => 'EGAD',
		ensembl => 'Ensembl',
		fb => 'FlyBase',
		genedb_lmajor => 'GeneDB L. major',
		genedb_pfalciparum => 'GeneDB P. falciparum',
		genedb_spombe => 'GeneDB S. pombe',
		genedb_tbrucei => 'GeneDB T. brucei',
		genedb_tsetse => 'GeneDB Tsetse',
		gr => 'Gramene',
		interpro => 'InterPro',
		mgi => 'MGI',
		pinc => 'Proteome Inc.',
		pombase => 'Pombase',
		reactome => 'Reactome',
		rgd => 'RGD',
		roslin_institute => 'Roslin Institute',
		sgd => 'SGD',
		sp => 'SWISS-PROT',
		sp_kw => 'SWISS-PROT keyword',
		sptr => 'SPTr',
		tair => 'TAIR',
		tigr_ath1 => 'TIGR Ath1',
		tigr_cmr => 'TIGR CMR',
		tigr_role => 'TIGR',
		tigr_tba1 => 'TIGR Tba1',
		tigrfams => 'TIGRFAMS',
		'tr' => 'TrEMBL',
		uniprot => 'UniProt',
		wb => 'Wormbase',
		zfin => 'ZFIN',
		all => 'All',
		aca => 'All Curator Approved',
#	relationships
		is_a => 'is a',
		part_of => 'part of',
		develops_from => 'develops from',
#	form labels
		taxid => 'Species',
		evcode => 'Evidence Code',
		gptype => 'Gene Product Type',
		ont => 'Ontology',
		speciesdb => 'Data source',
		gpfields => 'Search fields',
		termfields => 'Search fields',
		assby => 'Assigned by',
		qual => 'Qualifier',
		search_constraint => 'Search GO',
		term => 'Terms',
		gp => 'Genes or proteins',
		parents => 'term ancestors',
		sibling => 'term parents, siblings and children',
		direct =>'direct associations',
		list =>'all associations',
		graph_bgcolor => 'Box color',
		graph_textcolor => 'Text color',
		term_context => 'Term context',
		tree_view => 'Tree view',
#	qualifiers
		'not|contributes_to' => 'does not contribute to',
#	BLAST labels
		threshold => 'Expect threshold',
		maxhits => 'Maximum number of alignments',
		blast_filter => 'BLAST filter',
#	ontology names
#		biological_process => 'biological process',
#		cellular_component => 'cellular component',
#		molecular_function => 'molecular function',
		plant_growth_and_development_stage => 'Plant Growth Stage',
#	graphviz colours
		forestgreen => 'forest green',
		navy => 'navy blue',
		skyblue => 'sky blue',
	};
  
  return $human_name->{lc($abbr)} if $human_name->{lc($abbr)};
  $abbr =~ s/_/ /g;
  return $abbr;
}

=head2 get_full_name

parameters: -acronym=>$acronym

returns: Full name

=cut

sub get_full_name {
	my $self = shift;
	my $acronym = shift;
#	my ($acronym) =
#	rearrange(['acronym'], @_);

	my %acronyms = 
	(	IMP => 'Inferred from Mutant Phenotype',
		ISS => 'Inferred from Sequence Similarity',
		IGI => 'Inferred from Genetic Interaction',
		IPI => 'Inferred from Physical Interaction',
		IDA => 'Inferred from Direct Assay',
		IEP => 'Inferred from Expression Pattern',
		IEA => 'Inferred from Electronic Annotation',
		TAS => 'Traceable Author Statement',
		NAS => 'Non-traceable Author Statement',
		ND =>'No Biological Data Available',
		RCA => 'Inferred from Reviewed Computational Analysis',
		IC => 'Inferred by Curator',
		IGC => 'Inferred from Genomic Context',
		NR => 'Not Recorded',
		all => 'All',
		aca => 'All Curator Approved',
		ca => 'All Curator Approved'
	);
	return $acronyms{$acronym} || $acronym;
}

=head2 case_replace

parameters: gpxref

returns: case-corrected gpxref

=cut

sub case_replace {
	my $xref = shift;
	my ($db, $acc) = split /[:\|]/, $xref, 2;
	
	my %case_corrected = (
		cgd => "CGD",
		dictybase => "Dictybase",
		fb => "FB",
		genedb_lmajor => "GeneDB_Lmajor",
		genedb_pfalciparum => "GeneDB_Pfalciparum",
		genedb_spombe => "GeneDB_Spombe",
		genedb_tbrucei => "GeneDB_Tbrucei",
		uniprot => "UniProt",
		gr => "GR",
		mgi => "MGI",
		pseudocap => "PseudoCAP",
		rgd => "RGD",
		sgd => "SGD",
		tair => "TAIR",
		tigr_cmr => "TIGR_CMR",
		tigr_tba1 => "TIGR_Tba1",
		wb => "WB",
		zfin => "ZFIN");
	
	$db = $case_corrected{ lc($db) } || $db;
	
	return "$db:$acc";
}

sub get_field_name {
	my $self = shift;
	my $field = shift;

	my %fields = (
		all =>'all fields',
		comment =>'comment',
		dbxref =>'database cross-references',
		xref =>'database cross-references',
		definition =>'definition',
		full_name => 'full name(s)',
		name =>'term name or ID',
		product_synonym => 'synonyms',
		seq_xref => 'sequence cross-references',
		seq_name => 'sequence ID',
		subset =>'subset',
		symbol => 'symbol',
		synonym =>'synonyms',
		term_synonym => 'synonyms',
		gpxref => 'database ID',
		genus => 'genus',
		species => 'species',
		ncbi_taxa_id => 'NCBI taxon ID',
		binomial => 'binomial species name',
		common_name => 'common name',
	);
	return $fields{$field} || undef;
}

sub spp_dl_url {
=insert stuff
GeneDB_Lmajor
[5664]

GeneDB_Pfalciparum
[5833]

GeneDB_Spombe
[4896]

GeneDB_Tbrucei
[185431]

GeneDB_tsetse
[37546]

Candida Genome Database (CGD)
[5476]

dictyBase (ddb)
[5782, 44689]

FlyBase (FB)
[7227]

goa_chicken
[9031]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.goa_chicken.gz?rev=HEAD

goa_cow
[9913]


goa_human
[9606]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.goa_human.gz?rev=HEAD

gramene_oryza
[4528, 4529, 4530, 4532, 4533, 4534, 4535, 4536, 4537, 4538, 4539, 29689, 29690, 39946, 39947, 40148, 40149, 52545, 63629, 65489, 65491, 77588, 83307, 83308, 83309, 110450, 110451, 127571, 364099, 364100]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.gramene_oryza.gz?rev=HEAD

Mouse Genome Informatics (MGI)
[10090]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.mgi.gz?rev=HEAD

Rat Genome Database (RGD)
[10116]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.rgd.gz?rev=HEAD

Saccharomyces Genome Database (SGD)
[4932]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.sgd.gz?rev=HEAD

The Arabidopsis Information Resource (TAIR)
[3702]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.tair.gz?rev=HEAD

tigr_Banthracis
[198094]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.tigr_Banthracis.gz?rev=HEAD

tigr_Cburnetii
[227377]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.tigr_Cburnetii.gz?rev=HEAD

tigr_Cjejuni
[195099]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.tigr_Cjejuni.gz?rev=HEAD

tigr_Dethenogenes
[243164]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.tigr_Dethenogenes.gz?rev=HEAD

tigr_Gsulfurreducens
[243231]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.tigr_Gsulfurreducens.gz?rev=HEAD

tigr_Lmonocytogenes
[265669]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.tigr_Lmonocytogenes.gz?rev=HEAD

tigr_Mcapsulatus
[243233]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.tigr_Mcapsulatus.gz?rev=HEAD

tigr_Psyringae
[223283]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.tigr_Psyringae.gz?rev=HEAD

tigr_Soneidensis
[211586]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.tigr_Soneidensis.gz?rev=HEAD

tigr_Spomeroyi
[246200]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.tigr_Spomeroyi.gz?rev=HEAD

tigr_Tbrucei_chr2
[5691]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.tigr_Tbrucei_chr2.gz?rev=HEAD

tigr_Vcholerae
[686]
gene_association.tigr_Vcholerae.gz?rev=HEAD

Wormbase (WB)
[6239]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.wb.gz?rev=HEAD

Zebrafish Information Network (ZFIN)
[7955]
http://cvsweb.geneontology.org/cgi-bin/cvsweb.cgi/go/gene-associations/gene_association.zfin.gz?rev=HEAD
=cut
}

sub hilite {
	my $self = shift;
	my $text = shift;
	my $search = shift;
	return $text;
#	print STDERR "starting hilite...\n";

	my $matchstr = decode_entities($text);
	
#	my $queryset = $search->{queryset}{orig};
	my $queryset = $search->{queryset}{perllist};

#	print STDERR "matchstr: ".Dumper($matchstr)."\n";
#	print STDERR "queryset: ".Dumper($queryset)."\n";
#	print STDERR "queryset_perl: ".Dumper($queryset_perl)."\n";

	if (!$matchstr || !$queryset)
	{	return $text;
	}


	QUERY_LIST:
	foreach my $q (@$queryset)
	{	#print STDERR 'testing against '.Dumper($q);
		foreach (@$q)
		{	if ($_->{regexp})
			{	next QUERY_LIST unless ($matchstr =~ s/($_->{value})/START_MATCH$1END_MATCH/gi);
			}
			else
			{	next QUERY_LIST unless ($matchstr =~ s/($_->{value})/START_MATCH$1END_MATCH/gi);
			}
		}
		encode_entities($matchstr);
		$matchstr =~ s/START_MATCH(.*?)END_MATCH/<em class="hilite">$1<\/em>/g;
		return $matchstr;
	}
	return $text;
}

sub _highlighting {
	my $m = shift;
	my $str = shift;
	my $found;
	if ($m =~ /\w/)
	{	$m =~ s/\*//g;
		
	#	$found = $str =~ s/(\Q$m\E)/<span class="hilite">$1<\/span>/i;
		$found = $str =~ s/(\Q$m\E)/<>$1<>/i;
	}
	return ($found, $str);
}

sub markup_comment {
	my $self = shift;
	my $session_id = shift;
	my $comment = shift || return;
	$comment =~ s/GO\\:/GO:/g;
	$comment =~ s/(GO:\d+)/<a href=\"term-details.cgi?term=$1&amp;session_id=$session_id\">$1<\/a>/g;
	return $comment;
}

sub markup_search_comment {
	my $self = shift;
	my $session_id = shift;
	my $comment = shift || return;
	$comment =~ s/GO\\:/GO:/g;
	$comment =~ s/.*?(To update annotations.*)/$1/i;
	if ($comment =~ /GO:\d{7}/)
	{	$comment =~ s/(GO:\d+)/<a href=\"term-details.cgi?term=$1&amp;session_id=$session_id\">$1<\/a>/g;
		return $comment;
	}
	return;
}

sub aspect_abbrev {
	my $self = shift;
	my $aspect = shift;

	 my %aspect_h =
	   (biological_process=>'P',
		molecular_function=>'F',
		cellular_component=>'C'
	   );
#	my %aspect_h =
#	  (biological_process=>'process',
#	   molecular_function=>'function',
#	   cellular_component=>'component'
#	  );
	return $aspect_h{$aspect} || $aspect;
}

sub get_GO_doc_url {
	my $self = shift;
	my $type = shift;
	my $val = lc(shift);

	my $GO_url = "http://www.geneontology.org";
	my %url_h =
	(	aspect =>"$GO_url/GO.doc.shtml",
		evidence =>"$GO_url/GO.evidence.shtml",
		ont =>"$GO_url/GO.doc.shtml",
		evcode =>"$GO_url/GO.evidence.shtml",
	);
	return $url_h{$type}."#$val";
}



=not used

# UNUSED
sub get_link_to {
  my $self = shift;
  my ($session, $extension) =
	rearrange(['session', 'extension'], @_);

  my $url;

  my $ref = $session->get_param('link_to');
  if ($ref) {
	return $ref.$extension;
  }
  return undef;
}

# UNUSED
=head2 get_xref_image

parameters: database_abbreviation, acc_no

returns: path to the gif or png for each databases logo

This is the picture shown for the link that gets the reciprocal
terms for each xref.

=cut

=not used

sub get_xref_image {
  my $self = shift;
  my ($session, $database, $acc_no) =
	rearrange(['session', 'database', 'acc_no'], @_);

  my $image_dir = $session->get_param('image_dir') || "../images";


  my $url;
  if ($database eq "interpro") {
	$url = "$image_dir/interpro.gif";
  }
	return $url;
}

# UNUSED
sub get_matching_synonyms {
	my $self = shift;
	my $synonyms = shift;
	my $session = shift;

	my $params = $session->get_param_hash;
	my @query = split('\0', $params->{'query'});
	my @match = ();
	map{
		my $q = $_;
		map{
			my ($found, $syn) = &_highlighting($q, $self->spel_greek($_));
			if ($found) {
				push @match, $syn;
				}
			}@{$synonyms || []};
		}@query;
	my @a = grep{$_}@match;
	if (@a) {
		map { s/<>(.*?)<>/<span class="hilite">$1<\/span>/g; } @a;
		return [@a];
	} else {
		return [];
	}
}

# UNUSED
sub markup_matching_text {
	my $self = shift;
	my $fulln = shift;
	my $session = shift;

	my $params = $session->get_param_hash;
	my @query = split(/(\0|\s+)/, $params->{'query'});
	my $last = ''; my @unique;
	foreach my $q (sort @query)
	{	if ($q ne $last)
		{	push @unique, $q;
		}
		$last = $q;
	}
#	my @query = split(/\0/, %param_hash->{'query'});
	
#	if (%param_hash->{'boolean'} eq 'yes')
#	{	map{ s/^(\||AND|NOT|OR)$// }@query;
#	}
	
	map{$fulln = _highlighting($_,$fulln)}@unique;
	$fulln =~ s/<>(.*?)<>/<span class="hilite">$1<\/span>/g;
	return $fulln;
}

# UNUSED
sub get_alt_row_label {
	my $self = shift;
	my $c = shift || 0;

	my %label_h = (0=>'even_row', 1=>'odd_row');
	return $label_h{$c % 2};
}

#	UNUSED
sub get_paging_href {
	my $self = shift;
	my $page = shift;
	my $ses = shift;
	my $anchor = shift;
	my $label = shift || $page;
	#show_associations is NOT hidden param, need to include in url for detail view
	#and query view has action param
	my $href;
	my $name_anchor = "#$anchor" if ($anchor);

	#to avoid closure here
	 local *_param_string = sub {
		my $s = "session_id=".$ses->get_param('session_id');
		$s .= "&amp;page=$page" if ($page);
		map {
			$s .= "&amp;$_=".$ses->get_param($_) if ($ses->get_param($_));
		}qw(view search_constraint depth query sort action);
		$s .= $name_anchor;
		$s =~ s/\s/%20/g;
		return $s;
	};
	$href = sprintf("<a href=\"go.cgi?%s\">$label</a>", _param_string());
#	  if ($ses->get_param('view') =~ 'detail') {
#		  $href = sprintf("<a href=\"go.cgi?session_id=%s&page=$page&view=%s&search_constraint=%s&depth=%s&query=%s&show_associations=%s$name_anchor\">$label</a>",$ses->get_param('session_id'),$ses->get_param('view')||'',$ses->get_param('search_constraint')||'',$ses->get_param('depth')||'',$ses->get_param('query')||'',$ses->get_param('show_associations')||'');
#	  } else {
#		  $href = sprintf("<a href=\"go.cgi?session_id=%s&page=$page&view=%s&search_constraint=%s&depth=%s&query=%s&action=%s$name_anchor\">$label</a>",$ses->get_param('session_id'),$ses->get_param('view')||'',$ses->get_param('search_constraint')||'',$ses->get_param('depth')||'',$ses->get_param('query')||'',$ses->get_param('action')||'');
#	  }
	return $href;
}

#	UNUSED
sub get_page_href {
	my $self = shift;
	my $ses = shift;
	my $page = shift;
	#show_associations is NOT hidden param, need to include in url for detail view
	#and query view has action param
	my $href;

	#to avoid closure here
	 local *_param_string = sub {
		my $s = "session_id=".$ses->get_param('session_id');
		map {
			$s .= "&amp;$_=".$ses->get_param($_) if ($ses->get_param($_));
		}qw(view search_constraint depth sort action);
		map { $s .= "&amp;query=$_" if $_ }@{$ses->get_param_list('query')};
		$s =~ s/\s/%20/g;
		$s .= "&amp;page=$page" if ($page);
		return $s;
	};
	$href = sprintf("go.cgi?%s", _param_string());
	return $href;
}

#	UNUSED
sub commify {
	my $self = shift;
	my $text = reverse $_[0];
	$text =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
	return scalar reverse $text;
}
=cut
1;
