package GO::CGI::NameMunger;

use strict;
#use GO::Utils qw(rearrange spell_greek);
use FreezeThaw qw(freeze thaw);
use Data::Dumper;
use HTML::Entities;
use Exporter;
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);
@ISA = qw(Exporter);
#@EXPORT_OK = qw(case_replace);
@EXPORT_OK = qw(case_replace get_field_name_fn get_full_name_fn get_human_name_fn get_GO_doc_url_fn get_db_url_fn);
#@EXPORT_OK = qw(case_replace get_field_name_fn get_url_fn);
%EXPORT_TAGS = (all => [@EXPORT_OK]);

## Prepare new AmiGO requirements.
use AmiGO;
our $core = AmiGO->new() if ! $core;


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
  return get_url_fn(@_);
}

sub get_url_fn {

  my $database = shift;
  my $acc_no = shift;

  $database = lc($database);
  #	if ($database eq "tair") {
  #		$acc_no =~ s/^TAIR://;
  #	} elsif ($database eq "dros cdna") {
  #		$database =~ s/\ /\_/;
  #	}

  #is it safe to trim any thing after first word?
  $acc_no =~ s/^(\S+)\s+.*/$1/;

  ## First, let's try and get this done with the new stuff;
  ## otherwise fall back to the antiques.
  my $new_try = $core->database_link($database, $acc_no);
  if( defined $new_try ){
    $core->kvetch("defined: db: " . $database . ", acc: " . $acc_no);
    return $new_try;
  }else{
    $core->kvetch("not defined: db: " . $database . ", acc: " . $acc_no);
  }


  ## Ancient fallbacks in case of problems.
  $core->kvetch("check fallback...");
  my %db_hash =
    (
agi_locuscode => "http://www.arabidopsis.org/servlets/TairObject?type=locus&amp;name=$acc_no",
brenda => "http://www.brenda.uni-koeln.de/php/result_flat.php4?ecno=$acc_no",
cbs => "http://www.cbs.dtu.dk/services/$acc_no/",
cgd => "http://www.candidagenome.org/cgi-bin/locus.pl?dbid=$acc_no",
cgd_locus => "http://www.candidagenome.org/cgi-bin/locus.pl?locus=$acc_no",
cgd_ref => "http://www.candidagenome.org/cgi-bin/reference/reference.pl?dbid=$acc_no",
chebi => "http://www.ebi.ac.uk/chebi/searchId.do?chebiId=CHEBI:$acc_no",
ddb => "http://dictybase.org/db/cgi-bin/gene_page.pl?dictybaseid=$acc_no",
ddb_ref => "http://dictybase.org/db/cgi-bin/dictyBase/reference/reference.pl?refNo=$acc_no",
dictybase => "http://dictybase.org/db/cgi-bin/gene_page.pl?dictybaseid=$acc_no",
## Next two didn't seem to be here, but appear in db--all this to go
## when switch to new backend.
dictybase_gene_name => "http://dictybase.org/db/cgi-bin/gene_page.pl?gene_name=$acc_no",
dictybase_ref => "http://dictybase.org/db/cgi-bin/dictyBase/reference/reference.pl?refNo=$acc_no",
doi => "http://www.doi.org/$acc_no",
echobase => "http://www.biolws1.york.ac.uk/echobase/Gene.cfm?recordID=$acc_no",
eck => "http://www.ecogene.org/geneInfo.php?eck_id=$acc_no",
ecocyc => "http://biocyc.org/ECOLI/NEW-IMAGE?type=NIL&amp;object=$acc_no",
ecogene => "http://www.ecogene.org/geneInfo.php?eg_id=$acc_no",
embl => "http://www.ebi.ac.uk/cgi-bin/emblfetch?style=html&amp;Submit=Go&amp;id=$acc_no",
ensembl => "http://www.ensembl.org/perl/protview?peptide=$acc_no",
fb => "http://flybase.bio.indiana.edu/reports/$acc_no.html",
flybase => "http://flybase.bio.indiana.edu/reports/$acc_no.html",
gdb => "http://www.gdb.org/gdb-bin/genera/accno?accessionNum=$acc_no",
genedb_lmajor => "http://www.genedb.org/genedb/Search?organism=leish&name=$acc_no",
genedb_pfalciparum => "http://www.genedb.org/genedb/Search?organism=malaria&name=$acc_no",
genedb_spombe => "http://www.genedb.org/genedb/Search?organism=pombe&name=$acc_no",
genedb_tbrucei => "http://www.genedb.org/genedb/Search?organism=tryp&name=$acc_no",
go => "http://amigo.geneontology.org/cgi-bin/amigo/term_details?term=$acc_no",
go_evcode => "http://www.geneontology.org/GO.evidence.shtml#$acc_no",
go_ont =>"http://www.geneontology.org/GO.doc.shtml#$acc_no",
go_qual => "http://www.geneontology.org/GO.annotation.conventions.shtml#qual",
go_ref => "http://www.geneontology.org/cgi-bin/references.cgi#GO_REF:$acc_no",
gr => "http://www.gramene.org/db/protein/protein_search?acc=$acc_no",
gr_ref => "http://www.gramene.org/db/literature/pub_search?ref_id=$acc_no",
gramene => "http://www.gramene.org/db/protein/protein_search?acc=$acc_no",
'h-invdb' => "http://jbirc.jbic.or.jp/hinv/soup/pub_Detail.pl?acc_id=$acc_no",
'h-invdb_locus' => "http://jbirc.jbic.or.jp/hinv/soup/pub_Locus.pl?locus_id=$acc_no",
hamap => "http://us.expasy.org/unirules/$acc_no",
hgnc => "http://www.genenames.org/data/hgnc_data.php?hgnc_id=$acc_no",
http => "http:$acc_no",
img => "http://img.jgi.doe.gov/cgi-bin/pub/main.cgi?section=GeneDetail&amp;page=geneDetail&amp;gene_oid=$acc_no",
intact => "http://www.ebi.ac.uk/intact/search/do/search?searchString=$acc_no",
interpro => "http://www.ebi.ac.uk/interpro/DisplayIproEntry?ac=$acc_no",
isbn => "http://my.linkbaton.com/get?lbCC=q&amp;nC=q&amp;genre=book&amp;item=$acc_no",
kegg_pathway => "http://www.genome.ad.jp/dbget-bin/www_bget?path:$acc_no",
ma => "http://www.informatics.jax.org/searches/AMA.cgi?id=$acc_no",
maizegdb => "http://www.maizegdb.org/cgi-bin/id_search.cgi?id=$acc_no",
maizegdb_locus => "http://www.maizegdb.org/cgi-bin/displaylocusresults.cgi?term=$acc_no",
mesh => "http://www.nlm.nih.gov/cgi/mesh/2005/MB_cgi?mode=&amp;term=$acc_no",
metacyc => "http://biocyc.org/META/substring-search?type=NIL&amp;object=$acc_no",
mgi => "http://www.informatics.jax.org/searches/accession_report.cgi?id=$acc_no",
mips_funcat => "http://mips.gsf.de/cgi-bin/proj/funcatDB/search_advanced.pl?action=2&amp;wert=$acc_no",
ncbi_gene => "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&amp;db=gene&amp;dopt=full_report&amp;list_uids=$acc_no",
ncbi_gi => "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&amp;db=nucleotide&amp;dopt=GenBank&amp;list_uids=$acc_no",
ncbi_gp => "http://www.ncbi.nlm.nih.gov/entrez/viewer.fcgi?db=protein&amp;val=$acc_no",
ncbi_taxid => "http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=$acc_no",
omim => "http://www.ncbi.nlm.nih.gov/entrez/dispomim.cgi?id=$acc_no",
pdb => "http://www.rcsb.org/pdb/cgi/explore.cgi?pdbId=$acc_no",
pfam => "http://www.sanger.ac.uk/cgi-bin/Pfam/getacc?$acc_no",
pir => "http://pir.georgetown.edu/cgi-bin/pirwww/nbrfget?uid=$acc_no",
pirsf => "http://pir.georgetown.edu/cgi-bin/ipcSF?id=$acc_no",
pmid => "http://www.ncbi.nlm.nih.gov/pubmed/$acc_no",
po => "http://www.plantontology.org/amigo/go.cgi?action=query&amp;view=query&amp;search_constraint=terms&amp;query=$acc_no",
prints => "http://umber.sbs.man.ac.uk/cgi-bin/dbbrowser/PRINTS/DoPRINTS.pl?cmd_a=Display&amp;qua_a=none&amp;fun_a=Text&amp;qst_a=$acc_no",
prodom => "http://prodes.toulouse.inra.fr/prodom/current/cgi-bin/request.pl?question=DBEN&amp;query=$acc_no",
prosite => "http://www.expasy.ch/cgi-bin/prosite-search-ac?$acc_no",
pseudocap => "http://v2.pseudomonas.com/getAnnotation.do?locusID=$acc_no",
pubchem_bioassay => "http://pubchem.ncbi.nlm.nih.gov/assay/assay.cgi?aid=$acc_no",
pubchem_compound =>"http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?CMD=search&amp;DB=pccompound&amp;term=$acc_no",
pubchem_substance => "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?CMD=search&amp;DB=pcsubstance&amp;term=$acc_no",
reactome => "http://www.reactome.org/cgi-bin/eventbrowser?DB=gk_current&amp;ID=$acc_no",
rebase => "http://rebase.neb.com/rebase/enz/$acc_no.html",
resid => "http://srs.ebi.ac.uk/srsbin/cgi-bin/wgetz?-e+[RESID:'$acc_no']",
rgd => "http://rgd.mcw.edu/tools/genes/genes_view.cgi?id=$acc_no",
rgdid => "http://rgd.mcw.edu/tools/genes/genes_view.cgi?id=$acc_no",
rnamods => "http://medlib.med.utah.edu/cgi-bin/rnashow.cgi?$acc_no",
sgd => "http://db.yeastgenome.org/cgi-bin/locus.pl?sgdid=$acc_no",
sgd_locus => "http://db.yeastgenome.org/cgi-bin/locus.pl?locus=$acc_no",
sgd_ref => "http://db.yeastgenome.org/cgi-bin/reference/reference.pl?dbid=$acc_no",
sgdid => "http://db.yeastgenome.org/cgi-bin/locus.pl?sgdid=$acc_no",
smart => "http://smart.embl-heidelberg.de/smart/do_annotation.pl?BLAST=DUMMY&amp;DOMAIN=$acc_no",
sp_kw => "http://www.expasy.org/cgi-bin/get-entries?KW=$acc_no",
sptr => "http://srs.ebi.ac.uk/srs6bin/cgi-bin/wgetz?-e+[SWALL-acc:$acc_no]",
'swiss-prot' => "http://www.ebi.uniprot.org/entry/$acc_no",
swissprot => "http://www.ebi.uniprot.org/entry/$acc_no",
tair => "http://arabidopsis.org/servlets/TairObject?accession=$acc_no",
tc => "http://www.tcdb.org/tcdb/index.php?tc=$acc_no",
tigr_ath1 => "http://www.tigr.org/tigr-scripts/euk_manatee/shared/ORF_infopage.cgi?db=ath1&amp;orf=$acc_no",
tigr_cmr => "http://cmr.tigr.org/tigr-scripts/CMR/shared/GenePage.cgi?locus=$acc_no",
tigr_egad => "http://www.tigr.org/tigr-scripts/CMR2/ht_report.spl?prot_id=$acc_no",
tigr_genprop => "http://www.tigr.org/tigr-scripts/CMR2/genome_property_def.spl?prop_acc=GenProp0120",
tigr_pfa1 => "http://www.tigr.org/tigr-scripts/euk_manatee/shared/ORF_infopage.cgi?db=pfa1&amp;orf=$acc_no",
tigr_ref => "http://www.tigr.org/tdb/GO_REF/GO_REF.shtml",
tigr_tba1 => "http://www.tigr.org/tigr-scripts/euk_manatee/shared/ORF_infopage.cgi?db=tba1&amp;orf=$acc_no",
tigr_tigrfams => "http://cmr.tigr.org/tigr-scripts/CMR/HmmReport.cgi?hmm_acc=$acc_no",
tigrfams => "http://cmr.tigr.org/tigr-scripts/CMR/HmmReport.cgi?hmm_acc=$acc_no",
trembl => "http://www.ebi.uniprot.org/entry/$acc_no",
'um-bbd_enzymeid' => "http://umbbd.msi.umn.edu/servlets/pageservlet?ptype=ep&amp;enzymeID=$acc_no",
'um-bbd_pathwayid' => "http://umbbd.msi.umn.edu/acr/".$acc_no."_map.html",
uniprot => "http://www.ebi.uniprot.org/entry/$acc_no",
uniprotkb => "http://www.ebi.uniprot.org/entry/$acc_no",
'uniprotkb/swiss-prot' => "http://www.ebi.uniprot.org/entry/$acc_no",
'uniprotkb/trembl' => "http://www.ebi.uniprot.org/entry/$acc_no",
url => "$acc_no",
wb => "http://www.wormbase.org/db/searches/basic?class=Any&amp;query=$acc_no",
wb_ref => "http://www.wormbase.org/db/misc/paper?name=$acc_no",
wormbase => "http://www.wormbase.org/db/searches/basic?class=Any&amp;query=$acc_no",
wormpep => "http://www.wormbase.org/db/get?class=Protein;name=$acc_no",
wp => "http://www.wormbase.org/db/get?class=Protein;name=$acc_no",
zfin => "http://zfin.org/cgi-bin/ZFIN_jump?record=$acc_no",
url=>"$acc_no",
	);

        ## Did I get a direct hit out of the old NameMunger?
        if( $db_hash{$database} ){
           $core->kvetch("fallback hit: " . $db_hash{$database});
	   return $db_hash{$database};
	}else{
           $core->kvetch("nothing: goto second fallback");
	}

	if ($database eq 'um' && $acc_no =~ /:/)
	{	my ($db, $acc) = split(':', $acc_no, 1);
		$database .= '-'.$db;
		$acc_no = $acc;
		return $db_hash{$database} || undef;
	}
	elsif ($database eq 'ec'){
	  #$acc_no =~ s/\./\//g;
	  #if ($acc_no =~ /\d+\.\d+\.\d+\.\d+/){ # full EC number
	  #  $acc_no .= '.html';
	  #}
	  #return "http://www.chem.qmul.ac.uk/iubmb/enzyme/EC$acc_no";
	  return 'http://au.expasy.org/enzyme/' . $acc_no;
	}

        ## Did it work after a final massage?
        if( $db_hash{$database} ){
           $core->kvetch("second fallback hit: " . $db_hash{$database});
	}else{
           $core->kvetch("nothing here: undef");
	}

	return $db_hash{$database} || undef;
}

=head2 get_ref_url

parameters: database_abbreviation, acc_no

returns: url to the entry

This gets a link to evidence for gene product associations.
This is to a reference database, which in some organizations
is separate from the main database (i.e. SGD).

Also, it does a little munging of IDs that don't come in right.

=cut

sub get_ref_url {
	my $self = shift;
	return get_ref_url_fn(@_);
}

sub get_ref_url_fn {
	my $database = shift;
	my $acc_no = shift;

	$database = lc($database);

#	if ($database eq "sgd") {
#		$acc_no =~ s/\|.*$//;
#	} els
#		elsif (lc($database) eq "dros cdna" || $database eq "image") {
#		my $u_env = get_environment_param('ref_url');
#		$url = $u_env ? $u_env.$acc_no :
#			"http://toy.lbl.gov:8888/cgi-bin/ex/exgo_report.pl?image_dbxref=$acc_no";
#		return $url;
#	}

  my %db_hash = 
(	ddb=>"http://dictybase.org/db/cgi-bin/dictyBase/reference/reference.pl?refNo=$acc_no",
	gr=>"http://www.gramene.org/perl/pub_search?ref_id=$acc_no",
	rgd=>"http://rgd.mcw.edu/tools/references/references_view.cgi?id=$acc_no",
	sgd=>"http://db.yeastgenome.org/cgi-bin/SGD/reference/reference.pl?refNo=$acc_no",
#	http=>"http:$acc_no",
#	mgi=>"http://www.informatics.jax.org/searches/accession_report.cgi?id=$acc_no",
#	sgd_ref=>"http://db.yeastgenome.org/cgi-bin/reference/reference.pl?dbid=$acc_no",
#	swall=>"http://ca.expasy.org/cgi-bin/sprot-search-de?S=1&amp;T=1&amp;SEARCH=$acc_no",
#	tc=>"http://www.tcdb.org/tcdb/index.php?tc=$acc_no",
#	tigr_egad=>"http://www.tigr.org/tigr-scripts/CMR2/ht_report.spl?prot_id=$acc_no",
#	tigr_ref=>"http://www.tigr.org/tdb/GO_REF/GO_REF.shtml",
	);
	
	return $db_hash{$database} || return get_url_fn($database, $acc_no);
}

sub get_db_url {
	my $self = shift;
	return get_db_url_fn(@_);
}

sub get_db_url_fn {
	my $db = lc(shift);

	my %db_hash = (
	agbase => "http://www.agbase.msstate.edu/",
	cgd => 'http://www.candidagenome.org/',
	dictybase => 'http://dictybase.org/',
	fb => 'http://flybase.bio.indiana.edu/',
	flybase => 'http://flybase.bio.indiana.edu/',
	gdb => "http://www.gdb.org/",
	genedb_lmajor => 'http://www.genedb.org/',
	genedb_pfalciparum => 'http://www.genedb.org/',
	genedb_spombe => 'http://www.genedb.org/',
	genedb_tbrucei => 'http://www.genedb.org/',
	gr => 'http://www.gramene.org/',
	hgnc => 'http://www.genenames.org/',
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
	return get_human_name_fn(@_);
}

sub get_human_name_fn {
	my $abbr = shift;
  
	my $human_name = {
		cgen => 'Compugen',
		ddb => 'dictyBase',
		ec => 'NiceZyme',
		egad => 'EGAD',
		ensembl => 'Ensembl',
		embl => 'EMBL',
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
		term => 'terms',
		gp => 'genes or proteins',
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
	return get_full_name_fn(@_);
}

sub get_full_name_fn {
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
		EXP => 'Inferred from Experiment',
		ISO => 'Inferred from Sequence Orthology',
		ISA => 'Inferred from Sequence Alignment',
		ISM => 'Inferred from Sequence Model',
		NR => 'Not Recorded',
		all => 'All',
		aca => 'All Curator Approved',
		ca => 'All Curator Approved'
	);
	return $acronyms{$acronym} || $acronym;
}



sub get_GO_doc_url {
	my $self = shift;
	return get_GO_doc_url_fn(@_);
}

sub get_GO_doc_url_fn {
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

=head2 case_replace

parameters: gpxref

returns: case-corrected gpxref

=cut

sub case_replace {
	my $xref = shift;
	my ($db, $acc) = split /[:\|]/, $xref, 2;
	
	my %case_corrected = (
		cgd => "CGD",
		dictybase => "dictyBase",
		fb => "FB",
		genedb_lmajor => "GeneDB_Lmajor",
		genedb_pfalciparum => "GeneDB_Pfalciparum",
		genedb_spombe => "GeneDB_Spombe",
		genedb_tbrucei => "GeneDB_Tbrucei",
		gr => "GR",
		gr_protein => 'GR_protein',
		mgi => "MGI",
		pseudocap => "PseudoCAP",
		rgd => "RGD",
		sgd => "SGD",
		tair => "TAIR",
		tigr_cmr => "TIGR_CMR",
		tigr_tba1 => "TIGR_Tba1",
		uniprot => "UniProt",
		uniprotkb => "UniProtKB",
		wb => "WB",
		zfin => "ZFIN");
	
	$db = $case_corrected{ lc($db) } || $db;
	
	return "$db:$acc";
}

sub get_field_name {
	my $self = shift;
	return get_field_name_fn(@_);
}
	
sub get_field_name_fn {
	my $field = shift;
	my %fields = (
		all =>'all fields',
		comment =>'comment',
		dbxref =>'database cross-references',
		xref =>'database cross-references',
		definition =>'definition',
		full_name => 'full name(s)',
		name =>'term name',
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
		acc => 'term accession',
	);
	return $fields{$field} || undef;
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
	$comment =~ s/(GO:\d+)/<a href=\"term_details?term=$1&amp;session_id=$session_id\">$1<\/a>/g;
	return $comment;
}

sub markup_search_comment {
	my $self = shift;
	my $session_id = shift;
	my $comment = shift || return;
	$comment =~ s/GO\\:/GO:/g;
	$comment =~ s/.*?(To update annotations.*)/$1/i;
	if ($comment =~ /GO:\d{7}/)
	{	$comment =~ s/(GO:\d+)/<a href=\"term_details?term=$1&amp;session_id=$session_id\">$1<\/a>/g;
		return $comment;
	}
	return;
}

1;
