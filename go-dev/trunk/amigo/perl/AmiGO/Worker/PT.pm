package AmiGO::Worker::PT;
use warnings;
use strict;
use Memoize;

# This should replace AmiGO::Worker::Phylotree some time in the
# future. Unless gold comes out first.

use Data::Dumper;

use GOBO::DBIC::GODBModel::Query;
use AmiGO::Aid::ReferenceGenome;
use AmiGO::JavaScript;

# non-OO functions

sub new{
    my $c = shift;
    my $s = scalar(@_) ? { @_ } : {};

    return bless $s, $c;
}

memoize('gobo');
sub gobo{
    return GOBO::DBIC::GODBModel::Query->new({type=>shift()});
}

memoize('core');
sub core{
    return AmiGO::JavaScript->new();
}

memoize('is_refg');
sub is_refg{
    my $ncbi_taxa_id = shift;
    my $aid = AmiGO::Aid::PantherDB->new($ncbi_taxa_id);
    return $aid->is_refg();
}


memoize('reference_genome');
sub reference_genome{
    return AmiGO::Aid::PantherDB->reference_genome();
}

# dbname must me set (probably PANTHER)

sub groups{
    my $s = shift;

    my $r = gobo('phylotree')->get_all_results
      ({ xref_dbname => $s->{dbname} },
      );


    return map {
	my $dbxref = $_->dbxref();
	__PACKAGE__->new
	  (
	   gobo   => $_,
	   dbname => $dbxref->xref_dbname(),
	   key    => $dbxref->xref_key(),
	  );
    } @$r;
}

# needs both dbname & key set

sub last_annotated{
    my $s = shift;

    if (not exists($s->{last_annotated})) {
	my $ass = $s->{gobo}->associations();

	$s->{last_annotated} = $ass->get_column('assocdate')->max();
	$s->{exp} = $ass->search
	  ({ code => core()->experimental_evidence_codes },
	   {
	    join => [ 'evidence' ],
	    select => ['association.gene_product_id' ],
	    distinct => 1,
	   })->count();
    }
    return $s->{last_annotated};
}

sub exp{
    my $s = shift;

    if (not exists($s->{exp})) {
	$s->last_annotated();
    }
    return $s->{exp};
}

sub pretty_last_annotated{
    my $la = shift()->last_annotated();
    if (not $la) {
	return 'never';
    } elsif ($la =~ m/(\d\d\d\d)(\d\d)(\d\d)/) {
	return "$1-$2-$3";
    } else {
	warn "Don't know what date format '$la' is.";
	return $la;
    }
}

sub number_of_refg_members{
    my $s = shift;

    if (not $s->{number_of_refg_members}) {
	$s->{number_of_members} = 0;
	$s->{number_of_refg_members} = 0;

	my $rs = $s->{gobo}->gene_products({}, { prefetch => 'species' });
	while (my $gp = $rs->next()) {
	    my $species = $gp->species();
	    my $ncbi_taxa_id = $species->ncbi_taxa_id();

	    $s->{by_species}->{$ncbi_taxa_id}++;
	    $s->{number_of_members}++;
	    if (is_refg($ncbi_taxa_id)) {
		$s->{number_of_refg_members}++;
		#$s->{by_species}->{$ncbi_taxa_id}++;
	    }
	}
    }
    return $s->{number_of_refg_members};
}

sub number_of_members{
    my $s = shift;

    if (not exists($s->{number_of_members})) {
	#$s->{number_of_members} = $s->{gobo}->gene_product_phylotree()->count();
	$s->number_of_refg_members();
    }
    return $s->{number_of_members};
}

