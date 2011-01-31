=head1 AmiGO::ReferenceGenome

A wrapper to help access Reference Genome information inside the
database while using older parts in AmiGO.

To be clear, eventually, we will not need this...

=cut

use utf8;
use strict;
#use WWW::Mechanize;
#use XML::XPath;

package AmiGO::ReferenceGenome;

use base ("AmiGO");
use GOBO::DBIC::GODBModel::Schema;
use GOBO::DBIC::GODBModel::Query;
use GOBO::DBIC::GODBModel::Graph;


=item new

#

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  $self->{SCHEMA} = GOBO::DBIC::GODBModel::Schema->connect($self->db_connector());
  $self->{RESULT} = [];

  $self->{GPH_QUERY} =
    GOBO::DBIC::GODBModel::Query->new({type=>'gene_product_homolset'});

  $self->{GRAPH} = GOBO::DBIC::GODBModel::Graph->new();

  bless $self, $class;
  return $self;
}


=item find_refgen_info

Usage: $rg->find_refgen_info({gene_product => 'SGD:S000004199'})
Args: takes a Unique GP ID
Returns: returns array ref of complicated objects or undef

TODO: make this able to take multiple args as a array ref [] in the
gene_product hash.

=cut
sub find_refgen_info {

  ##
  my $self = shift;
  my $arg_hash = shift || {};
  my $results = [];

  my $dbname = undef;
  my $key = undef;
  if( $arg_hash->{gene_product} ){
    my $str = $arg_hash->{gene_product};
    ($dbname, $key) = $self->split_gene_product_acc($str);
  }
  return undef if ! defined $dbname || ! defined $key;

  ##
  my $dbx_rs =
    $self->{SCHEMA}->resultset('DBXRef')->search({ xref_dbname => $dbname,
						   xref_key    => $key},
						 { prefetch =>
						   {'gene_product' =>
						    {'gene_product_homolset' =>
						     'homolset'}}});

  ## A gp may belong to more than one homolset, let's be careful
  ## here. Also, we're just assuming one results without checking. May
  ## be bad if db is in a bad way...
  my @all_dbx = $dbx_rs->all;
  foreach my $dbx (@all_dbx){
    if( $dbx &&
	$dbx->gene_product &&
	$dbx->gene_product->gene_product_homolset ){

      my @all_gphs = $dbx->gene_product->gene_product_homolset;
      foreach my $a_gphs (@all_gphs){

	my $hs = $a_gphs->homolset;

	## TODO: look at all of the species in the homolset.
	my $all_gphs_for_hs =
	  $self->{GPH_QUERY}->get_all_results({homolset_id => $hs->id});

	## For all of the gps in the in the homolset, look at all of the
	## other species...
	my $ncbi_taxa_ids = {};
	foreach my $gph (@$all_gphs_for_hs){
	  ## Has NCBI taxa id...
	  if( defined $gph->gene_product &&
	      defined $gph->gene_product->species &&
	      defined $gph->gene_product->species->ncbi_taxa_id ){
	    ## Has well defined term...
	    my @all_a = $gph->gene_product->association->all;
	    foreach my $a ( @all_a ){
	      ## If not a root...
	      my $tacc = $a->term->acc;
	      my $tid = $gph->gene_product->species->ncbi_taxa_id;
	      if( ! $self->{GRAPH}->is_root_p($tacc) ){
		## Add is to the seen species.
		$ncbi_taxa_ids->{$tid} = 1;
		#$self->kvetch("_others_" . $tid);
	      }
	    }
	  }
	}


	## Create an info struct to return...
	my @others = keys %$ncbi_taxa_ids;
	push @$results,
	  {
	   id => $hs->id,
	   symbol => $hs->symbol,
	   xref => $hs->dbxref->xref_dbname,
	   key => $hs->dbxref->xref_key,
	   main_link => $self->get_interlink({mode=>'homolset_summary',
					      arg=>{jump=>$hs->id}}),
	   detail_link => $self->get_interlink({mode=>'homolset_annotation',
						arg=>{set=>$hs->id}}),
	   other_species => \@others,
	  };
      }
    }
  }

  my $final = undef;
  $final = $results if scalar(@$results) > 0;
  return $final;
}



1;
