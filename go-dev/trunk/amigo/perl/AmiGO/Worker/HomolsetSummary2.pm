=head1 AmiGO::Worker::HomolsetSummary2

Generates the Ref Genome summary from the database.

Basically performs homolset_annotation's operations on every homolset.

=cut

use utf8;
#use diagnostics;
use strict;

package AmiGO::Worker::HomolsetSummary2;

use base ("AmiGO");

use GOBO::DBIC::GODBModel::Schema;
use GOBO::DBIC::GODBModel::Query;
#use AmiGO::Worker::HomolsetEvidence;
use AmiGO::Worker::GPInformation::HomolsetInformation;

my $nl = "\n";


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  # my $session_id = shift;
  my $self  = $class->SUPER::new();

  $self->{HS_QUERY}= GOBO::DBIC::GODBModel::Query->new({type=>'homolset'});
  $self->{AID}= AmiGO::Aid::ReferenceGenome->new();
  #$self->{ASSOC_QUERY}= GOBO::DBIC::GODBModel::Query->new({type=>'homolset'});
  $self->{SCHEMA} = GOBO::DBIC::GODBModel::Schema->connect($self->db_connector());
  # #   $self->{SCHEMA} =
  # #     GOBO::DBIC::GODBModel::Schema->connect($self->db_connector(),
  # # 				  { cursor_class =>
  # # 				    'DBIx::Class::Cursor::Cached' });
  # #   $self->{SCHEMA}->default_resultset_attributes({
  # # 				 cache_object =>
  # # #		 Cache::FileCache->new({ namespace => 'SchemaClass' })});
  # # 	 Cache::Memcached->new({'servers' => ["127.0.0.1:11211"]})});;

  #$self->{INFO_BY_SPECIES} = {};
  #$self->{INFO_BY_HOMOLSET} = {};
  #$self->{INFO_BY_GP} = {};
  $self->{INFO_BY_ID} = {};

  # $self->{SESSION_ID} = $session_id || '';

  bless $self, $class;
  return $self;
}


=item get_summary_data

Create a summary dataset.

=cut
sub get_summary_data {

  my $self = shift;


  ## For every homolset...
  my $complete_results = {};
  my $all_hs = $self->{HS_QUERY}->get_all_results();
  $self->kvetch("Snagged a total of " . scalar(@$all_hs) . " homolsets.");
  foreach my $hs (@$all_hs){

    my $hs_id = $hs->id;

    ## DEBUG: Limit output. This if is here until I'm sure.
#     if( $hs_id < 11 ||
#      	$hs_id == 200 ||
#      	$hs_id == 307 ||
#      	$hs_id == 350 ){

      my $hs_symbol = $hs->symbol;
      $self->kvetch("HSS: Working on $hs_id ($hs_symbol)...");

      my $rg_info =
	AmiGO::Worker::GPInformation::HomolsetInformation->new({skip_roots=>1,
								no_graph=>1});

      $rg_info->calculate($hs_id);

      ##
      #$rg_info->remove_association_duplicates();
      my $assocs = $rg_info->get_matrix();
      my $seen_terms = $rg_info->get_terms();
      my $direct_terms = $rg_info->get_direct_terms();
      my $seen_species = $rg_info->get_species();
      my $seen_ontologies = $rg_info->get_ontologies();
      my $seen_species_by_ontology = $rg_info->get_species_by_ontology();
      my $rg_symbol =  $rg_info->get_symbol();

      ## Interesting information to return later on.
      $self->{INFO_BY_ID}{$hs_id} =
	{
	 symbol => $rg_symbol,
	 detail_link => $self->get_interlink({mode=>'homolset_annotation',
					      arg=>{set=>$hs_id}}),
	 svg_link => $self->get_interlink({mode=>'homolset_graph',
					   arg=>{set=>$hs_id, format=>'svg'}}),
	 png_link => $self->get_interlink({mode=>'homolset_graph',
					   arg=>{set=>$hs_id, format=>'png'}}),
	 browser_link => $self->get_interlink({mode=>'browse',
					       arg=>{action=>'set-tree',
						     terms=>$direct_terms}})
	};

      ## Walk the matrix.
      ## Don't care about the acc.

      foreach my $acc (keys %$assocs){

	#my $t = $nodes->{$acc};

	foreach my $ncbi_taxa_id (keys %{$assocs->{$acc}}){
	  foreach my $a_id (keys %{$assocs->{$acc}{$ncbi_taxa_id}}){

	    ##
	    my $assoc_data = $assocs->{$acc}{$ncbi_taxa_id}{$a_id};
	    my $gp_acc = $assoc_data->{gene_product_id};
	    my $gp_sym = $assoc_data->{gene_product_symbol};
	    my $direct_p = $assoc_data->{direct_p};
	    my $spec_name =
	      $self->{AID}->taxid2readable({spec_id=>$ncbi_taxa_id,web_safe=>1});
	    my $spec_color = $self->{AID}->taxid2color($ncbi_taxa_id);

	    ##
	    my $has_exp_p = $assoc_data->{has_exp_p};
	    my $has_good_iss_p = $assoc_data->{has_good_iss_p};
	    my $has_odd_iss_p = $assoc_data->{has_odd_iss_p};
	    my $has_bad_iss_p = $assoc_data->{has_bad_iss_p};

	    $self->kvetch("\tOn GP: " . $gp_acc . ' (' . $spec_color. ')');

	    if( $direct_p ){

	      ## Make sure the the data structure is complete this far
	      ## down.
	      $complete_results->{$hs_id} = {}
		if ! defined $complete_results->{$hs_id};
	      $complete_results->{$hs_id}{$spec_name} = {}
		if ! defined $complete_results->{$hs_id}{$spec_name};
	      $complete_results->{$hs_id}{$spec_name} = {}
		if ! defined $complete_results->{$hs_id}{$spec_name};

	      ## If we haven't yet defined it, do so, otherwise just
	      ## increment the values.
	      my $unique = $spec_name . $gp_acc . $has_exp_p .
		$has_good_iss_p . $has_odd_iss_p . $has_bad_iss_p ;

	      if( ! defined $complete_results->{$hs_id}{$spec_name}{$unique} ){

		$complete_results->{$hs_id}{$spec_name}{$unique} =
		  {
		   gene_product_acc => $gp_acc,
		   gene_product_symbol => $gp_sym,
		   gene_product_link =>
		   $self->get_interlink({mode=>'gp-details',
					 arg=>{gp=>$gp_acc}}),
		   has_exp_p => $has_exp_p,
		   has_good_iss_p => $has_good_iss_p,
		   has_odd_iss_p => $has_odd_iss_p,
		   has_odd_iss_p => $has_odd_iss_p,
		   count => 0,
		  };
		$self->kvetch("Summary adding: $hs_id $spec_name ... " .
			      $gp_sym . ' ('. $unique . ')');
	      }else{

		## Just flip the additional bits...
		my $res = $complete_results->{$hs_id}{$spec_name}{$unique};
		$res->{has_exp_p} = $has_exp_p
		  if $has_exp_p;
		$res->{has_good_iss_p} = $has_good_iss_p
		  if $has_good_iss_p;
		$res->{has_odd_iss_p} = $has_odd_iss_p
		  if $has_odd_iss_p;
		$res->{has_bad_iss_p} = $has_bad_iss_p
		  if $has_bad_iss_p;
	      }
	    }
	  }
	}
      }
    #}
  }

  ## TODO: any final data transforms before return.

  ## TODO:
  ## {<hsid>:<species>:<symbol>:<code1>...<coden>, <count>}
  return $complete_results;
}


##
sub get_homolset_information {
  my $self = shift;
  return $self->{INFO_BY_ID};
}



1;
