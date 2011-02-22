=head1 AmiGO::Worker::GPInformation

test branch

TODO/BUG: A lot of the work here can be memoized out, but I'm
not sure if it's worth the effort at this point...

=cut


use utf8;
#use diagnostics;
use strict;

package AmiGO::Worker::GPInformation;

use base ('AmiGO::Worker');

use GOBO::DBIC::GODBModel::Query;
use Time::HiRes qw(gettimeofday tv_interval);


=item new

Constructor.

Calling with the ({no_graph=>[0|1]}) decides whether or not the
information will be gathered going up the graph; time
consuming. Defaults to 0.

Calling with the ({skip_roots=>[0|1]}) decides whether or not the
ontology roots are included in the outputs. Defaults to 0.

Even with skip_roots=>1,the arguement ({graph_roots=>[0|1]}) decides
whether or not the ontology roots are included in the outputs for
graphing purposes--all other information about direct annotations is
lost. Defaults to 0.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  my $arg_hash = shift || {};
  $self->{SKIP_ROOTS} = 0;
  if( defined $arg_hash->{skip_roots} ){
    $self->{SKIP_ROOTS} = $arg_hash->{skip_roots};
  }
  $self->{NO_GRAPH} = 0;
  if( defined $arg_hash->{no_graph} ){
    $self->{NO_GRAPH} = $arg_hash->{no_graph};
  }
  $self->{GRAPH_ROOTS} = 0;
  if( defined $arg_hash->{graph_roots} ){
    $self->{GRAPH_ROOTS} = $arg_hash->{graph_roots};
  }

  ## We'll borrow SUCCESS and ERROR_MESSAGE from AmiGO.

  ## Start.
  my $time = [gettimeofday];

  ##
  $self->kvetch('_0_time: ' . tv_interval($time));

  $self->{SEEN_SPECIES} = {};
  $self->{SEEN_SPECIES_BY_ONTOLOGY} = {}; # This is going to be 
  $self->{SEEN_ASSOCS}   = {}; # This is going to be a complicated struct.
  $self->{SEEN_GPS}   = {}; # This is going to be a complicated struct.
  $self->{SEEN_TERMS}   = {}; # This is going to be a complicated struct.
  $self->{SEEN_ROOTS}   = {};
  $self->{SEEN_DIRECT_TERMS}   = {};
  $self->{SEEN_GPS}     = {}; # This is going to be a complicated struct.
  $self->{SEEN_DIRECT_GPS_BY_ACC} = {};
  $self->{SEEN_DEPTH}     = {};
  $self->{SEEN_COUNT}     = {};
  $self->{CALC_NODES}     = {};
  $self->{CALC_EDGES}     = {};

  bless $self, $class;
  return $self;
}


=item generate_information

foo

=cut
sub generate_information {

  my $self = shift;
  my $gp_set = shift || [];

  ## Caching variables.
  my @ret_set = ();
  my $seed_list = [];
  my %taxa_ids = ();
  my %acc2assocs_direct = ();
  my $nodes = {};

  ## Start.
  my $time = [gettimeofday];

  ## Go through all of the gps...
  foreach my $gp (@$gp_set){

    ## Go through all of the associations...
    my $rs_a = $gp->association;
    my @all_a = $rs_a->all;
    foreach my $a ( @all_a ){

      ## Add term to the seed list and node hash.
      push @{$seed_list}, $a->term;
      $nodes->{$a->term->acc} = $a->term;

#       $self->{SEEN_DIRECT_GPS}{$a->term->acc} = {}
# 	if ! defined $self->{SEEN_DIRECT_GPS_BY_ACC}{$a->term->acc};
#       $self->{SEEN_DIRECT_GPS_BY_ACC}{$a->term->acc}{}

      ## Go through all of the evidence...
      my $e_rs = $a->evidence;
      my @all_e = $e_rs->all;
      foreach my $e (@all_e){

	$self->kvetch("_direct_" .
		      " gp: " . $gp->symbol .
		      " a: " . $a->id .
		      " t: " . $a->term->acc .
		      " e: " . $e->code .
		      " (" . $gp->species->ncbi_taxa_id .
		      ") " . $gp->dbxref->xref_dbname.':'.$gp->dbxref->xref_key
		     );

	## Tie the term to an association via assoc id. Direct.
	$acc2assocs_direct{$a->term->acc} = {}
	  if ! defined($acc2assocs_direct{$a->term->acc});
	$acc2assocs_direct{$a->term->acc}{$a->id} = $a;
      }
    }
  }

  $self->kvetch('_1_time: ' . tv_interval($time));

  ## Start.
  $time = [gettimeofday];

  ## Do we really want to do the expensive climbing step?
  my %all_assocs_for_acc = {};
  my $edges = {};
  my $tc_desc_struct = {};
  my $tc_anc_struct = {};
  my $tc_depth_struct = {};
  if( ! $self->{NO_GRAPH} ){

    ## Get graph information all of the way up. $nodes will be reset
    ## from climb with all of the new values.
    ($nodes, $edges, $tc_desc_struct, $tc_anc_struct, $tc_depth_struct) =
      $self->{GRAPH}->climb($seed_list);

    $self->{CALC_NODES} = $nodes;
    $self->{CALC_EDGES} = $edges;
    $self->{SEEN_DEPTH} = $tc_depth_struct;

    $self->kvetch("Node count: " . scalar(keys %$nodes));
    $self->kvetch("Edge count: " . scalar(keys %$edges));

    ###
    ### Connect all of the indirect associations using the transitive
    ### closure structure from the climber.
    ###

    $self->kvetch('_2_time: ' . tv_interval($time));

    ## Start.
    $time = [gettimeofday];

    ###
    ### Go through our nodes (all direct) and pass them up the graph
    ### using the information from the climber..
    ###

    foreach my $acc (keys %{$tc_desc_struct}){

      ## If, Skip from entry if it is a root (indirect info only).
      ## Elsif, skip from entry if it is a root (no information).
      if( $self->{GRAPH_ROOTS} &&
	  $self->{SKIP_ROOTS} &&
	  $self->{GRAPH}->is_root_p($acc) ){

	$self->kvetch('Info skipped root: ' . $acc);

	$all_assocs_for_acc{$acc} = {} if ! defined $all_assocs_for_acc{$acc};

	## Collect all of the indirect associations, excepting the
	## roots.
	foreach my $sub_acc (keys %{$tc_desc_struct->{$acc}}){
	  if( defined $acc2assocs_direct{$sub_acc} &&
	      $sub_acc ne $acc ){
	    foreach my $a_id (keys %{$acc2assocs_direct{$sub_acc}}){
	      $all_assocs_for_acc{$acc}{$a_id} =
		$acc2assocs_direct{$sub_acc}{$a_id};
	    }
	  }
	}

      }elsif( $self->{SKIP_ROOTS} &&
	      $self->{GRAPH}->is_root_p($acc) ){

	$self->kvetch('Completely skipped root: ' . $acc);

      }else{

	$all_assocs_for_acc{$acc} = {} if ! defined $all_assocs_for_acc{$acc};

	## Collect all of the direct associations.
	if( defined $acc2assocs_direct{$acc} ){
	  foreach my $a_id (keys %{$acc2assocs_direct{$acc}}){
	    $all_assocs_for_acc{$acc}{$a_id} =
	      $acc2assocs_direct{$acc}{$a_id};
	  }
	}

	## Collect all of the indirect associations.
	foreach my $sub_acc (keys %{$tc_desc_struct->{$acc}}){
	  if( defined $acc2assocs_direct{$sub_acc} ){
	    foreach my $a_id (keys %{$acc2assocs_direct{$sub_acc}}){
	      $all_assocs_for_acc{$acc}{$a_id} =
		$acc2assocs_direct{$sub_acc}{$a_id};
	    }
	  }
	}
      }
    }

  }else{

    ## The directs become all...
    %all_assocs_for_acc = %acc2assocs_direct;

    ## If requested, skim-off the roots.
    if( $self->{SKIP_ROOTS} ){ #&& ! $self->{GRAPH_ROOTS} ){
      foreach my $acc (keys %all_assocs_for_acc){
	if( $self->{GRAPH}->is_root_p($acc) ){
	  delete $acc2assocs_direct{$acc};
	  $self->kvetch('Skipped root: ' . $acc);
	}
      }
    }
  }

  ## Now, for each association with each term, generate consumable
  ## data...
  foreach my $acc (keys %all_assocs_for_acc){

    ## Snag the count while we have the chance here.
    $self->{SEEN_COUNT}{$acc} = scalar(keys %{$all_assocs_for_acc{$acc}});
    $self->kvetch('Total assoc count for ' . $acc . ' is ' .
		  $self->{SEEN_COUNT}{$acc});

    my $term = $nodes->{$acc};

    ##
    foreach my $a_id (keys %{$all_assocs_for_acc{$acc}}){

      ## Get assocs back.
      my $a = $all_assocs_for_acc{$acc}{$a_id};

      #
      #$self->kvetch(" a id: " . $a_id . ' (' .
      #	    scalar(keys %{$acc2assocs_direct{$sub}}) . ')');

      my $gp = $a->gene_product;
      my $ncbi_taxa_id = $gp->species->ncbi_taxa_id;

      my $gpid = $gp->dbxref->xref_dbname . ':' . $gp->dbxref->xref_key;

      ## Do direct detection.
      my $direct_p = 0;
      if( defined $acc2assocs_direct{$acc} &&
	  defined $acc2assocs_direct{$acc}{$a_id} ){
	$direct_p = 1;
      }

      $self->{SEEN_ASSOCS}{$acc} = {}
	if ! defined($self->{SEEN_ASSOCS}{$acc});
      $self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id} = {}
	if ! defined($self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id});

      ## If we already have it, don't bother adding it again.
      if( ! defined($self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}{$a_id}) ){

	$self->kvetch("   _indirect_" .
		      " taxa: " . $ncbi_taxa_id .
		      " acc: " . $term->acc .
		      " aid: " . $a_id .
		      " sym: " . $gp->symbol);
	$self->kvetch("   \t\t" . "direct: " . $direct_p);
	$self->kvetch("   \t\t" . $term->term_type);
	$self->kvetch("   \t\t" . $term->name);

	## Generate the assoc lookup table.
	$self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}{$a_id} =
	  {
	   # Assoc
	   direct_p => $direct_p,

	   # GP
	   gene_product_id => $gpid,
	   gene_product_symbol => $gp->symbol,
 	   gene_product_link =>
	   $self->get_interlink({mode=>'gp-details',
				 arg=>{db=>$gp->dbxref->xref_dbname,
				       acc=>$gp->dbxref->xref_key}}),
	  };
      }

      ## Take note of the species, term, and ontology.
      if( ! defined $self->{SEEN_SPECIES}{$ncbi_taxa_id} ){
	$self->{SEEN_SPECIES}{$ncbi_taxa_id} = 1; }
      if( ! defined $self->{SEEN_TERMS}{$acc} ){
	$self->{SEEN_TERMS}{$acc} = $term; }
      if( ! defined $self->{SEEN_DIRECT_TERMS}{$acc} && $direct_p == 1 ){
	$self->{SEEN_DIRECT_TERMS}{$acc} = $term; }
      if( ! defined $self->{SEEN_ONTOLOGIES}{$term->term_type} ){
	$self->{SEEN_ONTOLOGIES}{$term->term_type} = 1; }

      ## TODO: remove this bit in favor of onto bit...
      $self->{SEEN_SPECIES_BY_ONTOLOGY}{$term->term_type} = {}
	if ! defined $self->{SEEN_SPECIES_BY_ONTOLOGY}{$term->term_type};
      $self->{SEEN_SPECIES_BY_ONTOLOGY}{$term->term_type}{$ncbi_taxa_id} = 1
	if ! defined $self->{SEEN_SPECIES_BY_ONTOLOGY}{$term->term_type}{$ncbi_taxa_id};

      ## Now do more or less the same thing for GPs.
      $self->{SEEN_GPS}{$acc} = {}
	if ! defined($self->{SEEN_GPS}{$acc});
      $self->{SEEN_GPS}{$acc}{$ncbi_taxa_id} = {}
	if ! defined($self->{SEEN_GPS}{$acc}{$ncbi_taxa_id});

      ## If we already have it, don't bother adding it again.
      if( ! defined($self->{SEEN_GPS}{$acc}{$ncbi_taxa_id}{$gpid}) ){

	$self->{SEEN_GPS}{$acc}{$ncbi_taxa_id}{$gpid} = $gp;
# 	  {
# 	   ## Static.
# 	   gene_product_id => $gp_acc,
# 	   gene_product_symbol => $gp->symbol,
# 	   gene_product_link =>
# 	   $self->get_interlink({mode=>'gp-details',
# 				 arg=>{db=>$gp->dbxref->xref_dbname,
# 				       acc=>$gp->dbxref->xref_key}}),
# 	  };
      }
    }
  }

  my $retval = 1;
  return $retval;
}


=item get_matrix

about complicated structure...

=cut
sub get_matrix {
  my $self = shift;
  return $self->{SEEN_ASSOCS};
}


=item get_terms

Returns a hashref of seen terms.

=cut
sub get_terms {
  my $self = shift;
  return $self->{SEEN_TERMS};
}


=item get_direct_terms

Returns a hashref of seen seed terms.

=cut
sub get_direct_terms {
  my $self = shift;
  return $self->{SEEN_DIRECT_TERMS};
}


=item get_gene_products

Returns a hashref of seen gene products.

=cut
sub get_gene_products {
  my $self = shift;
  return $self->{SEEN_GPS};
}


=item get_species_by_ontology

Returns a hashref of seen species keyed by seen ontology.

=cut
sub get_species_by_ontology {

  my $self = shift;
  return $self->{SEEN_SPECIES_BY_ONTOLOGY};
}


=item get_ontologies

Returns a hashref of seen ontologies.

=cut
sub get_ontologies {

  my $self = shift;
  return $self->{SEEN_ONTOLOGIES};
}


=item get_species

Returns a hashref of seen species.

=cut
sub get_species {

  my $self = shift;
  return $self->{SEEN_SPECIES};
}

###
### The next group come from the fact that climb is run in this object
### and it is expensive enough that it would be silly to recalculate
### it for the sake of separation.
### These values are not useful if the function was called with no_graph=>1
### TODO: turn this into a head2 section...
###

=item get_nodes

Returns a hashref of seen nodes.
Useful for ancestor graph building.

=cut
sub get_nodes {
  my $self = shift;
  return $self->{CALC_NODES};
}


=item get_edges

Returns a hashref of seen edges.
Useful for ancestor graph building.

=cut
sub get_edges {
  my $self = shift;
  return $self->{CALC_EDGES};
}


=item get_term_count

Returns the depth of the term in reference to the implicit graph.

=cut
sub get_term_count {
  my $self = shift;
  my $acc = shift || '';
  my $retval = 0;
  if( defined $self->{SEEN_COUNT}{$acc} ){
    $retval = $self->{SEEN_COUNT}{$acc};
  }
  return $retval;
}


=item get_term_depth

Returns the depth of the term in reference to the implicit graph.

=cut
sub get_term_depth {
  my $self = shift;
  my $acc = shift || '';
  my $retval = -1;
  if( defined $self->{SEEN_DEPTH}{$acc} ){
    $retval = $self->{SEEN_DEPTH}{$acc};
  }
  return $retval;
}



1;
