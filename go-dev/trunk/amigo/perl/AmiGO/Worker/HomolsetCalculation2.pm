=head1 AmiGO::Worker::HomolsetCalculation2

test branch

TODO/BUG: A lot of the work here can be memoized out, but I'm
not sure if it's worth the effort at this point...

=cut


use utf8;
#use diagnostics;
use strict;

package AmiGO::Worker::HomolsetCalculation2;

use base ("AmiGO::Worker");

#use GOBO::DBIC::GODBModel::Schema;
use GOBO::DBIC::GODBModel::Query;
#use GOBO::DBIC::GODBModel::Graph;
use AmiGO::Worker::EvidenceCalculation;
use Time::HiRes qw(gettimeofday tv_interval);



=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  ## We'll borrow SUCCESS and ERROR_MESSAGE from AmiGO.

  ## Start.
  my $time = [gettimeofday];

  $self->{QUERY}= GOBO::DBIC::GODBModel::Query->new({type=>'gene_product_homolset'});

  ##
  $self->kvetch('_0_time: ' . tv_interval($time));

  $self->{SEEN_SPECIES} = {}; # This is going to be a complicated struct.
  $self->{SEEN_ASSOCS}   = {}; # This is going to be a complicated struct.
  $self->{SEEN_TERMS}   = {}; # This is going to be a complicated struct.
  $self->{SEEN_GPS}     = {}; # This is going to be a complicated struct.
  $self->{HOMOLSET_SYMBOL}   = undef;

  bless $self, $class;
  return $self;
}


=item calculate

foo

=cut
sub calculate {

  my $self = shift;
  my $set = shift || die "calculate requires an arg: $!";

  ###
  ### First, we are going to get the relevant EXP, ISS-only, and good
  ### ISS-only information for this homolset.
  ###

  my $ev_calc = AmiGO::Worker::EvidenceCalculation->new();

  $ev_calc->generate_homolset_evidence($set);
  #foreach (1...400){
  #$ev_calc->generate_homolset_evidence($_);
  #}
  #die "remove me";

  ###
  ### On to the old stuff...
  ###

  my @ret_set = ();
  my $seed_list = [];
  my %taxa_ids = ();
  my %acc2assocs_direct = ();

  ## Start.
  my $time = [gettimeofday];

  ## For all of the gphs in the in the homolset...go through all of
  ## the gps...
  my $all_gphs = $self->{QUERY}->get_all_results({homolset_id => $set});
  foreach my $gph (@$all_gphs){

    ## Grab the hs symbol if we haven't.
    if( ! defined($self->{HOMOLSET_SYMBOL}) ){
      my $hss = $gph->homolset->symbol;
      ## BUG: There seems to be some control characters slipping in.
      $hss =~ s/^\s+//; #remove leading spaces
      $hss =~ s/\s+$//; #remove trailing spaces
      $self->{HOMOLSET_SYMBOL} = $hss;
    }

    my $gp = $gph->gene_product;

    ## Go through all of the associations...
    my $rs_a = $gp->association;
    my @all_a = $rs_a->all;
    foreach my $a ( @all_a ){

      ## Add term to the seed list.
      push @{$seed_list}, $a->term;

      ## Go through all of the evidence...
      my $e_rs = $a->evidence;
      my @all_e = $e_rs->all;
      foreach my $e (@all_e){

	$self->kvetch("_direct_" .
		      " gph: " . $gph->homolset->symbol .
		      " gp: " . $gp->symbol .
		      " a: " . $a->id .
		      " t: " .$a->term->acc .
		      " e: " . $e->code .
		      " (" . $gp->species->ncbi_taxa_id .
		      ")");

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

  #   ## DEBUG:
  #   my %hhsh = ();
  #   foreach my $seed (@$seed_list){ $hhsh{ $seed->acc } = $seed; }
  #   my $hhsh = [];
  #   foreach my $seed_acc (keys %hhsh){
  #     push @$hhsh, $hhsh{ $seed_acc };
  #     $self->kvetch("Original seed: " . $seed_acc);
  #   }

  my($nodes, $edges, $tc_struct) = $self->{GRAPH}->climb($seed_list);

  $self->kvetch("Node count: " . scalar(keys %$nodes));
  $self->kvetch("Edge count: " . scalar(keys %$edges));

  ###
  ### Connect all of the indirect associations using the transitive
  ### closure structure from the climber.
  ###

  #   ## For each of the objects...
  #   my %acc2assocs_indirect = ();
  #   foreach my $obj (keys %$tc_struct){

  #     $self->kvetch("\tNode " . $obj . ' has the folling in graph desc:');

  #     $acc2assocs_indirect{$obj} = {}
  #       if ! defined($acc2assocs_indirect{$obj});

  #     ## Get all of the possible subjects...
  #     foreach my $sub (keys %{$tc_struct->{$obj}}){

  #       $self->kvetch("\t\t". $sub . ' (sub)');

  #       ## And add the assocs if they are extend in the original cache
  #       ## via assoc id.
  #       if( defined $acc2assocs_direct{$sub} ){
  # 	foreach my $assoc (keys %{$acc2assocs_direct{$sub}}){
  # 	  $acc2assocs_indirect{$} =
  # 			       {$a->id} = $a;
  # 			      }
  #       }
  #     }
  #   }

  $self->kvetch('_2_time: ' . tv_interval($time));

  ## Start.
  $time = [gettimeofday];

  # #foreach my $acc (keys %$nodes){
  ## Go through our nodes (all direct and indirect terms).
  my %all_assocs_for_acc = {};
  foreach my $acc (keys %{$tc_struct}){

    ## Skip from entry if it is a root (no information).
    if( ! $self->{GRAPH}->is_root_p($acc) ){

      $all_assocs_for_acc{$acc} = {} if ! defined $all_assocs_for_acc{$acc};

      ## Collect all of the direct associations.
      if( defined $acc2assocs_direct{$acc} ){
	foreach my $a_id (keys %{$acc2assocs_direct{$acc}}){
	  $all_assocs_for_acc{$acc}{$a_id} = $acc2assocs_direct{$acc}{$a_id};
	}
      }

      ## Collect all of the indirect associations.
      foreach my $sub_acc (keys %{$tc_struct->{$acc}}){
	if( defined $acc2assocs_direct{$sub_acc} ){
	  foreach my $a_id (keys %{$acc2assocs_direct{$sub_acc}}){
	    $all_assocs_for_acc{$acc}{$a_id} =
	      $acc2assocs_direct{$sub_acc}{$a_id};
	  }
	}
      }
    }
  }

  ## Now, for each association with each term, generate consumable
  ## data...
  foreach my $acc (keys %all_assocs_for_acc){

    my $term = $nodes->{$acc};

    ##
    foreach my $a_id (keys %{$all_assocs_for_acc{$acc}}){

      ## Get assocs back.
      my $a = $all_assocs_for_acc{$acc}{$a_id};

      # my $a = $acc2assocs_indirect{$acc}{$a_id};
      # $self->kvetch(" a id: " . $a_id . ' (' .
      #		    scalar(keys %{$acc2assocs_indirect{$acc}}) . ')');
      #
      #$self->kvetch(" a id: " . $a_id . ' (' .
      #	    scalar(keys %{$acc2assocs_direct{$sub}}) . ')');

      my $gp = $a->gene_product;
      #my $term = $a->term;
      my $ncbi_taxa_id = $gp->species->ncbi_taxa_id;

      my $gpid = $gp->dbxref->xref_dbname . ':' . $gp->dbxref->xref_key;

      ## Do ISS and EXP-only detection.
      my $has_exp_p = $ev_calc->has_exp($a_id);
      my $has_good_iss_p = $ev_calc->has_good_iss($a_id);
      my $has_odd_iss_p = $ev_calc->has_odd_iss($a_id);
      my $has_bad_iss_p = $ev_calc->has_bad_iss($a_id);

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
		      " acc: " . $term->acc .
		      " aid: " . $a_id .
		      " sym: " . $gp->symbol);
	$self->kvetch("   \t\t" .
		      " exp: " . $has_exp_p .
		      " good: " . $has_good_iss_p .
		      " odd: " . $has_odd_iss_p .
		      " bad: " . $has_bad_iss_p .
		      #" iss good: " . $iss_only_good_p .
		      #" iss good spec: " . $iss_only_good_odd_p .
		      #" iss bad: " . $iss_only_bad_p .
		      " dir: " . $direct_p);
	$self->kvetch("   \t\t" . $self->readable($term->term_type));
	$self->kvetch("   \t\t" . $term->name);
	#$self->kvetch("   \t" . $top_term->name);

	## Generate the assoc lookup table.
	$self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}{$a_id} =
	  {
	   # Assoc
	   #iss_only_good_p => $iss_only_good_p,
	   #iss_only_good_odd_p => $iss_only_good_odd_p,
	   #iss_only_bad_p => $iss_only_bad_p,
	   #iss_only_bad_p => 0,
	   has_exp_p => $has_exp_p,
	   has_good_iss_p => $has_good_iss_p,
	   has_odd_iss_p => $has_odd_iss_p,
	   direct_p => $direct_p,

	   # Term
	   acc => $term->acc,
	   name => $term->name,
	   ontology => $self->readable($term->term_type),
	   term_details_link => $self->get_interlink({mode=>'term_details',
						      arg=>{acc=>$acc}}),

	   # GP
	   gene_product_symbol => $gp->symbol,
 	   gene_product_link =>
	   $self->get_interlink({mode=>'gp_details',
				 arg=>{db=>$gp->dbxref->xref_dbname,
				       acc=>$gp->dbxref->xref_key}}),
	  };
      }

      ## TODO: remove this bit in favor of onto bit...
      ## Take note of the species and term.
      if( ! $self->{SEEN_SPECIES}{$ncbi_taxa_id} ){
	$self->{SEEN_SPECIES}{$ncbi_taxa_id} = 1; }
      if( ! $self->{SEEN_TERMS}{$acc} ){
	$self->{SEEN_TERMS}{$acc} = $term; }
    }
  }

  my $retval = 1;
  $retval = 0 if ! defined($self->{HOMOLSET_SYMBOL});
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


=item get_species

Returns a hashref of seen species.

=cut
sub get_species {

  my $self = shift;
  return $self->{SEEN_SPECIES};
}


=item get_symbol

Get the homolset symbol string.

=cut
sub get_symbol {
  my $self = shift;
  return $self->{HOMOLSET_SYMBOL};
}



1;
