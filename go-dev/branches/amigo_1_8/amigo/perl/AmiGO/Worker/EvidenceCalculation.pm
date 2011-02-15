=head1 AmiGO::Worker::EvidenceCalculation



=cut

use utf8;
#use diagnostics;
use strict;

package AmiGO::Worker::EvidenceCalculation;

use base ("AmiGO");

use GOBO::DBIC::GODBModel::Schema;
use GOBO::DBIC::GODBModel::Query;


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  $self->{QUERY}= GOBO::DBIC::GODBModel::Query->new({type=>'association_deep'});
  $self->{SCHEMA} = GOBO::DBIC::GODBModel::Schema->connect($self->db_connector());

  $self->{EXP_EV_CODES_ARRAY} = $self->experimental_evidence_codes();
  $self->{EXP_EV_CODES_HASH} = {};
  foreach ( @{$self->{EXP_EV_CODES_ARRAY}} ){
    $self->{EXP_EV_CODES_HASH}{$_} = 1;
  }

  $self->{HAS_EXP_COUNT} = {};
  $self->{HAS_ISS_COUNT} = {};

  $self->{GOOD_ISS_COUNT} = {};
  $self->{ODD_ISS_COUNT} = {};
  $self->{BAD_ISS_COUNT} = {};

  bless $self, $class;
  return $self;
}


=item generate_homolset_evidence

arg: h_set as integer
ret: nuttin'

=cut
sub generate_homolset_evidence {

  my $self = shift;
  my $set = shift || 0;

  ###
  ### Collect assocs that have EXP associations. Also collect the dbxrefs
  ### of the associated gene product.
  ###

  my @e_codes = @{$self->experimental_evidence_codes()};
  my $exp_results =
    $self->{QUERY}->get_all_results({-and=>[
					    {'homolset.id' => $set},
					    {'evidence.code'=>
					     {'=',
					      $self->{EXP_EV_CODES_ARRAY}}}
					   ]});

  ##
  foreach my $a (@$exp_results){

    my $aid = $a->id;

    my $agpid = $a->gene_product->dbxref->xref_dbname .':'.
      $a->gene_product->dbxref->xref_key;
    $self->kvetch("EXP gpid: " . $agpid . ' aid:' . $aid);

    $self->{HAS_EXP_COUNT}{$aid} = 0
      if ! defined $self->{HAS_EXP_COUNT}{$aid};
    $self->{HAS_EXP_COUNT}{$aid}++;
  }

  ##
  ## Collect assocs that have only "good" ISS associations. This is
  ## going to be a little more intensive because there is no way to
  ## directly link the dbxref table with GO IDs back to the the actual
  ## term, so we have to take a break and do another query.
  ##

  my $iss_only_results =
    $self->{QUERY}->get_all_results({-and=>[
					    {'homolset.id' => $set},
					    {'evidence.code'=> {'=', 'ISS'}},
					    {'evidence.code'=>
					     {'!=', $self->evidence_codes_sans(['ISS'])}},
					    ## BUG: I don't seem to be
					    ## able to control it this
					    ## deep...
					    {'evidence_2.code'=>
					     {'=',
					      $self->{EXP_EV_CODES_ARRAY}}},
					    {'evidence_2.code'=>
					     {'!=', 'ND'}},
					   ]});

  ## For keeping track of which assocs have the good iss evidence.
  foreach my $a (@$iss_only_results){

    my $aid = $a->id;

    $self->{HAS_ISS_COUNT}{$aid} = 0
      if ! defined $self->{HAS_ISS_COUNT}{$aid};
    $self->{HAS_ISS_COUNT}{$aid}++;

    ## No increment needed for these yet.
    $self->{GOOD_ISS_COUNT}{$aid} = 0
      if ! defined $self->{GOOD_ISS_COUNT}{$aid};
    $self->{ODD_ISS_COUNT}{$aid} = 0
      if ! defined $self->{ODD_ISS_COUNT}{$aid};
    $self->{BAD_ISS_COUNT}{$aid} = 0
      if ! defined $self->{BAD_ISS_COUNT}{$aid};

    my @all_e = $a->evidence->all;
    foreach my $e (@all_e){

      ## BUG/TODO: I'm leaving this here until I figure out why
      ## sometimes constraints don't work...
      die "THIS SHOULD BE ISS: $!" if $e->code ne 'ISS';

      ## Does the "with" refer back to a term?
      my @edbx_all = $e->evidence_dbxref->all;

      if( @edbx_all ){
	foreach my $edbx (@edbx_all){

	  if( $edbx->dbxref ){

	    #$self->kvetch("equiv key: " . $edbx->dbxref->xref_dbname  . ':' .
	    #	  $edbx->dbxref->xref_key);

	    ## From here, we jump the boundry and work our way up the
	    ## other side...
	    my $gp = $edbx->dbxref->gene_product;
	    if( $gp ){

	      my @assoc_all = $gp->association->all;
	      foreach my $a_jump (@assoc_all){

		my @all_e = $a_jump->evidence->all;
		foreach my $e_jump (@all_e){

		  ## BUG: We have to do this if 'cause we seem not to
		  ## be able to constrain this (yet) in the SQL in
		  ## Query.pm
		  if( defined( $self->{EXP_EV_CODES_HASH}{$e_jump->code} ) ){

		    my $agpid = $a->gene_product->dbxref->xref_dbname .':'.
		      $a->gene_product->dbxref->xref_key;
		    $self->kvetch("OK one: " . $e_jump->code . ' - '.
				  $agpid . ' ' . $aid);

		    ## BUG? We should constrain this so it pops out of
		    ## the SQL. Having trouble with that...the query
		    ## constraints don't seem to be working down
		    ## here...
		    my $should_be_upper_acc = $a->term->acc;
		    my $should_be_lower_acc = $a_jump->term->acc;
		    $self->kvetch('___' . $should_be_upper_acc . ' ?>= ' .
				  $should_be_lower_acc);

		    ## Take a look at all of the ancestors. Only not
		    ## odd if we find the ancestor somewhere.
		    my @all_ans = $a_jump->term->ancestors;
		    my $has_correct_ancestor_p = 0;
		    foreach my $ans (@all_ans) {

		      if( $should_be_upper_acc eq $ans->object->acc ){
			$has_correct_ancestor_p = 1;
			$self->kvetch( "matched: " . $ans->object->acc .
				       ' eq ' .  $should_be_upper_acc );
			last;
		      }
		    }

		    ##
		    if( $has_correct_ancestor_p ){
		      $self->kvetch("ISS *not* odd.");
		      $self->{GOOD_ISS_COUNT}{$aid}++;
		    }else{
		      $self->kvetch("ISS <<is>> odd.");
		      $self->{ODD_ISS_COUNT}{$aid}++;
		    }

		  }else{
		    ## TODO/BUG: Is bad actually an interesting concept?
		    my $agpid = $a->gene_product->dbxref->xref_dbname .':'.
		      $a->gene_product->dbxref->xref_key;
		    $self->kvetch("WRONG one: " . $e_jump->code . ' - '.
				  $agpid . ' ' . $aid);

		    $self->{BAD_ISS_COUNT}{$aid}++;
		  }
		}
	      }
	    }
	  }
	}
      }
    }
    $self->kvetch("___ g " . $self->{GOOD_ISS_COUNT}{$aid} .
    		  ' o ' . $self->{ODD_ISS_COUNT}{$aid} .
    		  ' b ' . $self->{BAD_ISS_COUNT}{$aid});
  }
}


=item has_exp

arg: aid as string
ret: 1 or 0

=cut
sub has_exp {

  my $self = shift;
  my $aid = shift || '';
  my $bool = 0;
  $bool = 1
    if defined($self->{HAS_EXP_COUNT}{$aid}) &&
      $self->{HAS_EXP_COUNT}{$aid} > 0;
  return $bool;
}


=item has_good_iss

arg: aid as string
ret: 1 or 0

=cut
sub has_good_iss {

  my $self = shift;
  my $aid = shift || '';
  my $bool = 0;

  if( defined($self->{GOOD_ISS_COUNT}{$aid}) &&
      $self->{GOOD_ISS_COUNT}{$aid} != 0 ){
    $bool = 1
  }
  return $bool;
}


=item has_odd_iss

arg: aid as string
ret: 1 or 0

=cut
sub has_odd_iss {

  my $self = shift;
  my $aid = shift || '';
  my $bool = 0;

  if( defined($self->{ODD_ISS_COUNT}{$aid}) &&
      $self->{ODD_ISS_COUNT}{$aid} != 0 ){
    $bool = 1
  }
  return $bool;
}


=item has_bad_iss

arg: aid as string
ret: 1 or 0

=cut
sub has_bad_iss {

  my $self = shift;
  my $aid = shift || '';
  my $bool = 0;

  if( defined($self->{BAD_ISS_COUNT}{$aid}) &&
      $self->{BAD_ISS_COUNT}{$aid} != 0 ){
    $bool = 1
  }
  return $bool;
}



1;
