=head1 AmiGO::Worker::HomolsetEvidence

NOTE/BUG: BAD ISS is largely ignored. In the future, maybe I should
just remove all traces of it.

=cut

use utf8;
#use diagnostics;
use strict;

package AmiGO::Worker::HomolsetEvidence;

use base ("AmiGO::Worker");

#use GOBO::DBIC::GODBModel::Schema;
use GOBO::DBIC::GODBModel::Query;

## These will be expanded out into COUNTs AND CODES.
my $EVIDENCE_GROUPS =
  ['EXP', 'GOOD_ISS', 'ODD_ISS', 'BAD_ISS'];


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  $self->{EXP_QUERY} =
    GOBO::DBIC::GODBModel::Query->new({type=>'association_deep'});
  $self->{ISS_QUERY} =
    #GOBO::DBIC::GODBModel::Query->new({type=>'association_very_deep'});
    GOBO::DBIC::GODBModel::Query->new({type=>'association_very_deep_DEBUG'});

  $self->{EXP_EV_CODES_ARRAY} = $self->experimental_evidence_codes();
  $self->{EXP_EV_CODES_HASH} = $self->experimental_evidence_hash();

  $self->{ISS_EV_CODES_ARRAY} = $self->iss_evidence_codes();
  $self->{ISS_EV_CODES_HASH} = $self->iss_evidence_hash();
  #foreach ( @{$self->{EXP_EV_CODES_ARRAY}} ){
  #  $self->{EXP_EV_CODES_HASH}{$_} = 1;
  #}

  $self->{HEV_INFO} = {};

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
  ### Collect assocs that have exp associations. Also collect the dbxrefs
  ### of the associated gene product.
  ###

  my $exp_results =
    $self->{EXP_QUERY}->get_all_results({-and=>[
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

    $self->kvetch("experimental gpid: " . $a->gene_product->symbol .
		  " " . $agpid . ' aid:' . $aid);

    $self->_increment_count_in_info($aid, 'EXP');

    ## TODO: Get the evidence types.
    my @all_e = $a->evidence->all;
    #my $check = 0;
    foreach my $e (@all_e){
      #$check++;
      #$self->kvetch( "_ev_" . $e->code . ' ' . $check);
      $self->kvetch( "_ev_" . $e->code);
      $self->_add_evcode_into_info($aid, 'EXP', $e->code);
    }
    #$self->kvetch( "\t_ev_" . $check);
    #die "nope" if $check == 0;
  }
  #$self->kvetch("Done collecting experimentals...",1);

  ###
  ### Look at all ISS-style associations, and decide whether they have
  ### experimental (good) backing or don't (odd).
  ###

  my $iss_only_results =
    $self->{ISS_QUERY}->get_all_results({-and=>[
						{'homolset.id' => $set},
						{'evidence.code'=>
						 #{'=', 'ISS'}},
						 {'=',
						  $self->{ISS_EV_CODES_ARRAY}}}#,
#						{'evidence.code'=>
#						 {'!=', $self->evidence_codes_sans($self->{ISS_EV_CODES_ARRAY})}}
					       ]});
## We want to consider all ISSs so we know when we have losers.
#						 {'!=', $self->evidence_codes_sans(['ISS'])}},
# 						## BUG: I don't seem to be
# 						## able to control it this
# 						## deep...
# 						{'evidence_2.code'=>
# 						 {'=',
# 						  $self->{EXP_EV_CODES_ARRAY}}},
# 						{'evidence_2.code'=>
# 						 {'!=', 'ND'}},
# 					       ]});

  ## For keeping track of which assocs have the good iss evidence.
  my $debug_counter = 0;
  foreach my $a (@$iss_only_results){

    my $aid = $a->id;

    $debug_counter++;
    my $foogle =
      $a->gene_product->dbxref->xref_dbname.':'.
	$a->gene_product->dbxref->xref_key;
    $self->kvetch("(" . $debug_counter . ") iss-type: " .
		  $a->gene_product->symbol .
		  ' gid: ' . $foogle);

    my @all_e = $a->evidence->all;

    ## First, make sure that there is *some evidence*...
    if( ! @all_e || scalar(@all_e) < 1 ){
      $self->kvetch("no evidence: " . $aid);
      $self->_increment_count_in_info($aid, 'ODD_ISS');
    }else{

      ## Look at all of the evidence for the association and see if
      ## there is anything we like and/or don't like.
      foreach my $e (@all_e){

	## BUG/TODO: I'm leaving this here until I figure out why
	## sometimes constraints don't work...may be related to the
	## joining bug above...
	die "EV CODE (". $e->code .") SHOULD BE ISS-type: $!"
	  #if $e->code ne 'ISS';
	  if ! defined $self->{ISS_EV_CODES_HASH}{$e->code};

	## Does the "with" refer back to a term?
	my @edbx_all = $e->evidence_dbxref->all;

	## All good ISS will have exp evidence on the other side, so
	## this will be necessary as well.
	if( ! @edbx_all || scalar(@edbx_all) < 1 ){
	  $self->kvetch("\tno evidence dbxref: " . $e->code . ' ' . $aid);
	  $self->_increment_count_in_info($aid, 'ODD_ISS');
	}else{

	  ## Take a look at all of the evidence crossrefs.
	  foreach my $edbx (@edbx_all){

	    ## Gotta have one to be good...
	    if( ! $edbx->dbxref ){
	      $self->kvetch("\tno dbxref from edbx: " . $aid);
	      $self->_increment_count_in_info($aid, 'ODD_ISS');
	    }else{

	      ## From here, we try to jump the boundry and work our
	      ## way up the other side. If we can't jump the boundry
	      ## directly (! $gp), we'll try to work our way through
	      ## seq_dbxref -> seq -> gp_seq, and on our way to gp
	      ## again...this is a hack...
	      my @found_gps = (); # we may find more than one if we go
                                  # into hack mode...
	      my $test_gp = $edbx->dbxref->gene_product;
	      if( $test_gp ){
		push @found_gps, $test_gp; # just the one (normal!)
	      }else{

		$self->kvetch("\teasy jump failed: " . $aid);

		#my $bridge_seq = $edbx->dbxref->seq_dbxref->seq;
		## HACK MODE (TODO): Above is the way it should be,
		## but there is some problem with how Uniprot is
		## naming itself (and maybe something with MGI as
		## well), so we are going to move into hack mode and
		## use seq's display_id as a key for the dbxref...
 		my $bridge_dbx = $edbx->dbxref;
		if( ! $bridge_dbx ){
		  $self->kvetch("\t\tbad dbx");
		}else{

		  $self->kvetch("\t\tok sdbx: " . $edbx);

		  ## Leap the gap and collect the gps--usually one,
		  ## but sometimes more...
		  my $bridge_seq = $bridge_dbx->seq_hack;
# 		  if( ! $bridge_seq && defined($bridge_dbx->seq_dbxref) ){
# 		    ## Hack didn't work, so try the non-hack method...
# 		      $bridge_seq = $bridge_dbx->seq_dbxref->seq;
# 		   }

		  if( ! $bridge_seq ){
		    ## I guess we can't get it to work...
		    $self->kvetch("\t\tbad seq");
		  }else{
		    $self->kvetch("\t\tok seq: " . $bridge_seq);
		    my $bridge_gpseq_rs = $bridge_seq->gene_product_seq;
		    my @bridge_gpseq_all = $bridge_gpseq_rs->all;
		    if( scalar(@bridge_gpseq_all) ){
		      @found_gps = map {$_->gene_product} @bridge_gpseq_all;
		      $self->kvetch("\thard jump seems to have worked: ".
				    scalar(@found_gps));
		    }else{
		      $self->kvetch("\tjump seems to have failed: " .
				    scalar(@bridge_gpseq_all));
		    }
		  }
		}
	      }

	      ## Odd if we found nothing.
	      if( ! scalar(@found_gps) ){
		$self->kvetch("\tno way to boundry jump: " . $aid);
		$self->_increment_count_in_info($aid, 'ODD_ISS');
	      }else{

		## Given the above hack, we may have more than one gp
		## to examine, so we loop. This shouldn't cause a
		## tallying problem because at the end we're just
		## looking for existance.
		my $gp_index = 0;
		foreach my $gp (@found_gps){

		  ## DEBUG: Which one are we on?
		  $gp_index++;
		  $self->kvetch("\tyes boundry jumped (" . $gp_index . '/' .
				scalar(@found_gps) . "): " . $aid);

		  my @assoc_all = $gp->association->all;

		  ## Better have associations.
		  if( ! @assoc_all || scalar(@assoc_all) < 1 ){
		    $self->kvetch("\t\tno associations after jump: " . $aid);
		    $self->_increment_count_in_info($aid, 'ODD_ISS');
		  }else{

		    foreach my $a_jump (@assoc_all){

		      my @all_e = $a_jump->evidence->all;

		      ## And (more) evidence...
		      if( ! @all_e || scalar(@all_e) < 1 ){
			$self->kvetch("\t\tno assoc ev after jump: " . $aid);
			$self->_increment_count_in_info($aid, 'ODD_ISS');
		      }else{

			foreach my $e_jump (@all_e){

			  ## BUG: We have to do this if 'cause we seem not to
			  ## be able to constrain this (yet) in the SQL in
			  ## Query.pm
			  if(defined($self->{EXP_EV_CODES_HASH}{$e_jump->code})){

			    my $agpid = $a->gene_product->dbxref->xref_dbname .
			      ':' . $a->gene_product->dbxref->xref_key;
			    $self->kvetch("\t\tOK one: " .$e_jump->code. ' - '.
					  $agpid . ' ' . $aid);

			    ## BUG? We should constrain this so it pops
			    ## out of the SQL. Having trouble with
			    ## that...the query constraints don't seem
			    ## to be working down here...
			    my $should_be_upper_acc = $a->term->acc;
			    my $should_be_lower_acc = $a_jump->term->acc;
			    $self->kvetch("\t\t" . $should_be_upper_acc .
					  ' ?>= ' . $should_be_lower_acc);

			    ## Take a look at all of the ancestors. Only
			    ## not odd if we find the ancestor
			    ## somewhere.
			    ## TODO: ->ancestors should be more abstract and
			    ## sunk into Model::Graph?
			    my @all_ans = $a_jump->term->ancestors;
			    my $has_correct_ancestor_p = 0;
			    foreach my $ans (@all_ans) {

			      #$self->kvetch("try: " . $should_be_upper_acc .
			      #		  $ans->object->acc );
			      if( $should_be_upper_acc eq $ans->object->acc ){
				$has_correct_ancestor_p = 1;
				$self->kvetch("\t\tmatched:".$ans->object->acc.
					      ' eq ' .  $should_be_upper_acc );
				last;
			      }
			    }

			    ##
			    if( $has_correct_ancestor_p ){
			      $self->kvetch("\t\t\t". $e->code ." *not* odd.");
			      $self->_increment_count_in_info($aid,'GOOD_ISS');
			    }else{
			      $self->kvetch("\t\t\t" . $e->code .
					    " <is> odd (bad anc.).");
			      $self->_increment_count_in_info($aid, 'ODD_ISS');
			    }

			  }else{
			    ## TODO/BUG: Is bad really an interesting concept?
			    my $agpid = $a->gene_product->dbxref->xref_dbname .
			      ':'. $a->gene_product->dbxref->xref_key;
			    $self->kvetch("\t\tWRONG one: " . $e_jump->code .
					  ' - '. $agpid . ' ' . $aid);
			    $self->_increment_count_in_info($aid, 'ODD_ISS');
			  }
			}
		      }
		    }
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
ret: 0 or n > 0

=cut
sub has_exp {

  my $self = shift;
  my $aid = shift || '';
  my $ret = 0;
  my $foo = $self->_get_count_from_info($aid, 'EXP');
  $ret = 1
    if defined $foo && $foo > 0;
  return $ret;
}


=item has_good_iss

arg: aid as string
ret: 0 or n > 0

=cut
sub has_good_iss {

  my $self = shift;
  my $aid = shift || '';
  return $self->_get_count_from_info($aid, 'GOOD_ISS');
}


=item has_odd_iss

arg: aid as string
ret: 0 or n > 0

=cut
sub has_odd_iss {

  my $self = shift;
  my $aid = shift || '';
  return $self->_get_count_from_info($aid, 'ODD_ISS');
}


=item has_bad_iss

arg: aid as string
ret: 1 or 0

=cut
sub has_bad_iss {

  my $self = shift;
  my $aid = shift || '';
  return $self->_get_count_from_info($aid, 'BAD_ISS');
}


## Add a new struct to the hash if one isn't already defined.
sub _add_struct_by_id {

  my $self = shift;
  my $id = shift;

  my $info = $self->{HEV_INFO};

  if( ! defined $info->{$id} ){
    $info->{$id} = {};
    foreach my $group (@$EVIDENCE_GROUPS){
      $info->{$id}{$group} =
	{
	 count => 0,
	 evidence => {},
	};
    }
  }
}


## Increment the count of a group.
sub _increment_count_in_info {

  my $self = shift;
  my $id = shift;
  my $group = shift;

  $self->_add_struct_by_id($id);

  my $info = $self->{HEV_INFO};
  $info->{$id}{$group}{count}++;
}


## Add an evcode into a group.
sub _add_evcode_into_info {

  my $self = shift;
  my $id = shift;
  my $group = shift;
  my $evcode = shift;

  $self->_add_struct_by_id($id);

  my $info = $self->{HEV_INFO};
  $info->{$id}{$group}{evidence}{$evcode} = 1;
}


## Get the count out.
sub _get_count_from_info {

  my $self = shift;
  my $id = shift;
  my $group = shift;

  my $info = $self->{HEV_INFO};

  my $ret = 0;
  if( defined $info->{$id} &&
      defined $info->{$id}{$group} &&
      defined $info->{$id}{$group}{count} ){
    $ret = $info->{$id}{$group}{count};
  }
  return $ret;
}


## Get the seen evcodes out again.
sub _get_evcodes_from_info {

  my $self = shift;
  my $id = shift;
  my $group = shift;

  my $info = $self->{HEV_INFO};

  my @ret = ();
  if( defined $info->{$id} &&
      defined $info->{$id}{$group} &&
      defined $info->{$id}{$group}{evidence} ){
    @ret = keys %{$info->{$id}{$group}{evidence}};
    #$self->kvetch('_sc_' . scalar(@ret));
  }
  return \@ret;
}


=item evcodes_by_id

arg: aid as string and evcode group
ret: array ref of evcodes in that group for that aid

=cut
sub evcodes_by_association_id {

  my $self = shift;
  my $aid = shift || '';
  my $group = shift || '';

  return $self->_get_evcodes_from_info($aid, $group);
}



1;
