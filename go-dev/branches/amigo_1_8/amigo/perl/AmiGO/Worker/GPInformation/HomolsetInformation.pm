=head1 AmiGO::Worker::GPInformation::HomolsetInformation

test branch

TODO/BUG: A lot of the work here can be memoized out, but I'm
not sure if it's worth the effort at this point...

=cut


use utf8;
#use diagnostics;
use strict;

package AmiGO::Worker::GPInformation::HomolsetInformation;

use base ("AmiGO::Worker::GPInformation");

use GOBO::DBIC::GODBModel::Schema;
use GOBO::DBIC::GODBModel::Query;
use GOBO::DBIC::GODBModel::Graph;
use AmiGO::Worker::HomolsetEvidence;
use Time::HiRes qw(gettimeofday tv_interval);


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new(@_);

  #my $arg_hash = shift || {};
  #$self->{SKIP_ROOTS} = 1;
  #if( defined $arg_hash->{skip_roots} ){
  #  $self->{SKIP_ROOTS} = $arg_hash->{skip_roots};
  #}


  ## Start.
  my $time = [gettimeofday];

  $self->{QUERY}= GOBO::DBIC::GODBModel::Query->new({type=>'gene_product_homolset'});

  ##
  $self->kvetch('_0_time: ' . tv_interval($time));

  $self->{HOMOLSET_SYMBOL} = undef;
  $self->{HOMOLSET_LINK} = undef;

  bless $self, $class;
  return $self;
}


=item calculate

foo

=cut
sub calculate {

  my $self = shift;
  my $set = shift || die "calculate requires an arg: $!";

  ## Start.
  my $time = [gettimeofday];

  ## For all of the gphs in the in the homolset...go through all of
  ## the gps...
  my $gps = [];
  my $all_gphs = $self->{QUERY}->get_all_results({homolset_id => $set});
  foreach my $gph (@$all_gphs){

    ## Grab the hs symbol if we haven't.
    if( ! defined($self->{HOMOLSET_SYMBOL}) ){
      my $hss = $gph->homolset->symbol;
      ## BUG: There seems to be some control characters slipping in.
      $hss =~ s/^\s+//; #remove leading spaces
      $hss =~ s/\s+$//; #remove trailing spaces
      $self->{HOMOLSET_SYMBOL} = $hss;

      ## Make the homolset link while we're here.
      my $hsid = $gph->homolset->id;
      $self->{HOMOLSET_LINK} =
	$self->get_interlink({mode => 'homolset_annotation',
			      arg => {set => $hsid}});
    }

    my $gp = $gph->gene_product;
    push @$gps, $gp;
  }

  my $ev_calc = AmiGO::Worker::HomolsetEvidence->new();
  $ev_calc->generate_homolset_evidence($set);

  ## Do the calculation, add the additional HS information, but filter
  ## out anything with the same "personality" (information dupes).
  $self->generate_information($gps);
  foreach my $acc (keys %{$self->{SEEN_ASSOCS}}){
    foreach my $ncbi_taxa_id (keys %{$self->{SEEN_ASSOCS}{$acc}}){
      foreach my $a_id (keys %{$self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}}){

	## Do ISS and experimental detection.
	my $has_exp_p = $ev_calc->has_exp($a_id);
	my $has_good_iss_p = $ev_calc->has_good_iss($a_id);
	my $has_odd_iss_p = $ev_calc->has_odd_iss($a_id);
	my $has_bad_iss_p = $ev_calc->has_bad_iss($a_id);

	## TEST: 
	my $exp_ev = $ev_calc->evcodes_by_association_id($a_id,'EXP');
	my $good_iss_ev =$ev_calc->evcodes_by_association_id($a_id,'GOOD_ISS');
	my $odd_iss_ev = $ev_calc->evcodes_by_association_id($a_id,'ODD_ISS');

	$self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}{$a_id}{exp_evidence} =
	  $exp_ev;
	$self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}{$a_id}{good_iss_evidence} =
	  $good_iss_ev;
	$self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}{$a_id}{odd_iss_evidence} =
	  $odd_iss_ev;

	$self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}{$a_id}{has_exp_p} =
	  $has_exp_p;
	$self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}{$a_id}{has_good_iss_p} =
	  $has_good_iss_p;
	$self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}{$a_id}{has_odd_iss_p} =
	  $has_odd_iss_p;
	$self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}{$a_id}{has_bad_iss_p} =
	  undef;

# 	## DEBUG
# 	if( $acc eq 'GO:0003677' ){
# 	  $self->kvetch('CHECK '. $acc . ' (' . $ncbi_taxa_id . '): ' .
# 			scalar(@$exp_ev) . ' ' . $has_exp_p, 1);
# 	}
      }
    }
  }

  $self->kvetch("Exiting from HomolsetInformation...");
  return 1;
}


# =item remove_association_duplicates

# Remove associations that have the same term, gp, and properties, but
# different assoc ids.

# TODO:

# =cut
# sub remove_association_duplicates {

#   my $self = shift;

#   my %keys = ();
#   foreach my $acc (keys %{$self->{SEEN_ASSOCS}}){
#     foreach my $ncbi_taxa_id (keys %{$self->{SEEN_ASSOCS}{$acc}}){
#       foreach my $a_id (keys %{$self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}}){

# 	my $assoc = $self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}{$a_id};
# 	my $gp_id = $assoc->{gene_product_id};
# 	my $dir = $assoc->{direct_p};
# 	my $exp = $assoc->{has_exp_p};
# 	my $good = $assoc->{has_good_iss_p};
# 	my $odd = $assoc->{has_odd_iss_p};
# 	my $bad = $assoc->{has_bad_iss_p};

# 	## Strip out assocs where the "personality" is the same
# 	## (i.e. equiv has_X_p profile).
# 	my $unique_enough =
# 	  $ncbi_taxa_id . $gp_id . $dir . $exp . $good . $odd . $bad . $acc;
# 	if( defined $keys{$unique_enough} ){
# 	  delete $self->{SEEN_ASSOCS}{$acc}{$ncbi_taxa_id}{$a_id};
# 	  $self->kvetch($a_id . " is not unique enough; dropping...");
# 	}else{
# 	  $keys{$unique_enough} = 1;
# 	}
#       }
#     }
#   }
# }


=item get_symbol

Get the homolset symbol string.

=cut
sub get_symbol {
  my $self = shift;
  return $self->{HOMOLSET_SYMBOL} || 'unknown_symbol';
}


=item get_link

Get the link for this homolset's annotation page

=cut
sub get_link {
  my $self = shift;
  return $self->{HOMOLSET_LINK} || '';
}



1;
