=head1 AmiGO::Worker::GeneProductCount

Generates consumable static information about gene product counts.
This is not a search tool, but a (hopefully efficient) data retrieval tool.

=cut

use utf8;
use strict;

package AmiGO::Worker::GeneProductCount;

use base ("AmiGO::Worker");

use Data::Dumper;
use AmiGO::Aid;
use AmiGO::KVStore::GPCount;


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self = $class->SUPER::new();
  my $arg = shift || die "need an argument";

  ## Only array refs as core args.
  if( ref $arg ne 'ARRAY' ){ $arg = [$arg]; }
  $self->{AW_GPCQ_ACC} = $arg;

  # ##
  # $self->{AW_GPCQ_TOTAL} = undef;

  $self->{AW_GPCQ_CACHE} = {};

  ##
  $self->{AW_GPCQ} =
    GOBO::DBIC::GODBModel::Query->new({type=>'gene_product_count'});
  # $self->{A_AID} = AmiGO::Aid->new();;

  bless $self, $class;
  return $self;
}


=item get_info

Args: term acc string or arrayref of term term strings.
Returns: hash containing various gene product infomation, keyed by acc

=cut
sub get_info {

  my $self = shift;

  my $acc = $self->{AW_GPCQ_ACC};
  # $self->{AW_GPCQ_TOTAL} = 0;

  #print STDERR "<<IN_0>>\n";

  ##
  my $query_results = $self->{AW_GPCQ}->get_all_results({'term.acc' => $acc});

  #print STDERR "<<IN_1>>\n";

  ##
  my $spec2ncbi_map = $self->species_to_ncbi_map();
  #$self->kvetch(Dumper($spec2ncbi_map));

  #print STDERR "<<IN_2>>\n";

  ##
  my $spec_cache = {};
  my $gpc_info = {};
  foreach my $gpc (@$query_results){

    ## TODO/BUG: this is very expensive--too many species. Needs better caching.
    ## ncbi_taxa_id gets ncbi taxa id or undef.
    my $ncbi_taxa_id = undef;
    if( defined $spec2ncbi_map->{$gpc->species_id} ){
      $ncbi_taxa_id = $spec2ncbi_map->{$gpc->species_id};
      # $self->kvetch('got cached: ' . $ncbi_taxa_id);
    }else{
      # $self->kvetch('no cached');
    }

    ## Setup if we haven't seen this acc before.
    my $acc = $gpc->term->acc;
    if( ! defined $gpc_info->{$acc} ){
      $gpc_info->{$acc} = [];
    }

    ## 
    my $count = $gpc->product_count;
    #$self->kvetch('gpc: ' . $acc . ', c: ' . $count);

    # ## Count only on the acc beat.
    # if( $self->{AW_GPCQ_ACC} eq $acc ){
    #   $self->{AW_GPCQ_TOTAL} += $count;
    # }

    push @{$gpc_info->{$acc}},
      {
       code => $gpc->code,
       dbname => $gpc->speciesdbname,
       ## TODO/BUG: Add species after creating separate id 2 species hash...
       ncbi_taxa_id => $ncbi_taxa_id,
       count => $count,
      };
  }

  $self->{AW_GPCQ_CACHE} = $gpc_info;

  #print STDERR "<<IN_3>>\n";

  # $self->kvetch('final: ' . $self->{AW_GPCQ_TOTAL} );

  ##
  return $gpc_info;
}


=item get_count

Args: none, but must call after get_info
Returns: integer

=cut
sub get_count {

  my $self = shift;
  my $acc = shift || die "get_count needs an argument";

  my $retval = undef;
  my $gpc_info = $self->{AW_GPCQ_CACHE};
  my $gpc_case = $gpc_info->{$acc};

  if( defined $gpc_case && scalar(@$gpc_case) ){
    foreach my $item (@$gpc_case){
      if( defined $item->{dbname} ){
	$retval += $item->{count};
      }
    }
  }
  return $retval;
}



1;
