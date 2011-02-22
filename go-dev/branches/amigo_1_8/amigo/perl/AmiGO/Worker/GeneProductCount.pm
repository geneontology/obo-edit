=head1 AmiGO::Worker::GeneProductCount

Generates consumable static information about gene product counts.
This is not a search tool, but a (hopefully efficient) data retrieval
tool.

BUG/TODO: Why does get_info exist? Can't that be done better? Take a
look at how it is called in term_details...

=cut

use utf8;
use strict;

package AmiGO::Worker::GeneProductCount;

use base ("AmiGO::Worker");

use Data::Dumper;
use AmiGO::JavaScript;
use AmiGO::KVStore::Filesystem::GeneProductCount;


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self = $class->SUPER::new();
  my $acc = shift || die "need an acc";
  my $arg = shift || die "need an acc list";

  ## Only array refs as core args.
  if( ref $arg ne 'ARRAY' ){ $arg = [$arg]; }
  $self->{AW_GPCQ_ACC} = $arg;

  ## Give me an absolute ordering and join to act as a key.
  #my @sorted = sort { return $a eq $b; } @$arg;
  #my $key = join '^', @sorted;
  my $key = $acc;

  ## Get the JS in file off of cache if possible.
  my $cache = undef;
  my $js = AmiGO::JavaScript->new();
  my $fs = AmiGO::KVStore::Filesystem::GeneProductCount->new();
  my $json_blob = $fs->get($key);
  if( $json_blob ){
    $self->kvetch("in cache: " . $key);
    $cache = $js->parse_json_data($json_blob);
    if( ! defined $cache || $self->empty_hash_p($cache) ){
      die "something went south in the conversion";
    }
  }else{
    $self->kvetch("not in cache: " . $key);
  }
  $self->{AW_GPCQ_CACHE} = $cache;
  $self->{AW_GPCQ_JS} = $js;
  $self->{AW_GPCQ_FS} = $fs;
  $self->{AW_GPCQ_KEY} = $key;

  ##
  $self->{AW_GPCQ} =
    GOBO::DBIC::GODBModel::Query->new({type=>'gene_product_count'});

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

  if( ! defined $self->{AW_GPCQ_CACHE} ){

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

      ## TODO/BUG: this is very expensive--too many species. Needs
      ## better caching.  ncbi_taxa_id gets ncbi taxa id or undef.
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
    $self->{AW_GPCQ_FS}->put($self->{AW_GPCQ_KEY},
			     $self->{AW_GPCQ_JS}->make_js($gpc_info));
    #print STDERR "<<IN_3>>\n";
  }

  # $self->kvetch('final: ' . $self->{AW_GPCQ_TOTAL} );

  ##
  return $self->{AW_GPCQ_CACHE};
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
