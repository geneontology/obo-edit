=head1 AmiGO::Worker::GeneProduct

Generates consumable static information about gene products.
This is not a search tool, but a (hopefully efficient) data retrieval tool.

=cut

use utf8;
use strict;

package AmiGO::Worker::GeneProduct;

use base ("AmiGO::Worker");

use Data::Dumper;
use AmiGO::Aid;

=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self = $class->SUPER::new();

  $self->{AW_GPQ} = GOBO::DBIC::GODBModel::Query->new({type=>'dbxref_lazy'});
  $self->{A_AID} = AmiGO::Aid->new();;

  bless $self, $class;
  return $self;
}


=item get_info

Args: gene product acc string or arrayref of term gene product strings.
Returns: hash containing various gene product infomation, keyed by acc

=cut
sub get_info {

  my $self = shift;
  my $arg = shift || die "need an argument";

  ## Only array refs.
  if( ref $arg ne 'ARRAY' ){
    $arg = [$arg];
  }

  ## Split out for query by dbxrefs and query out the gene product.
  my $gp_info = {};
  foreach my $acc (@$arg){
    my($dbname, $key) = $self->split_gene_product_acc($acc);

    ## Get gp information.
    ## TODO: batch this.
    # $self->kvetch('h2.0: ' . $self->{AW_GPQ});
    # $self->kvetch('h2.1: ' . $dbname);
    # $self->kvetch('h2.2: ' . $key);
    my $query_results =
      $self->{AW_GPQ}->get_all_results({
					'me.xref_dbname' => $dbname,
					'me.xref_key' => $key,
				       });
    # $self->kvetch('h3');
    if( ! @$query_results ){
      ## Couldn't be found.
    }elsif( scalar(@$query_results) > 1 ){
      die 'ERROR: non-unique gp acc';
    }else{
      my $tmp_gp = $query_results->[0]->gene_product;
      my $tmp_gp_info =	$self->{A_AID}->gene_product_information([$tmp_gp]);
      my $tmp_key = (keys(%$tmp_gp_info))[0];
      my $tmp_val = (values(%$tmp_gp_info))[0];
      $gp_info->{$tmp_key} = $tmp_val;
    }
  }

  return $gp_info;
}



1;
