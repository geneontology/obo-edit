=head1 AmiGO::Worker::Intersection

Generates the n-axis species table.

=cut

use utf8;
#use diagnostics;
use strict;

package AmiGO::Worker::Intersection;

use base ("AmiGO");

use GOBO::DBIC::GODBModel::Query;


=item new

Constructor.

Args: ncbi_taxa_id, axis cardinality, (optional graph_type)

=cut
sub new {

  ##
  my $class = shift;
  my $self = $class->SUPER::new();

  my $spec = shift || die "need a species here";
  my $card = shift || die "need an integer cardinality here";
  my $graph_type = shift || 'all';

  ##
  $self->{MATRIX_SPEC} = $spec;
  $self->{MATRIX_CARD} = $card;
  $self->{MATRIX_GTYPE} = $graph_type;
  $self->{MATRIX_QUERY} =
    GOBO::DBIC::GODBModel::Query->new({type => 'gene_product_n_terms',
				       n => $card});

  bless $self, $class;
  return $self;
}


=item get_information

Args: aref of term ids

Returns: hashref of info related to the intersection of the incoming
terms for the gives ncbi_taxa_id.

=cut
sub get_information {

  my $self = shift;

  my $term_set = shift || [];
  my $n = scalar(@$term_set);

  ### Double check.
  if( ! $n || ($n != $self->{MATRIX_CARD} ) ){
    die "argument set disagrees from cardinality: $!";
  }

  ## Generate arguments from arguments. Also, cache what we see for the axis.
  my @axis_cache = ();
  ## TODO/BUG: make multiple species possible with OR.
  my $query_args = [{'species.ncbi_taxa_id' => $self->{MATRIX_SPEC}}];
  for( my $i = 1; $i <= $n; $i++ ){
    my $ikey = 'object_aux_' . $i . '.acc';
    my $ival = $$term_set[$i-1];
    push @$query_args, {$ikey => $ival};
    push @axis_cache, $ival;
  }

  ## Generate new answer.
  ## TODO/BUG: Add the ability to toggle 'regulates' here on and
  ## off once the database supports it.
  my $matrix_results = undef;
  if( $self->{MATRIX_GTYPE} eq 'change_me_later' ){
    $matrix_results = [];
  }else{
    ## This is the problem--here
    $self->kvetch('query_args: ' . $query_args);
    $matrix_results =
      $self->{MATRIX_QUERY}->get_all_results({-and => $query_args});
  }

  ## Cache gp information.
  my $gp_cache = {};
  foreach my $gp (@$matrix_results){
    my $gpid = $gp->dbxref->xref_dbname. ':' . $gp->dbxref->xref_key;
    $gp_cache->{$gpid} = 1;
  }

  ## Cache new answer.
  my @gp_list = keys %$gp_cache;
  my $answer_cache =
    {
     gene_product_count => scalar(@gp_list),
     gene_products => \@gp_list,
     axes => \@axis_cache,

     ## TODO/BUG?: Should links really be part of it here? Very
     ## gauche.
     link => $self->get_interlink({mode => 'gp-assoc', 
				   arg => {gps => \@gp_list}}),
    };

  return $answer_cache;
}



1;
