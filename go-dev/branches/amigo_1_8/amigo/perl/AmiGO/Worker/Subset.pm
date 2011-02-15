=head1 AmiGO::Worker::Subset

Generates consumable static information about ontology subsets and
their terms. This is not a search tool, but a (hopefully efficient)
data retrieval tool.

=cut

use utf8;
use strict;

package AmiGO::Worker::Subset;

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

  #$self->{AW_SQ} =
  #$self->{AW_AID} = AmiGO::Aid->new();

  bless $self, $class;
  return $self;
}


=item get_term_accs

Args: term subset acc string
Returns: hash ref keyed by included term acc

=cut
sub get_term_accs {

  my $self = shift;
  my $subset_acc = shift || die "need an argument";

  my $sqry = GOBO::DBIC::GODBModel::Query->new({type=>'term_subset'});
  my $query_results =
    $sqry->get_all_results({'subset.acc' => {'=', $subset_acc}});
  my $term_list = {};
  foreach my $sub_res (@$query_results){
    my $term = $sub_res->term->acc;
    $term_list->{$term} = 1;
  }

  return $term_list;
}


=item get_subset_accs

Args:
Returns: hash ref keyed by included subset accs

=cut
sub get_subset_accs {

  my $self = shift;

  my $sqry = GOBO::DBIC::GODBModel::Query->new({type=>'term_lazy'});
  my $query_results =
    $sqry->get_all_results({'me.term_type' => {'=', 'subset'}});
  my $acc_list = {};
  foreach my $sub_res (@$query_results){
    my $term = $sub_res->acc;
    $acc_list->{$term} = 1;
  }

  return $acc_list;
}



1;
