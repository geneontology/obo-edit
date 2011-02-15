=head1 AmiGO::Worker::Term

Generates consumable static information about ontology terms.
This is not a search tool, but a (hopefully efficient) data retrieval tool.

=cut

use utf8;
use strict;

package AmiGO::Worker::Term;

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
  #  my $excessive = shift || undef; # TODO: document

  $self->{AW_TQ} = GOBO::DBIC::GODBModel::Query->new({type=>'term_lazy'});
  #$self->{AW_TG} = GOBO::DBIC::GODBModel::Graph->new();
  $self->{AW_AID} = AmiGO::Aid->new();
  #  $self->{AWT_LOTS} = $excessive;

  bless $self, $class;
  return $self;
}


=item get_info

Args: term acc string or arrayref of term acc strings.
Returns: hash containing various term infomation, keyed by acc

=cut
sub get_info {

  my $self = shift;
  my $arg = shift || die "need an argument";

  ## Only array refs.
  if( ref $arg ne 'ARRAY' ){
    $arg = [$arg];
  }

  ## Get term information for display.
  my $query_results =
    $self->{AW_TQ}->get_all_results({'me.acc' => {-in => $arg}});
  my $term_info = $self->{AW_AID}->term_information($query_results);

  # ## Add even more information! But only if desired...
  #  if( defined $self->{AWT_LOTS} && $self->{AWT_LOTS} ){
  #  }

  return $term_info;
}


# =item get_ancestors

# Args: term acc string or arrayref of term acc strings.
# Returns: hash containing various term infomation, keyed by acc

# =cut
# sub get_ancestors {

#   my $self = shift;
#   my $arg = shift || die "need an argument";

#   ## Only array refs.
#   if( ref $arg ne 'ARRAY' ){
#     $arg = [$arg];
#   }

#   ## Get term information for display.
#   my $query_results =
#     $self->{AW_TQ}->get_all_results({'me.acc' => {-in => $arg}});
#   my $term_info = $self->{AW_AID}->term_information($query_results);

#   # ## Add even more information! But only if desired...
#   #  if( defined $self->{AWT_LOTS} && $self->{AWT_LOTS} ){
#   #  }

#   return $term_info;
# }



1;
