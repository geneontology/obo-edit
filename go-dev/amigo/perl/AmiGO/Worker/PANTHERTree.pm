=head1 AmiGO::Worker::PANTHERTree

Get (or not) the blobs out from the phylotree_property table if
possible. Key in on the id. No speed issues here so we'll be "lazy".

=cut

use utf8;
use strict;

package AmiGO::Worker::PANTHERTree;

use base ("AmiGO::Worker");

use Data::Dumper;


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self = $class->SUPER::new();
  #  my $excessive = shift || undef; # TODO: document

  $self->{AW_PLQ} = GOBO::DBIC::GODBModel::Query->new({type=>'phylotree_lazy'});

  bless $self, $class;
  return $self;
}


=item get_tree

Args: The fully qualified PANTHER id (or array ref of such).
Returns: Newick tree string or undef.

=cut
sub get_tree {

  my $self = shift;
  my $arg = shift || die "need an argument";
  my $ret = undef;

  ## Only array refs.
  if( ref $arg ne 'ARRAY' ){
    $arg = [$arg];
  }

  ## Get term information for display.
  my $query_results =
    $self->{AW_PLQ}->get_all_results({'me.name' => {-in => $arg}});

  if( defined $query_results ){

    $ret = [];
    foreach my $ptree (@$query_results){
      my $props = $ptree->phylotree_property;
      if( defined $props ){
	foreach my $prop ($props->all){
	  my $name = $prop->property_key;
	  my $tree = $prop->property_val;
	  # $self->kvetch("name: " . $name);
	  # $self->kvetch("tree: " . $tree);
	  push @$ret,
	    {
	     name => $prop->property_key,
	     tree => $prop->property_val,
	    };
	}
      }
    }
  }

  return $ret;
}



1;
