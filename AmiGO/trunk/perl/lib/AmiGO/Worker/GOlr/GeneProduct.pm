=head1 AmiGO::Worker::GOlr::GeneProduct

Generates consumable static information about gene products, and
backed by a GOlr document store. It should be similar in structure to
AmiGO::Worker::GeneProduct (which should be eventually removed).

This is not a search tool, but an efficient data retrieval tool.

=cut

package AmiGO::Worker::GOlr::GeneProduct;
use base ("AmiGO::Worker::GOlr");


=item new

Args: gp acc string # or arrayref of gp acc strings.
Returns: hash containing various gp infomation, keyed by acc

=cut
sub new {

  my $class = shift;
  my $args = shift || die "need an argument";
  my $self = $class->SUPER::new();

  ## Only array refs internally.
  if( ref $args ne 'ARRAY' ){ $args = [$args]; }

  ## ...
  $self->{AWGG_INFO} = {};
  foreach my $arg (@$args){
    my $found_doc = $self->{AEJS_GOLR_DOC}->get_by_id($arg);

    my $intermediate = undef;
    if( $found_doc ){
      $intermediate =
	{
	 acc => $found_doc->{id},
	 id => $found_doc->{id},
	 name => $found_doc->{label},
	 label => $found_doc->{label},
	 type => $found_doc->{type},
	 source => $found_doc->{source},
	 taxon => $found_doc->{taxon},
	 taxon_readable => $found_doc->{taxon},
	 #ontology_readable => $self->{A_AID}->readable($found_doc->{source}),
	 gp_link =>
	 $self->get_interlink({mode=>'gp_details',
			       arg=>{gp=>$found_doc->{id}}}),
	 descriptive_name => $found_doc->{descriptive_name},
	 #comment => $found_doc->{comment},
	 synonyms => [],
	 #dbxrefs => [],
	 #gp_dbxrefs => [],
	};
    }
    $self->{AWGG_INFO}{$arg} = $intermediate;
  }

  bless $self, $class;
  return $self;
}


=item get_info

Args: n/a
Returns: hash ref containing various gene product information, keyed by acc

=cut
sub get_info {

  my $self = shift;
  return $self->{AWGG_INFO};
}



1;
