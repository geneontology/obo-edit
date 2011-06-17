=head1 AmiGO::Worker::Solr::GeneProduct

Generates consumable static information about gene products, and
backed by a Solr document store. It should be similar in structure to
AmiGO::Worker::GeneProduct (which should be eventually removed).

This is not a search tool, but an efficient data retrieval tool.

=cut

use utf8;
use strict;

package AmiGO::Worker::Solr::GeneProduct;

use base ("AmiGO::Worker::Solr");

use Data::Dumper;
use AmiGO::Aid;
use AmiGO::External::JSON::SolrDocument;


=item get_info

Args: gp acc string # or arrayref of gp acc strings.
Returns: hash containing various gp infomation, keyed by acc

=cut
sub get_info {

  my $self = shift;
  my $args = shift || die "need an argument";

  ## Only array refs internally.
  if( ref $args ne 'ARRAY' ){ $args = [$args]; }

  ## ...
  my $gp_info = {};
  foreach my $arg (@$args){
    my $found_doc = $self->{AEJ_SOLR}->get_by_id($arg);

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
	 $self->get_interlink({mode=>'gp-details',
			       arg=>{gp=>$found_doc->{id}}}),
	 descriptive_name => $found_doc->{descriptive_name},
	 #comment => $found_doc->{comment},
	 synonyms => [],
	 #dbxrefs => [],
	 #gp_dbxrefs => [],
	};
    }
    $gp_info->{$arg} = $intermediate;
  }

  return $gp_info;
}



1;
