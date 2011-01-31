=head1 AmiGO::Worker::LiveSearch::GeneProduct

Basically, a LexicalSearch that we have more control over (adding and
removing terms in post processing, etc.). Forked from the named.

use AmiGO::Worker::LiveSearch::GeneProduct;
$ls = AmiGO::Worker::LiveSearch::GeneProduct->new();
print Dumper($ls->query('choc*', 0, 10, [7227], ['FB'], [], []))

=cut


package AmiGO::Worker::LiveSearch::GeneProduct;
use base ("AmiGO::Worker::LiveSearch");


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $args = shift || {};

  my $self = $class->SUPER::new({index => 'gene_product',
				 search_fields =>
				 ['dbxref','symbol','full_name', 'gpsynonym']});

  bless $self, $class;
  return $self;
}


=item query

Perform a lexical query with a json string result.

TODO/BUG: replace args with arg hash.

=cut
sub query {

  my $self = shift;
  my $qstr = shift || '';
  my $index = shift || 0;
  my $count = shift || 0;

  my $species = shift || [];
  my $source = shift || [];
  my $gptype = shift || [];
  my $homolset = shift || [];

  ##
  my $filters_hash =
    {
     species => $species,
     source => $source,
     gptype => $gptype,
     homolset => $homolset,
    };
  # if( $homolset && scalar(@$homolset) && 
  #   $filters_hash->{homolset} = $homolset;
  # }
  my $results =
    $self->SUPER::query({
			 query => $qstr,
			 index => $index,
			 count => $count,
			 filters => $filters_hash,
			 return_fields =>
			 ['dbxref', 'symbol', 'full_name', 'gpsynonym',
			  'species', 'source', 'gptype', 'homolset'],
			});

  ## Add qurls.
  if( defined $results &&
      defined $results->{hits} &&
      scalar @{$results->{hits}} ){

    foreach my $hit (@{$results->{hits}} ){
      my $acc = $hit->{dbxref};
      $hit->{link} = $self->get_interlink({mode=>'gp-details',
					   arg=>{gp=>$acc}});
    }
  }

  return $results;
}



1;
