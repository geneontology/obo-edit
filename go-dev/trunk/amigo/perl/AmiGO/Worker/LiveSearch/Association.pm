=head1 AmiGO::Worker::LiveSearch::Association

Basically, a LexicalSearch that we have more control over (adding and
removing terms in post processing, etc.). Forked from the named.

...

=cut


package AmiGO::Worker::LiveSearch::Association;
use base ("AmiGO::Worker::LiveSearch");


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $args = shift || {};

  my $self = $class->SUPER::new({index => 'association',
				 search_fields =>
				 ['acc', 'name', 'synonym',
				  'dbxref','symbol','full_name','gpsynonym']});

  bless $self, $class;
  return $self;
}


=item query

Perform a lexical query with a json string result.

=cut
sub query {

  my $self = shift;
  my $qstr = shift || '';
  my $index = shift || 0;
  my $count = shift || 0;

  my $ontology = shift || [];

  my $species = shift || [];
  my $source = shift || [];
  my $gptype = shift || [];

  my $evidence = shift || [];

  ##
  my $results =
    $self->SUPER::query({
			 query => $qstr,
			 index => $index,
			 count => $count,
			 filters => {
				     ontology => $ontology,
				     species => $species,
				     source => $source,
				     gptype => $gptype,
				     evidence => $evidence,
				    },
			 return_fields =>
			 ['acc', 'name', 'synonym', 'ontology',
			  'dbxref', 'symbol', 'full_name' ,'gpsynonym',
			  'species', 'source', 'gptype',
			  'evidence'],
			});

  ## Add qurls.
  if( defined $results &&
      defined $results->{hits} &&
      scalar @{$results->{hits}} ){

    foreach my $hit (@{$results->{hits}} ){
      my $acc = $hit->{acc};
      my $dbxref = $hit->{dbxref};
      $hit->{gene_product_link} = $self->get_interlink({mode=>'gp-details',
							arg=>{gp=>$dbxref}});
      $hit->{term_link} = $self->get_interlink({mode=>'term-details',
						arg=>{acc=>$acc}});
    }
  }

  return $results;
}



1;
