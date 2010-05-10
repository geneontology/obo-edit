=head1 AmiGO::Worker::LiveSearch::Term

Specialize on terms. Add qurl.


use AmiGO::Worker::LiveSearch::Term
$ls = AmiGO::Worker::LiveSearch::Term->new();
print Dumper($ls->query('neurogenesis', 0, 10, ['biological_process']));

=cut


package AmiGO::Worker::LiveSearch::Term;
use base ("AmiGO::Worker::LiveSearch");


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $args = shift || {};

  ##
  #my $lucy = AmiGO::Lucene->new();
  #my @kws = keys( %{$lucy->term_keywords()} );
  #my $self  = $class->SUPER::new({index => 'term', search_fields => \@ksw);
  my $self  = $class->SUPER::new({index => 'term',
				  search_fields => ['acc','name','synonym']});

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

  my $results =
    $self->SUPER::query({
			 query => $qstr,
			 index => $index,
			 count => $count,
			 filters => {ontology => $ontology},
			 return_fields => ['acc','name','synonym','ontology'],
			});

  ## Add qurls.
  if( defined $results &&
      defined $results->{hits} &&
      scalar @{$results->{hits}} ){

    foreach my $hit (@{$results->{hits}} ){
      my $acc = $hit->{acc};
      $hit->{link} = $self->get_interlink({mode=>'term-details',
					   arg=>{acc=>$acc}});
    }
  }

  return $results;
}



1;
