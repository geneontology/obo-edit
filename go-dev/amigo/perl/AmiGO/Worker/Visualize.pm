=head1 AmiGO::Worker::Visualize

TODO: Return a blob that visualizes the ontology.

TODO: Rationalize visual (so it's just an intelligent web wrapper) and
add in QuickGO resource.

variables:
simple_terms,
complex_terms,
format,
source

=cut

package AmiGO::Worker::Visualize;

## Use a slightly different base...
use base ("AmiGO::Worker");

use AmiGO::GraphViz;
use AmiGO::SVGRewrite;
use AmiGO::JavaScript;
use AmiGO::JSON;
use AmiGO::External::QuickGO::OntGraphics;


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();
  my $self  = $class->SUPER::new();

  my $term_list = shift || [];

  ## Build graph with term list.
  ## Go through build graph routine only if there is something coming
  ## in.
  if( defined($term_list) &&
      scalar(@$term_list) != 0 ){

    my $graph = GOBO::DBIC::GODBModel::Graph->new();

    ## Convert string terms to DBIC terms.
    my $terms = [];
    foreach my $acc (@$term_list){
      my $term = $graph->get_term($acc);
      if( defined $term ){
	push @$terms, $term;
      }
    }
  }

  bless $self, $class;
  return $self;
}


sub

1;
