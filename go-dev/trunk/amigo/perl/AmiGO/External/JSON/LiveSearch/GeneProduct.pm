=head1 AmiGO::External::JSON::LiveSearch::GeneProduct

...

=cut

package AmiGO::External::JSON::LiveSearch::GeneProduct;

use base ("AmiGO::External::JSON::LiveSearch");


=item new

Just arg to superclass.

=cut
sub new {

  ## 
  my $class = shift;
  my $self  = $class->SUPER::new('gene_product');
  bless $self, $class;
  return $self;
}



1;
