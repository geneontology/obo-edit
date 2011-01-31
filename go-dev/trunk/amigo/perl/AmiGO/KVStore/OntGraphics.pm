=head1 AmiGO::KVStore::OntGraphics

Wrapper for QuickGO ontology visualization. Final layer in
storage/cache.

=cut

package AmiGO::KVStore::OntGraphics;

use base ("AmiGO::KVStore");


=item new

# Sets the right parameters for the super class.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new('local_ont');

  bless $self, $class;
  return $self;
}



1;
