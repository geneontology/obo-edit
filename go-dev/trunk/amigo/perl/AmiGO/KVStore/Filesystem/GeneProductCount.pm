=head1 AmiGO::KVStore::Filesystem::GeneProductCount

...

=cut

package AmiGO::KVStore::Filesystem::GeneProductCount;

use base 'AmiGO::KVStore::Filesystem';


=item new

# Sets the right parameters for the super class.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new('gp_count', 1000);

  bless $self, $class;
  return $self;
}



1;
