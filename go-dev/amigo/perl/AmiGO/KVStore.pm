=head1 AmiGO::KVStore

A library to manage and access the produced key-value stores (the subclasses).

=cut

package AmiGO::KVStore;

use base 'AmiGO';
use GO::SQLite3::KVStore;


=item new

Args: name
Returns: 1/0

Creates (or opens an extant) key-value store.

=cut
sub new {

  ##
  my $class = shift;
  my $loc = shift || die "gotta have a name path here $!";
  my $self = $class->SUPER::new();

  ## Get the store out on disk.
  $self->{AKVS_LOCATION} =
    $self->amigo_env('AMIGO_CACHE_DIR') . '/kvstore_'.  $loc . '.db';
  $self->{AKVS_STORE} = GO::SQLite3::KVStore->new($self->{AKVS_LOCATION}, 1);

  bless $self, $class;
  return $self;
}



1;
