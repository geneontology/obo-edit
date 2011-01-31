=head1 AmiGO::Core

Defines a few constants and helpers for the sub classes. Should the
actual live database stuff (as opposed to statics from installation
time) be from here on down?

=cut

use utf8;
use strict;

package AmiGO::Core;

use base ("AmiGO");


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  ## We'll borrow SUCCES and ERROR_MESSAGE from AmiGO.

  bless $self, $class;
  return $self;
}



1;
