=head1 AmiGO::Log

Needs just enough of the AmiGO environment to be able to put logging
into a use (probably session) place. Otherwise, it will just dump to
STDOUT (thereby making it more useful as a general access tool for the
GO DB).

=cut

use utf8;
use strict;

package AmiGO::Log;

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
