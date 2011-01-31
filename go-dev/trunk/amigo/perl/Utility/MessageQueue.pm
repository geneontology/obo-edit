=head1 Utility::MessageQueue

Maintains a queue of Utility::Message objects in an easy to handle
way. Not only does it handle a list of messages, but adds things like
time stamping.

Note that this is not a true subclass of Utility::Message, but it
seems more logical here.

=cut

use utf8;
use strict;
use Utility::Message;

package Utility::MessageQueue;

require Exporter;
my @ISA = qw(Exporter);
my @EXPORT = qw(new add_to_queue get_from_queue size_of_queue);
#my @EXPORT_OK = qw();


==item



=cut
sub new {

  ## Incoming arguments.
  my $class = shift;
  my $arg = shift || {};

  ## Set up class.
  my $self = {};
  #$self->{IDENTIFIER} = $arg->{IDENTIFIER} if $arg->{IDENTIFIER};

  ## Actual messages.
  $self->{MESSAGES} = [];

  bless $self, $class;
  return $self;
}


=item

Set setables.

=cut
sub add_to_message {

  my $self = shift;
  my $arg = shift || {};

  $self->{TYPE} = $arg->{TYPE} if $arg->{TYPE};
  $self->{DATE} = $arg->{DATE} if $arg->{DATE};
  $self->{ERROR_MESSAGE} = $arg->{ERROR_MESSAGE} if $arg->{ERROR_MESSAGE};
  $self->{ERROR_LIST} = $arg->{ERROR_LIST} if $arg->{ERROR_LIST};
}


=item



=cut
sub get_from_queue {

  my $self = shift;
  my $arg = shift || {};

  $self->{ERROR_MESSAGE} = $arg->{ERROR_MESSAGE} if $arg->{ERROR_MESSAGE};
  $self->{ERROR_LIST} = $arg->{ERROR_LIST} if $arg->{ERROR_LIST};

  return 1;
}


=item

=cut
sub size_of_queue {

  my $self = shift;
  my $return_val = 0;

  ## Actual messages.
  if( defined($self->{MESSAGE_QUEUE}) &&
      scalar($self->{MESSAGE_QUEUE}) ){
    $self->{MESSAGE_QUEUE};
  }

  return $return_val;
}



1;
