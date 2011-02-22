=head1 Utility::Message



=cut

use utf8;
use strict;

package Utility::Message;


=head2

The available methods are

=cut
require Exporter;
my @ISA = qw(Exporter);
my @EXPORT = qw(
		 new
		 set_message get_message
		 add_to_list get_list
	      );


=item new

Arguments: string of message (required)
Returns: Message object (undef on construction error)

=cut
sub new {

  ##
  my $class = shift;
  my $msg = shift || '';

  ## Internal DS.
  my $self = {};
  $self->{MESSAGE} = '';
  $self->{LIST} = [];

  ## Only add a message and message list if message is properly
  ## defined. There was a need for this paranoia at one point...but
  ## I've forgotten why.
  if( _is_well_defined($msg) ){
    $self->{MESSAGE} = $msg;
  }else{
    return undef;
  }

  bless $self, $class;
  return $self;
}


=item set_message

This functionality has been folded into the constructor.

=cut
# sub set_message {
#   my $self = shift;
#   my $msg = shift || "";
#   my $ok_p = 0;
#   if( _is_well_defined($msg) ){
#     $self->{MESSAGE} = $msg;
#     $ok_p = 1;
#   }
#   return $ok_p;
# }


=item get_message

Arguments: none
Returns: string containing the message

=cut
sub get_message {

  my $self = shift;
  return $self->{MESSAGE};
}


=item add_to_list

This method allows adding an arbitrary list of information (strings)
to a message.

Arguments: array (of strings) or string
Returns: number of things added to list

=cut
sub add_to_list {

  my $self = shift;
  my(@args) = @_;
  my $ok_p = 0;

  foreach my $arg (@args) {
    if ( _is_well_defined($arg) ) {
      push( @{$self->{LIST}}, $arg);
      $ok_p++; # only ok if we added at least one element
    }
  }

  return $ok_p;
}


=item

Arguments: none
Returns: an array of the list that has been added to the message

=cut
sub get_list {

  my $self = shift;
  return @{$self->{LIST}};
}


=item

A private sub that defines what is accepted as input for a
message.

Arguments: hopefully a string
Returns: 1 or 0

=cut
sub _is_well_defined {

  my $str = shift;
  my $is_well_defined = 0;

  if( $str && defined($str) && length($str) > 0 ){
    $is_well_defined = 1;
  }

  return $is_well_defined;
}



1;
