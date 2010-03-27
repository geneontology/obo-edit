=head1 GOBO::DBIC::GODBModel::Modify

Used to modify the database (as opposed to query, which is for reading...)

=cut

package GOBO::DBIC::GODBModel::Modify;

use base 'GOBO::DBIC::GODBModel';
use utf8;
use strict;
use GOBO::DBIC::GODBModel::Schema;


=item new

...TODO...

=cut
sub new {

  ##
  my $class = shift;
  my $args = shift || {};
  my $self = $class->SUPER::new($args);

  ## Argument processing.
  #my $args = shift || {};
  $self->{MOD_TYPE} = $args->{type} || die "need type: $!";
  $self->{MOD_RS} = $self->{SCHEMA}->resultset($self->{MOD_TYPE});

  bless $self, $class;
  return $self;
}


=item add

Args: ?
Returns: ?

=cut
sub add {

  my $self = shift;
  my $args = shift || die "need good arg hash";

  return $self->{MOD_RS}->update_or_create($args);
}



1;
