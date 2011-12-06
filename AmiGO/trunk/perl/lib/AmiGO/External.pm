=head1 AmiGO::External

Defines a general setup to contact external (RESTy) data sources.
There must be one object per external source.

=cut


package AmiGO::External;
use base ("AmiGO");

use utf8;
use strict;
use WWW::Mechanize;
use Carp;


=item new

#

=cut
sub new {

  ##
  my $class = shift;
  my $self = $class->SUPER::new();

  #print STDERR "___: " . $self . "\n";

  ## Ready mech.
  my $agent_version = $self->amigo_env('AMIGO_VERSION') || '?.?';
  $self->{MECH} = WWW::Mechanize->new(agent => 'AmiGO ' . $agent_version);

  ## TODO: Tie this to an AmiGO variable after merge from MAINTENANCE
  $self->{MECH}->timeout(2); # Timeout in seconds

  bless $self, $class;
  return $self;
}


=item get_external_data

Interface stub.
This must get overrided in subclasses.
It should add the external document to EXT_DATA.

=cut
sub get_external_data { die "must override get_external_data method $!"; }


=item try

Interface stub.
This must get overrided in subclasses.
It should try to safely extract a data point path from the external source.
And may take an optional second arguement for what to return in the case
of failure.

Will likely be working off of EXT_DATA.

=cut
sub try { die "must override try method $!"; }


1;
