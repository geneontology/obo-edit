=head1 AmiGO::Worker::Solr

Base class for things trying to get static consumable data out of an
external GOlr index.

=cut

use utf8;
use strict;

package AmiGO::Worker::Solr;

use base ("AmiGO::Worker");

use Data::Dumper;
use AmiGO::Aid;
use AmiGO::External::JSON::SolrDocument;


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self = $class->SUPER::new();

  $self->{A_AID} = AmiGO::Aid->new();
  $self->{AEJ_SOLR} = AmiGO::External::JSON::SolrDocument->new();
  $self->{AWST_DOC} = undef;

  bless $self, $class;
  return $self;
}



1;
