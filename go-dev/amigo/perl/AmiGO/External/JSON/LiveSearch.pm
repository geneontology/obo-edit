=head1 AmiGO::External::JSON::LiveSearch

Needs a subclass addition to get the url correct...or do it raw at
creation time.

=cut

package AmiGO::External::JSON::LiveSearch;

use base ("AmiGO::External::JSON");


=item new

#

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();
  my $type = shift || die "Need a live search type: $!";

  $self->{JSLS_BASE_HASH} =
    {
     query => '',
     count => 10,
     index => 1,
    };

  $self->{JSLS_BASE_URL} =
    $self->amigo_env('AMIGO_SERVICE_URL') . '?mode=live_search_' . $type;

  bless $self, $class;
  return $self;
}


=item query

Args: hash ref (see service/js docs)
Return: perl hash structure (see service/js docs) or undef

=cut
sub query {

  my $self = shift;
  my $in_hash = shift || {};
  my $retval = undef;

  ## Merge incoming with default template.
  my $final_hash = $self->merge($self->{JSLS_BASE_HASH}, $in_hash);

  ## Create URL
  my $qstr = $self->hash_to_query_string($final_hash);
  my $url = $self->{JSLS_BASE_URL};
  $url .= '&' . $qstr if $qstr;

  ## Make query against resource and try to perlify it.
  $self->get_external_data($url);
  my $final_blob = $self->try();

  ## Make sure we got something.
  if( ! $self->empty_hash_p($final_blob) ){
    $retval = $final_blob;
  }

  return $retval;
}



1;
