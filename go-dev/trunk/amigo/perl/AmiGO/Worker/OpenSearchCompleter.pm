=head1 AmiGO::Worker::OpenSearchCompleter

See:
http://developer.mozilla.org/en/Supporting_search_suggestions_in_search_plugins

NOTE/TODO: It would be better to have a different backend, like
plucene or something...better ordering for one...

=cut

use utf8;
use strict;

package AmiGO::Worker::OpenSearchCompleter;

use base ("AmiGO");

use GOBO::DBIC::GODBModel::Schema;
use GOBO::DBIC::GODBModel::Query;
use JSON;


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  #$self->{SC_RESULTS} = {};

  ## We'll borrow SUCCESS and ERROR_MESSAGE from AmiGO.

  bless $self, $class;
  return $self;
}


=item complete

Args: string (query) and int (min size)
Return: JSON format Open Search string.

=cut
sub complete {

  my $self = shift;
  my $query = shift || '';
  my $min_size = shift || 3;
  my $limit = shift || 10;

  my $response_head = '["' . $query . '", ';
  my $response_tail = ']';

  ## Default answer.
  my $ret = $response_head . '[]' . $response_tail;

  ## Look only if we meet minimal requirements.
  if( $query && length($query) > $min_size ){

    my $qstr = '%' . $query . '%';

    my $term_search = GOBO::DBIC::GODBModel::Query->new({type=>'term'});
    my $results =
      $term_search->get_all_results({-limit=>$limit,
				     -or=>[
					   {'me.name' =>
					    {'like', $qstr}},
					   {'me.acc' =>
					    {'like', $qstr}},
					   {'term_synonym.term_synonym' =>
					    {'like', $qstr}},
					   {'term_synonym.acc_synonym' =>
					    {'like', $qstr}},
					  ]});

    my $comp = [];
    my $desc = [];
    my $qurl = [];
    foreach my $t (@$results){

      ## TODO: shorten name to something smaller...
      push @$comp, $t->name;
      push @$desc, $t->acc;
      push @$qurl, 'http://localhost/term?' . $t->acc;
    }

    ## Encode findings.
    my $json = JSON->new();
    my $json_comp = $json->to_json($comp);
    my $json_desc = $json->to_json($desc);

    ## TODO: Add qurl if useful...
    $ret = $response_head . $json_comp . ', ' . $json_desc . $response_tail;
  }

  return $ret;
}



1;
