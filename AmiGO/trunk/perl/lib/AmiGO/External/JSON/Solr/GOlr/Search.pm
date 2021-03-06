=head1 AmiGO::External::JSON::Solr::GOlr::Search

...

=cut

package AmiGO::External::JSON::Solr::GOlr::Search;
use base ("AmiGO::External::JSON::Solr::GOlr");

use utf8;
use strict;
use Data::Dumper;


=item new

#

=cut
sub new {

  ## Pass the buck back for getting a sensible default.
  my $class = shift;
  my $target = shift || undef;
  my $self = $class->SUPER::new($target);

  bless $self, $class;
  return $self;
}


=item smart_query

Arguments: simple query string, a golr class id str, and (optional) page num.
Return: true or false on minimal success

edismax built through our config hash

mostly runs off of Solr::query

=cut
sub smart_query {

  my $self = shift;
  my $qstr = shift || die "smart_query requires a q";
  my $gc_id = shift || die "smart_query requires a golr conf class id";
  my $page = shift || 1;


  $self->{AEJSGS_GOLR_CLASS_ID} = $gc_id;
  # ## Paging will be part of our world, so get ready.
  # $self->_ready_paging();

  ## TODO: Manipulate the config to get the hash.
  $self->kvetch("query with: " . $qstr);
  $self->kvetch("looking in: " . $gc_id);
  my $gconf = $self->golr_configuration();
  #$self->kvetch("conf: " . Dumper($gconf));

  ## 
  my $retval = 0;
  if( ! defined $gconf->{$gc_id} ){
    $self->kvetch("rotten document conf id: " . $gc_id);
  }else{

    ## Grab the main nutrients. Let's assumed that nobody screwed up
    ## the format. Now we need to break it down, see if there are any
    ## searchable fields to use, transfer to them, and reassemble.
    my $dfab = $self->golr_class_weights($gc_id, 'boost');
    #my @fields = split /\s+/, $dfab;
    my $fixed_boosts = [];
    #my $fields_to_search = [];
    my $search_ext = $self->golr_class_searchable_extension($gc_id);
    foreach my $field (keys %{$dfab}){
      my $boost = $dfab->{$field};

      ## TODO: Check to see if the field is searchable, and if it is,
      ## add the extension.
      if( $self->golr_class_field_searchable_p($gc_id, $field) ){
	$field = $field . $search_ext;
      }

      ## 
      #push @$fields_to_search, $field;
      push @$fixed_boosts, $field . '^' . $boost;
    }
    my $final_dfab_str = join ' ', @$fixed_boosts;

    ## Fold what we have into the hash.
    $self->{AEJS_BASE_HASH}{qf} = $final_dfab_str;
    $self->{AEJS_BASE_HASH}{defType} = 'edismax';
    $self->{AEJS_BASE_HASH}{fq} =
      'document_category:"' . $self->golr_class_document_category($gc_id) . '"';
    $self->{AEJS_BASE_HASH}{hl} = 'true';
    $self->{AEJS_BASE_HASH}{'hl.simple.pre'} = '<em class="hilite">';
    #$self->{AEJS_BASE_HASH}{'hl.simple.post'} = '</em>';
    #$self->{AEJS_BASE_HASH}{'hl.fl'} = '*';
    $self->{AEJS_BASE_HASH}{q} = $qstr;

    ## Calculate any necessary paging.
    my $nrows = $self->rows_requested() || 10;
    $self->{AEJS_BASE_HASH}{start} = ($nrows * $page) - $nrows;

    ## Call the main engine.
    $retval = $self->query();
  }

  return $retval;
}


=item more_p

Return: 0 or 1

Returns whether or not an additional "paging" would return more
results with the current start/rows settings.

=cut
sub more_p {

  my $self = shift;
  my $retval = 1;

  ## Make sure we got something.
  my $num_rows = $self->rows_requested() || 10;
  my $current_returned = $self->count() || 10;
  if( $num_rows > $current_returned ){
    $retval = 0;
  }

  return $retval;
}


=item range_high

Return: undef or int

Returns the highest index of items returned with the current
start/rows settings.

=cut
sub range_high {

  my $self = shift;
  my $curr_page = shift || die 'we need to know the current "page" for this';
  my $retval = undef;

  ## Make sure we got something.
  my $rows_req = $self->rows_requested();
  my $current_returned = $self->count();

  if( defined $rows_req &&
      defined $current_returned ){
    $retval = ($rows_req * $curr_page) - ($rows_req - $current_returned);
  }

  return $retval;
}


=item range_low

Return: undef or int

Returns the lowest index of items returned with the current
start/rows settings.

=cut
sub range_low {

  my $self = shift;
  my $curr_page = shift || die 'we need to know the current "page" for this';
  my $retval = undef;

  ## Make sure we got something.
  my $rows_req = $self->rows_requested();
  my $current_returned = $self->count();

  if( defined $rows_req &&
      defined $current_returned ){
    $retval = ($rows_req * ($curr_page - 1)) + 1;
  }

  return $retval;
}

=item highlighting

Return: "highlighting" data structure as a hash ref or empty, keyed by id

The highlighting found during last query.
TODO: I'm only taking the first highlight here.

=cut
sub highlighting {

  my $self = shift;
  my $worked_highlighting = {};

  ## Make sure we got something.
  my $raw_highlighting = {};
  if( $self->{AEJS_RESPONSE} &&
      $self->{AEJS_RESPONSE}{highlighting} ){
    $raw_highlighting = $self->{AEJS_RESPONSE}{highlighting};
    #$self->kvetch('got raw highlighting: ' . Dumper($raw_highlighting));
  }

  ## And take care of highlighting; maybe this should be a worker?
  my $ext =
    $self->golr_class_searchable_extension($self->{AEJSGS_GOLR_CLASS_ID});
  #$self->kvetch('ext: ' . $ext);
  foreach my $hid (keys %{$raw_highlighting}){

    ## Ensure that is in our worked hash.
    if( ! defined $worked_highlighting->{$hid} ){
      $worked_highlighting->{$hid} = {};
    }

    ## Cycle through the fields under the id.
    foreach my $raw_fid (keys %{$raw_highlighting->{$hid}} ){

      ## Instead of cycling through the available highlights and
      ## merging/do something with them, just take the first for now.
      my $hrows = $raw_highlighting->{$hid}{$raw_fid};
      if( defined $hrows && length(@{$hrows}) ){

	#$self->kvetch('l@: ' . $raw_fid);

	## Try and remove the searchable extension from the results.
	my $fid = $raw_fid;
	if( $raw_fid =~ /(.*)$ext$/ ){
	  $fid = $1;
	}

	# ## Ensure that is in our worked hash.
	# if( ! defined $worked_highlighting->{$hid}{$fid} ){
	#   $worked_highlighting->{$hid}{$fid} = {};
	# }
	#$worked_highlighting->{$hid}{$fid} = $$hrows->[0];

	## Since we're only taking one for now:
	$worked_highlighting->{$hid}{$fid} = $hrows->[0];
      }
    }
  }

  return $worked_highlighting;
}



1;
