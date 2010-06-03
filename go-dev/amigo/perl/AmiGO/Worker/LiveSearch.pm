=head1 AmiGO::Worker::LiveSearch

A generalized engine for getting results out of a luigi index.
The base for our index-based searchers.

use AmiGO::Worker::LiveSearch;
$ls = AmiGO::Worker::LiveSearch->new({index => 'gene_product', search_fields=>['full_name', 'symbol']});
print Dumper($ls->query({query=>'cho*', index=>0, count=>10, filters=>{}, return_fields=>['full_name', 'symbol']}))

=cut

package AmiGO::Worker::LiveSearch;
use base ("AmiGO");

use Lucene;
use AmiGO::Lucene;
use Data::Dumper;
use Time::HiRes qw(gettimeofday tv_interval);
use Data::Page;
#use JSON;
#use Search::Tools::HiLiter;
use HTML::Highlight;


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();
  my $in_args = shift || {};

  ## Arg handling.
  my $final_args =
    $self->merge({
		  search_fields => [],
		 },
		$in_args);
  my $index_type = $final_args->{index} || die "index type not found: $!";

  ## 
  $self->{LS_SEARCH_FIELDS} = $final_args->{search_fields};

  ## Default spot.
  $self->{LS_SPOT} = $self->amigo_env('INDEX_DIR') .'/lucene/'.  $index_type;

  ## Make a pager.
  $self->{LS_PAGER} = Data::Page->new();

  bless $self, $class;
  return $self;
}


=item query

Perform a lexical query with a json string result.

=cut
sub query {

  my $self = shift;
  my $in_args = shift || {};

  ## 
  my $final_args =
    $self->merge({
		  query => '',
		  index => 0,
		  count => 0,
		  filters => {},
		  return_fields => [],
		 },
		 $in_args);
  my $qstr = $final_args->{query};
  my $index = $final_args->{index};
  my $count = $final_args->{count};
  my $filters = $final_args->{filters};
  my $return_fields = $final_args->{return_fields};

  #$self->kvetch("base query: " . $qstr);
  $self->kvetch("final_args: " . Dumper($final_args));

  ## Start with no results, override of we find anything. BTW, we're
  ## going to ignore anything that has a query less than three
  ## characters.
  my $complete = {
		  meta => {total => 0, first => 0, last => 0, step=> 0},
		  hits => [],
		 };
  if( $qstr && length($qstr) >= 3 ){

    my $analyzer = new Lucene::Analysis::Standard::StandardAnalyzer();
    my $store = Lucene::Store::FSDirectory->getDirectory($self->{LS_SPOT}, 0);
    my $searcher = new Lucene::Search::IndexSearcher($store);

    ## Query across the key fields, and make sure that the ID colons are
    ## escaped properly going in.
    my $lucy = AmiGO::Lucene->new();
    my $general_parser =
      new Lucene::MultiFieldQueryParser($self->{LS_SEARCH_FIELDS},
					$analyzer);
    my $general_query = $general_parser->parse($lucy->fix_query($qstr));

    ## Reality check.
    $self->kvetch("search query: " . $qstr);
    $self->kvetch("fixed query: " . $lucy->fix_query($qstr));
    $self->kvetch("stripped query: " . $lucy->strip_query($qstr));
    $self->kvetch("parsed query (pre): " . $general_query->toString());

    ## TODO: add/remove constraints to query from the additional
    ## arguments.
    ## We'll start by just being positive and see if that is enough os
    ## a filter.

    ## Get the filters and add them to the query string.
    my @filter_qstr_cache = ();
    my $filter_fields_cache = ();
    foreach my $filter_name (keys %$filters){

      push @filter_fields_cache, $filter_name;

      ##
      my $filter_rref = $filters->{$filter_name};
      if( scalar @$filter_rref ){

	##
	my $filter_qstr =
	  $self->_array_ref_to_field_query_string($filter_name, $filter_rref);
	push @filter_qstr_cache, $filter_qstr if $filter_qstr;
      }
    }
    my $filter_qstr = join ' AND ', @filter_qstr_cache;

    $self->kvetch("raw filter string: " . $filter_qstr);

    ## If we have a filter_string, define the filter.
    my $filter = undef;
    if( $filter_qstr ){

      my $filter_parser =
	new Lucene::MultiFieldQueryParser(\@filter_fields_cache, $analyzer);
      #my $filter_query = $filter_parser->parse($lucy->fix_query($filter_qstr));
      my $filter_query = $filter_parser->parse($filter_qstr);
      # $self->kvetch("query: raw filter_fields_cache: " .
      # 		    Dumper(\@filter_fields_cache));
      # $self->kvetch("query: raw analyzer: " . Dumper($analyzer));
      # $self->kvetch("query: raw filter_parser: " . Dumper($filter_parser));
      # $self->kvetch("query: raw filter_query: " . Dumper($filter_query));
      # $self->kvetch("query: raw filter_qstr: " . Dumper($filter_qstr));
      $self->kvetch("query: parsed filter query: " . $filter_query->toString());

      $filter = new Lucene::Search::QueryFilter($filter_query);

      undef $filter_query;
      undef $filter_parser;
    }
    $self->kvetch("parsed query (post): " . $general_query->toString());

    ## Query index and get results. Filter them too.
    #my $hits = $searcher->search($general_query);
    my $hits = undef;
    eval{
      if( defined $filter ){
	$self->kvetch("running filtered query");
	$hits = $searcher->search($general_query, $filter);
      }else{
	$self->kvetch("running straight query");
	$hits = $searcher->search($general_query);
      }
    };
    if($@){
      ## BUG?: too many returned? See commentary in
      ## experimental/completion.
      my $badness = "LiveSearch stopped due to too many results. Please try and further limit the search.\n";
      $self->kvetch($badness);
      die $badness;
    }

    ## Get the total number of results, and let the pager know.
    my $num_hits = $hits->length() ;
    $self->kvetch("num hits: " . $num_hits);
    $self->{LS_PAGER}->total_entries($num_hits);
    $self->{LS_PAGER}->entries_per_page($count);
    $self->{LS_PAGER}->current_page($index);

    ## Split on space not surrounded by quotes after doing a strip of
    ## some lucene-specific artifacts. Proceed to remove undefs and
    ## empty sting from the list--the results being a final list of
    ## good words to highlight.
    my @start_words_to_hilite = split(/"([^"]*)"| /, $lucy->strip_query($qstr));
    my @final_words_to_hilite = ();
    foreach my $w (@start_words_to_hilite){
      push @final_words_to_hilite, $w if defined $w && $w;
    }
    $self->kvetch("hiliter words: " . Dumper(\@final_words_to_hilite));

    ## Create highlighter.
    my $hiliter = new HTML::Highlight
      (
       words => \@final_words_to_hilite,
       wildcards => ['%', '*', '~', '?'],
       # colors => [],
       czech_language => 0,
       debug => 0
      );

    #$self->source();
    #$self->gptype();

    ## Get fields and ranking score for each hit. Note that Lucene
    ## wants indexing to start at zero, so the loop is of by one and
    ## wrapped in a test to make sure we got something.
    my $first = $self->{LS_PAGER}->first;
    my $last = $self->{LS_PAGER}->last;
    my $results = [];
    if( $first > 0 ){
      for( my $i = $first -1; $i < $last; $i++ ){

	$self->kvetch(($first-1) . ' ... ' . ($i) . ' ... ' . ($last-1));

	my $score = $hits->score($i);
	my $doc = $hits->doc($i);

	#$self->kvetch('hit: ' . $name . ' (' . $acc . ')');
	# 	## Switch between gps and terms for links.
	# 	my $qurl = $self->get_interlink({mode=>'gp-details',
	# 					 arg=>{gp=>$acc}});
	my $result =
	  {
	   'score' => sprintf("%.2f", $score) * 100,
	   #'link' => $qurl,
	  };

	## Iterate over the return fields for pulling stuff out of the
	## doc.
	foreach my $ret_field (@$return_fields){

	  my $field_content = $doc->get($ret_field);
	  $result->{$ret_field} = $field_content;
	  $result->{'hilite_' . $ret_field} =
	    $hiliter->highlight($lucy->strip_query($field_content));
	}

	push @$results, $result;
      }
    }

    ## Free memory and close everything.
    undef $hits;
    undef $general_query;
    undef $general_parser;
    undef $filter;
    undef $analyzer;
    $searcher->close();
    #undef $fsdir;
    undef $searcher;
    $store->close;
    undef $store;

    $complete = {
		 meta => {
			  total => $self->{LS_PAGER}->total_entries,
			  first => $self->{LS_PAGER}->first,
			  last => $self->{LS_PAGER}->last,
			 },
		 hits => $results,
		};
  }

  #return JSON::encode_json($complete);
  return $complete;
}


##
sub _array_ref_to_field_query_string {

  my $self = shift;
  my $field = shift || 'ERROR';
  my $array_ref = shift || [];

  $self->kvetch('filter to make check: ' . $field);

  my $str = '';
  if( length(@$array_ref) > 0 ){
    my @filter_cache = ();
    foreach my $t (@$array_ref){
      if( $t ne '' && $t ne 'all' ){ # TODO/BUG: what is this again?
	push @filter_cache, $field . ':' . $t;
	$self->kvetch('add positive filter to ' . $field . ': ' . $t);
      }
    }
    $str = join(' ', @filter_cache);
  }
  return $str;
}



1;
