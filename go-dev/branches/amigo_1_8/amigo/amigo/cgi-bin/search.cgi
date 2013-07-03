#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

BEGIN { require "config.pl" if -f "config.pl" ; }
use lib "$ENV{GO_DEV_ROOT}/go-perl";
use lib "$ENV{GO_DEV_ROOT}/go-db-perl";
use lib "$ENV{GO_DEV_ROOT}/amigo/perl";
use lib $ENV{GOBO_ROOT};

use strict;
use FileHandle;
use GO::CGI::Search qw(:std);
use GO::CGI::Session;
use GO::CGI::NameMunger;
use GO::CGI::Utilities qw(:std);
#use GO::Template::Template;
use HTML::Entities;

use Data::Dumper;
$Data::Dumper::Indent = 1;
use Time::HiRes qw(gettimeofday);

my @valid_search_constraints = qw(term gp); # spp);
my $verbose = get_environment_param('verbose'); # my $verbose = 1;
my $max_upload_size = 10;  		## 10K uploads max.
my $max_number_queries = 50;		## limit to 50 queries per search.
use CGI;
$CGI::POST_MAX = 1024 * $max_upload_size;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

## Some new stuff to try...
#use AmiGO;
#my $core = AmiGO->new();
use AmiGO::Aid::ReferenceGenome;
use AmiGO::ReferenceGenome;
my $core = AmiGO::Aid::ReferenceGenome->new();

#
# Set up the relevant objects.
#

# valid params (other than persisting params)
# termfields gpfields sppfields search_constraint exact_match
# query idfile
# action format

my $q = new CGI;
my $cgi_error = $q->cgi_error;
my $vars;

$core->kvetch("Starting errors: ". $cgi_error);
$core->kvetch("Starting search.cgi, CGI: ".Dumper($q));

# Initial check for cgi errors
if( $cgi_error ){
  if ($cgi_error eq '413 Request entity too large'){
    $cgi_error = 'The file you uploaded was too large. The maximum size for file uploads is '.$max_upload_size.' Kb.';
  }
  $vars->{error} = set_message(undef, 'fatal', $cgi_error);
  $core->kvetch("In error 1: ". $cgi_error);
  my $session = new GO::CGI::Session(-temp => 1);
  $core->kvetch("In error 2: ". $cgi_error);
  process_page_template({ error => $vars->{error}, page_title => 'Search Error' }, $session);
  exit;
}

my %params = $q->Vars;
my $ses_type = 'front';
my $query;

if ($params{action}){
  if ($params{action} eq 'advanced_query'){ # turn on the advanced query mode
    $ses_type = 'advanced_query';
  }elsif ($params{action} eq 'new-search'){
    $ses_type = 'search';
    # A new search. If a file was uploaded,
    # move the contents into the 'query' param
    my %q_hash;

    if ($params{search_query_file}){
      $core->kvetch("Found a search_query_file!");
      # my $file = $params{search_query_file};
      my $file = $q->upload('search_query_file');
      if (-f $file) { # check for the file
	while (<$file>) {
	  # get rid of any tracts of whitespace
	  s/(\t|\s{2,})/ /g;
	  s/^\s*(\S+.*?)\s*$/$1/;
	  if (/\w/){
	    if (scalar keys %q_hash < $max_number_queries){
	      $q_hash{$_}++;
	    }else{
	      $vars->{error} = set_message($vars->{error}, 'warning', 'AmiGO searches are limited to '.$max_number_queries.' queries; remaining queries will have to be processed in a separate search.');
	      last;
	    }
	  }

	  ## May be an incoming spammer--stop them.
	  $vars->{error} =
	    set_message($vars->{error},
			'fatal',
			'URLs are not allowed')
	      if $q =~ /http\:\/\//gos;
	}
      }else{
	$core->kvetch("File is not a plain file");
	$vars->{error} = set_message($vars->{error}, 'warning',
				     'not_plain_file');
      }
    }

    # similarly if something was added to the query box
    if ($params{search_query}){
      my @queries = split /(\n|\0)/, $params{search_query};
      foreach my $q (@queries){
	# get rid of any tracts of whitespace
	$q =~ s/(\t|\s{2,})/ /g;
	$q =~ s/^\s*(\S+.*?)\s*$/$1/;
	# decode any funny characters
	$q = decode_entities($q);
	$q_hash{$q}++ if $q =~ /\w/;

	## May be an incoming spammer--stop them.
	$vars->{error} =
	  set_message($vars->{error},
		      'fatal',
		      'URLs are not allowed')
	    if $q =~ /http\:\/\//gos;

	$core->kvetch("cleaned search_query: " . $q);
      }
    }

    if (keys %q_hash){	# put these values into the 'query' param of the cgi
      #	$q->param(-name=>'query', -'values'=>[ keys %q_hash ]);
      $query = [ keys %q_hash ];
    }else{ # no valid query found! return an error
      $core->kvetch("No query found!");
      $vars->{error} = set_message($vars->{error}, 'fatal', 'no_valid_query');
    }
  }
}

# if there's a query present, set the ses_type to search
if ($params{query} && $ses_type eq 'front'){
  $ses_type = 'search';
  # convert $params{query} into a list
  $query = [ split(/\0|\n/, $params{query}) ];
}

$core->kvetch('our $query: ' . Dumper($query));

## Check to see if the total query size is larger than we might want.
if ( $query && @$query &&
     scalar(@$query) > $max_number_queries ){
  $vars->{error} = set_message($vars->{error}, 'fatal', 'AmiGO searches are limited to '.$max_number_queries.' queries. Please go back and retry with a shorter list; remaining queries will have to be processed in a separate search.');
}

# check the search constraint is valid
if ($params{search_constraint}){
  # invalid search constraint
  if (!grep { $params{search_constraint} } @valid_search_constraints){
    $core->kvetch("search constraint: ".$params{search_constraint}."");
    $vars->{error} = set_message($vars->{error}, 'fatal',
				 'unknown_search_type',
				 $params{search_constraint});
  }
}

# die if there's a fatal message
if ($vars->{error}{fatal}){
  my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>'amigo_message',
				     -temp => 1);
  process_page_template({ error => $vars->{error},
			  page_title => 'Search Error' },
			$session, 'amigo_message');
  exit;
}

my $session = new GO::CGI::Session('-q'=>$q,
				   -ses_type=>$ses_type, -read_only=>1);
$session->save_session;

if ($ses_type eq 'front' || $ses_type eq 'advanced_query'){
  # nothing to do for these pages
  $vars->{search_constraint} =
    $params{search_constraint} || $session->get_param('search_constraint');

  if ($ses_type eq 'advanced_query'){
    # set up the options for the page
    $vars->{searchfields}{$_.'fields'} = $session->set_option($_.'fields')
      foreach @valid_search_constraints;
    $vars->{max_upload_size} = $max_upload_size;
  }

  ## Add AmiGO 2 link.
  $vars->{EOL} = int($core->amigo_env('AMIGO_EOL')) || 0;
  my $a2l = $core->get_interlink({mode => 'amigo2-search'});
  $vars->{'AMIGO2_LINK'} = $a2l;

  process_page_template($vars, $session);
  exit;
}

# Performing a search.
# Create the search object
my $search = new GO::CGI::Search;

# Get the various values for the search ready
my $option_h;

foreach ('exact_match', 'page'){ #, 'format') #format not yet in use
  $option_h->{$_} = $params{$_} if $params{$_};
}

if ($params{action}){
  if ($params{action} eq 'new-search' ||
      $params{action} eq 'filter' ||
      $params{action} eq 'reset-filters'){
    $option_h->{action} = 'search';
  }else{
    $option_h->{action} = $params{action};
  }

  if ($params{action} eq 'sort'){
    $option_h->{page} = 1; # reset to the first page if we're sorting
  }
}

if (($params{action} && $params{action} eq 'sort')
    || $params{page}
    || $params{page_size} && $params{page_size} eq 'all'){
  #	|| $params{'format'}) # not yet in use
  #	this may well be a cached query. Load up the cache.
  $core->kvetch("Loading the cache...");
  my $cache = $session->load_cached_results($params{search_constraint}.
					    "_search");

  # check the search parameters are the same as those in the cached query
  if ($cache && $cache->{query} && $cache->{result_list}){
    #$core->kvetch("CACHE: query from cache: ".Dumper($cache->{query})."CACHE: params{query}: ".Dumper($query)."");
    if ( join("\n", sort @{$cache->{query}{input}}) eq join("\n", sort @$query) ){
      # $core->kvetch("Cached query matches this query. Hurrah!");
      $option_h->{cache} = $cache;
    }
  }
}

# set the page size
$option_h->{page_size} = $params{page_size} ||
  $session->get_saved_param('page_size');

#	new stuff
#	if there's a format specified OR the page size is 'all', don't use paging
#	(i.e. using paging if there is no format and page_size isn't 'all')
if ($option_h->{page_size} ne 'all') #  && !$option_h->{'format'}
{	$option_h->{use_paging} = 1;
	$option_h->{chunk_by} = 'LIST_ITEM';
}

#	set the maximums for html and downloads (DLs not yet implemented)
foreach ('max_results_html', 'max_results_download')
{	$option_h->{$_} = $session->get_saved_param($_);
}

#	set the search constraint, fields and sorting criteria
$option_h->{search_constraint} = $params{search_constraint} || $session->get_param('search_constraint');

foreach ($option_h->{search_constraint})
{	$option_h->{$_.'fields'} = [ split(/,|\0/, $params{$_.'fields'}) ] if $params{$_.'fields'};
	$option_h->{$_.'sort'} = $params{$_.'sort'} || $session->get_param($_.'sort');
}

#	set whether or not we can get the GP/term counts
for ('gp', 'term')
{	$vars->{"show_".$_."_counts"} = $option_h->{"show_".$_."_counts"} = $session->show_counts($_);
}
$vars->{gp_count_ok} = $option_h->{gp_count_ok} = $session->gp_count_ok;
$option_h->{ontology_list} = $session->get_ontology_list;

#my $t0 = gettimeofday();
## Perform the search
my $data = $search->getResultList({
	apph => $session->apph,
	query => $query,
	option_h => $option_h,
	error => $vars->{error},
});

#$core->kvetch((gettimeofday() - $t0).": new search method done");

## Process the results
#$core->kvetch("results: ".Dumper($search->{results})."");
$vars->{n_results} = $search->n_results || 0;

## BUG: "Smarts". Create a redirect without AmiGO.pm on single results and exit. Added a special case to deal with new location for term_details.
if ($search->n_results == 1 && $search->get_param('single_result')){

  my $result = $search->get_param('single_result');

  ## New term system.
  if( $result->{search_constraint} eq 'term' ){
    my $turl = $core->get_interlink({
  				     mode => 'term_details',
  				     #optional => { public => 1 },
  				     arg => {
  					     acc => $result->{id},
  					     session => $session->id
  					    }
  				    });
    $core->kvetch("Single result--trying to forward to: " . $turl);
    print "Location: " . $turl . "\n\n";
  }else{
    print "Location: ".$session->get_param('cgi_url')."/".$result->{search_constraint}."-details.cgi?".$result->{search_constraint}."=".$result->{id}."&session_id=".$session->id."\n\n";
  }
  exit;
}


$vars->{cgi} = 'search';
$vars->{error} = $search->get_msg;
$vars->{search} = $search;
$vars->{filters_on} = 1 if keys %{$session->apph->filters};

#	this is needed for the query summary
$vars->{search_constraint} = $search->get_param('search_constraint');
$vars->{search_constraint_name} = GO::CGI::NameMunger::get_human_name_fn($vars->{search_constraint});
$session->ses_type($vars->{search_constraint}."_search");

$core->kvetch("Search fields: ".Dumper($search->get_param('ordered_search_fields'))."");
$core->kvetch("get query param: ".Dumper($search->get_query_param)."");

#	if we successfully found a query and were therefore able to perform a search...
if ($search->get_query_param)
{	$vars->{querylist} = [ map { join(" ", @$_) } @{$search->get_query_param} ];
	$vars->{query} = join(" ", $vars->{querylist}[0]);
	$vars->{querytext} = join(" or ", @{$vars->{querylist}});
	## Used to be: $vars->{queryurl} = CGI::escape(...); But it
	## sent encoded spaces to the search, which caused paging
	## problems. It looks like we're fine even without this
	## because of theescaping on the templates.
	$vars->{queryurl} =
	   join("&amp;query=",
			    map { encode_entities($_) }
			    @{$vars->{querylist}});

	$core->kvetch("querylist: ".Dumper($vars->{querylist})."");
	$core->kvetch("query URL: ".Dumper($vars->{queryurl})."");

	$vars->{search_fields} = [ map { GO::CGI::NameMunger::get_field_name_fn($_) } @{$search->get_param('ordered_search_fields')} ];

	if ($search->get_param('large_result_set'))
	{	$vars->{large_result_set} = $search->get_param('large_result_set');
	}

	if ($data)
	{	$vars->{data} = $data;
		$vars->{n_pages} = $search->get_param('n_pages') || 1;
		
		$vars->{page} = $params{page} || 1;
		$vars->{url_string} = "query=".$vars->{queryurl} ."&amp;search_constraint=".$vars->{search_constraint};
		$vars->{sort_crit_names} = {
			name => 'term name',
			acc => 'term accession',
			rel => 'relevance',
			term_type => 'ontology name',
			full_name => 'gene product name',
			symbol => 'gene product symbol',
			spp => 'species',
		};

		$vars->{sort_criterion} = $search->get_param(  $vars->{search_constraint}.'sort' ) || 'rel';

		$core->kvetch("sort criterion: ".$vars->{sort_criterion}."");
	
		#	disable the view all results link if the number of results is huuuuuge
		if ($search->n_results > $option_h->{max_results_html})
		{	$vars->{disable_view_all_results_link} = 1;
		}
	}

	#	cache the results if reqd
	if ($search->{cache_me} && $search->cache)
	{	$session->save_cached_results($search->cache);
	}

}
else
{	$vars->{page_title} = 'Search Error';
#	$session->ses_type('amigo_message');
}
if ($vars->{n_pages} && $vars->{n_pages} > 1)
{	$vars->{use_paging} = 1;
}

## New stuff for refgen.
$vars->{REFGEN_INFO} = {};
$vars->{REFGEN_INFO}{HAS_INFO_P} = 0;
if( $params{search_constraint} eq 'gp'){

  my $rg = AmiGO::ReferenceGenome->new();
  foreach my $gpo (@$data){

    my $acc = $gpo->gpxref;
    if( ! defined $vars->{REFGEN_INFO}{$acc} ){
      $vars->{REFGEN_INFO}{$acc} = {};
      $vars->{REFGEN_INFO}{$acc}{REFGEN_P} = 0;
    }
    if( defined( my $results = $rg->find_refgen_info({gene_product=>$acc})) ){

      $rg->kvetch('found a refgen ref for: ' . $acc);

      ## General flag.
      $vars->{REFGEN_INFO}{HAS_INFO_P}++;

      ## Specific info by acc.
      $vars->{REFGEN_INFO}{$acc}{REFGEN_P} = 1;
      $vars->{REFGEN_INFO}{$acc}{REFGEN_RHASHES} = $results;

      ## Collect the species info for these results.
      $vars->{REFGEN_INFO}{$acc}{SPECIES_INFO} = {};
      foreach my $set (@$results){
	my $others = $core->species_information($set->{other_species});
	$vars->{REFGEN_INFO}{$acc}{SPECIES_INFO}{$set->{id}} = $others;
	$core->kvetch($set->{id} . ' ' . $others);
      }
    }
  }
}

## Add AmiGO 2 link.
$vars->{EOL} = int($core->amigo_env('AMIGO_EOL')) || 0;
my $a2l = $core->get_interlink({mode => 'amigo2-search'});
$vars->{'AMIGO2_LINK'} = $a2l;

process_page_template($vars, $session);
exit;
