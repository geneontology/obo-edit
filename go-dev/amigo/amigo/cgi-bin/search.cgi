#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

BEGIN {

		if (-f "config.pl") {
			require "config.pl";
		}

		if (defined($ENV{GO_ROOT})) {
		} elsif (-f "../cvs/go-dev/") {
		$ENV{GO_ROOT} = "../cvs/go-dev";
		}
}

use lib "$ENV{GO_ROOT}/go-perl";
use lib "$ENV{GO_ROOT}/go-db-perl";
use lib "$ENV{GO_ROOT}/amigo/perl";
use lib $ENV{GO_SVN_ROOT} . '/gobo-dbic';

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

#	valid params (other than persisting params)
#	termfields gpfields sppfields search_constraint exact_match
#	query idfile
#	action format

my $q = new CGI;
my $cgi_error = $q->cgi_error;
my $vars;

print STDERR "\n\n\nStarting search.cgi\nCGI: ".Dumper($q)."\n" if $verbose;

# Initial check for cgi errors
if ($cgi_error)
{	if ($cgi_error eq '413 Request entity too large')
	{	$cgi_error = 'The file you uploaded was too large. The maximum size for file uploads is '.$max_upload_size.' Kb.';
	}
	$vars->{error} = set_message(undef, 'fatal', $cgi_error);
	my $session = new GO::CGI::Session(-temp => 1);
	process_page_template({ error => $vars->{error}, page_title => 'Search Error' }, $session);
	exit;
}

my %params = $q->Vars;
my $ses_type = 'front';
my $query;

if ($params{action})
{	if ($params{action} eq 'advanced_query')
	{	#	turn on the advanced query mode
		$ses_type = 'advanced_query';
	}
	elsif ($params{action} eq 'new-search')
	{	$ses_type = 'search';
	#	A new search. If a file was uploaded,
	#	move the contents into the 'query' param
		my %q_hash;

		if ($params{search_query_file})
		{	print STDERR "Found a search_query_file!\n" if $verbose;
#			my $file = $params{search_query_file};
			my $file = $q->upload('search_query_file');
			if (-f $file)  # check for the file
			{
				while (<$file>) {
				  #	get rid of any tracts of whitespace
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
			}
			else
			{	print STDERR "File is not a plain file\n" if $verbose;
				$vars->{error} = set_message($vars->{error}, 'warning', 'not_plain_file');
			}
		}

		#	similarly if something was added to the query box
		if ($params{search_query})
		{	my @queries = split /(\n|\0)/, $params{search_query};
			foreach my $q (@queries)
			{	#	get rid of any tracts of whitespace
				$q =~ s/(\t|\s{2,})/ /g;
				$q =~ s/^\s*(\S+.*?)\s*$/$1/;
				#	decode any funny characters
				$q = decode_entities($q);
				$q_hash{$q}++ if $q =~ /\w/;

				## May be an incoming spammer--stop them.
				$vars->{error} =
				  set_message($vars->{error},
					      'fatal',
					      'URLs are not allowed')
				    if $q =~ /http\:\/\//gos;
			}
		}
		
		if (keys %q_hash)
		{	#	put these values into the 'query' param of the cgi
#			$q->param(-name=>'query', -'values'=>[ keys %q_hash ]);
			$query = [ keys %q_hash ];
		}
		else
		{	#	no valid query found! return an error
			print STDERR "No query found!\n" if $verbose;
			$vars->{error} = set_message($vars->{error}, 'fatal', 'no_valid_query');
		}
	}
}

#	if there's a query present, set the ses_type to search
if ($params{query} && $ses_type eq 'front')
{	$ses_type = 'search';
	#	convert $params{query} into a list
	$query = [ split(/\0|\n/, $params{query}) ];
}


## Check to see if the total query size is larger than we might want.
if ( $query && @$query &&
     scalar(@$query) > $max_number_queries ){
  $vars->{error} = set_message($vars->{error}, 'fatal', 'AmiGO searches are limited to '.$max_number_queries.' queries. Please go back and retry with a shorter list; remaining queries will have to be processed in a separate search.');
}


#	check the search constraint is valid
if ($params{search_constraint})
{	if (!grep { $params{search_constraint} } @valid_search_constraints)
	{	#	invalid search constraint
		print STDERR "search constraint: ".$params{search_constraint}."\n" if $verbose;
		$vars->{error} = set_message($vars->{error}, 'fatal', 'unknown_search_type', $params{search_constraint});
	}
}



#	die if there's a fatal message
if ($vars->{error}{fatal})
{	my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>'amigo_message', -temp => 1);
	process_page_template({ error => $vars->{error}, page_title => 'Search Error' }, $session, 'amigo_message');
	exit;
}

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);
$session->save_session;

if ($ses_type eq 'front' || $ses_type eq 'advanced_query')
{	#	nothing to do for these pages
	$vars->{search_constraint} = $params{search_constraint} || $session->get_param('search_constraint');
	
	if ($ses_type eq 'advanced_query')
	{	#	set up the options for the page
		$vars->{searchfields}{$_.'fields'} = $session->set_option($_.'fields') foreach @valid_search_constraints;
		$vars->{max_upload_size} = $max_upload_size;
	}
	process_page_template($vars, $session);
	exit;
}

#	Performing a search.
#	Create the search object
my $search = new GO::CGI::Search;

#	Get the various values for the search ready
my $option_h;

foreach ('exact_match', 'page') #, 'format') #format not yet in use
{	$option_h->{$_} = $params{$_} if $params{$_};
}

if ($params{action})
{	if ($params{action} eq 'new-search' || $params{action} eq 'filter' || $params{action} eq 'reset-filters')
	{	$option_h->{action} = 'search';
	}
	else
	{	$option_h->{action} = $params{action};
	}

	if ($params{action} eq 'sort')
	{	$option_h->{page} = 1; # reset to the first page if we're sorting
	}
}

if (($params{action} && $params{action} eq 'sort')
	|| $params{page}
	|| $params{page_size} && $params{page_size} eq 'all')
#	|| $params{'format'}) # not yet in use
{	#	this may well be a cached query. Load up the cache.
	print STDERR "Loading the cache...\n" if $verbose;
	my $cache = $session->load_cached_results($params{search_constraint}."_search");

	#	check the search parameters are the same as those in the cached query
	if ($cache && $cache->{query} && $cache->{result_list})
	{	#print STDERR "CACHE: query from cache: ".Dumper($cache->{query})."CACHE: params{query}: ".Dumper($query)."\n" if $verbose;
		if ( join("\n", sort @{$cache->{query}{input}}) eq join("\n", sort @$query) )
		{	#print STDERR "Cached query matches this query. Hurrah!\n" if $verbose;
			$option_h->{cache} = $cache;
		}
	}
}

#	set the page size
$option_h->{page_size} = $params{page_size} || $session->get_saved_param('page_size');

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

#print STDERR (gettimeofday() - $t0).": new search method done\n" if $verbose;

## Process the results
#print STDERR "results: ".Dumper($search->{results})."\n" if $verbose;
$vars->{n_results} = $search->n_results || 0;

#	if we only have a single result, we can create a redirect and exit.
if ($search->n_results == 1 && $search->get_param('single_result'))
{	my $result = $search->get_param('single_result');
	print "Location: ".$session->get_param('cgi_url')."/".$result->{search_constraint}."-details.cgi?".$result->{search_constraint}."=".$result->{id}."&session_id=".$session->id."\n\n";
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

print STDERR "Search fields: ".Dumper($search->get_param('ordered_search_fields'))."\n" if $verbose;
print STDERR "get query param: ".Dumper($search->get_query_param)."\n" if $verbose;

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

	print STDERR "querylist: ".Dumper($vars->{querylist})."\n" if $verbose;
	print STDERR "query URL: ".Dumper($vars->{queryurl})."\n" if $verbose;

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

		print STDERR "sort criterion: ".$vars->{sort_criterion}."\n" if $verbose;
	
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
process_page_template($vars, $session);
exit;
