#!/usr/bin/perl -w
require 5.8.0;

print STDERR "\n\nStarting search.cgi\n";

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
use lib "$ENV{GO_ROOT}/new-amigo/perl";

use strict;
use FileHandle;
use GO::CGI::Search qw(:std);
use GO::CGI::Session;
use GO::CGI::NameMunger;
use GO::CGI::Utilities qw(:std);
use GO::Template::Template;
use HTML::Entities;

use Data::Dumper;
$Data::Dumper::Indent = 1;
use Time::HiRes qw(gettimeofday);

my $max_size = 100;		## 100K uploads max.
use CGI;
$CGI::POST_MAX = 1024 * $max_size;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

#
# Set up the relevant objects.
#

#	valid params (other than persisting params)
#	termfields gpfields sppfields search_constraint exact_match
#	query idfile
#	action format

my $q = new CGI;
my $error = $q->cgi_error;
my $msg_h;

print STDERR "cgi: ".Dumper($q)."\n";

if ($error)
{	if ($error eq '413 Request entity too large')
	{	$error .= '.<br>The maximum size for file uploads is '.$max_size.' Kb.';
	}
	$msg_h = set_message($msg_h, 'fatal', $error);
	my $session = new GO::CGI::Session(-temp => 1);
	process_page_template({ msg_h => $msg_h, page_title => 'Search error' }, $session, 'advanced_search');
	exit;
}

my %params = $q->Vars;
my $ses_type = 'front';
my @valid_search_constraints = qw(term gp); # spp);
my $vars;
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
		{	my $file = $params{search_query_file};
			print STDERR "Found a search_query_file!\n";
			while (<$file>) {
				#	get rid of any tracts of whitespace
				s/(\t|\s{2,})/ /g;
				s/^\s*(\S+.*?)\s*$/$1/;
				$q_hash{$_}++ if /\w/;
			}
		}
		#	similarly if something was added to the query box
		if ($params{search_query})
		{	my @queries = split /(\n|\0)/, $params{search_query};
			foreach my $q (@queries)
			{	#	get rid of any tracts of whitespace
				$q =~ s/(\t|\s{2,})/ /g;
				$q =~ s/^\s*(\S+.*?)\s*$/$1/;
				$q_hash{$q}++ if $q =~ /\w/;
			}
		}
		
		if (keys %q_hash)
		{	#	put these values into the 'query' param of the cgi
#			$q->param(-name=>'query', -'values'=>[ keys %q_hash ]);
			$query = [ keys %q_hash ];
		}
		else
		{	#	no valid query found! return an error
			print STDERR "No query found!\n";
			$msg_h = set_message($msg_h, 'fatal', 'no_valid_query');
		}
	}
}

#	if there's a query present, set the ses_type to search
if ($params{query} && $ses_type eq 'front')
{	$ses_type = 'search';
	#	convert $params{query} into a list
	$query = [ split(/\0|\n/, $params{query}) ];
}

#	check the search constraint is valid
if ($params{search_constraint})
{	if (!grep { $params{search_constraint} } @valid_search_constraints)
	{	#	invalid search constraint
		print STDERR "search constraint: ".$params{search_constraint}."\n";
		$msg_h = set_message($msg_h, 'fatal', 'unknown_search_type', $params{search_constraint});
	}
}

if ($msg_h->{fatal})
{	my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>'amigo_message', -temp => 1);
	process_page_template({ msg_h => $msg_h, page_title => 'Search error' }, $session, 'amigo_message');
	exit;
}

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);

if ($ses_type eq 'front' || $ses_type eq 'advanced_query')
{	#	nothing to do for these pages
	$vars->{search_constraint} = $params{search_constraint} || $session->get_param('search_constraint');
	
	if ($ses_type eq 'advanced_query')
	{	$vars->{searchfields}{$_.'fields'} = $session->set_option($_.'fields') foreach @valid_search_constraints;
	}
	process_page_template($vars, $session);
	exit;
}

#
# Perform the query
#

#	Do the search

#my $search = new GO::CGI::Search({ get_relevance => 0 });

my $search = new GO::CGI::Search;

#	Get the various values for the search ready
my $option_h;

$option_h->{search_constraint} = $params{search_constraint} || $session->get_param('search_constraint');

$session->ses_type($option_h->{search_constraint}."_search");

if (($params{action} && $params{action} eq 'sort')
	|| $params{page}
	|| $params{page_size} && $params{page_size} eq 'all'
	|| $params{'format'})
{	#	this may well be a cached query. Load up the cache.
	my $cache = $session->load_cached_results;

	#	check the query is the same as that in params
	if ($cache && $cache->{result_list} && $cache->{queryset})
	{	print STDERR "queryset from cache: ".Dumper($cache->{queryset})."params{query}: ".Dumper($query)."\n";
	#	if ($cache->{queryset} eq $query)
	#	{	
			$option_h->{cache} = $cache;
	#	}
	}
}

foreach ('exact_match', 'page', 'action') #, 'format')
{	$option_h->{$_} = $params{$_} if $params{$_};
}

foreach ($option_h->{search_constraint}.'sort', 'page_size')
{	$option_h->{$_} = $params{$_} || $session->get_param($_);
}

foreach (@valid_search_constraints)
{	$option_h->{$_.'fields'} = [ split(/,|\0/, $params{$_.'fields'}) ] if $params{$_.'fields'};
}

foreach ('gp_count_ok', 'show_gp_counts', 'show_term_counts')
{	$option_h->{$_} = $session->$_;
}
$option_h->{ontology_list} = $session->get_ontology_list;

my $t0 = gettimeofday();
my $data = $search->getResultList($session->apph, $query, $option_h);

print STDERR "results: ".Dumper($search->{results})."\n";

#print STDERR "Search object: ".Dumper($search)."\n";

#$session->search_sync($search);

#if (!$data)
#{	$vars->{msg_h} = $search->get_msg;
#	print STDERR "msg: ".Dumper($vars->{msg_h})."\n";
#	print STDERR "Errors!\n".Dumper($search->get_msg)."\n";
#	process_page_template($vars, $session);
#	exit;
#}

print STDERR (gettimeofday() - $t0).": new search method done\n";

#print STDERR "search object:\n".Dumper($search)."\n";
if ($search->{cache_me} && $search->cache)
{	$session->save_cached_results($search->cache);
}

#	print STDERR "session object:\n".Dumper($session)."\n";

if ($search->{results}{single_result})
{	print STDERR "data: ".Dumper($data);
	print "Location: ".$session->get_param('cgi_url')."/".$data->{search_constraint}."-details.cgi?".$data->{search_constraint}."=".$data->{id}."&session_id=".$session->id."\n\n";
	exit;
}
else
{
	$vars = {
#		data => $data,
		search => $search,
		search_constraint => $search->get_param('search_constraint'),
		cgi => 'search',
		search_fields => [ map { GO::CGI::NameMunger::get_field_name(1, $_) } @{$search->get_param('select_list')} ],
		n_results => $search->get_result_param('n_results') || 0,
	};

	if ($data)
	{	$vars->{data} = $data;
		$vars->{n_pages} = $search->get_result_param('n_pages') || 1;
		$vars->{page} = $params{page} || 1;
	}
#	else
#	{	$vars->{n_results} = 0;
#	}
	
	foreach ('exact_match', 'show_gp_counts', 'show_term_counts')
	{	$vars->{$_} = 1 if $search->get_param($_);
	}
	
	print STDERR "get query param: ".Dumper($search->get_query_param)."\n";
#	my $q = $search->get_query_param;
#	if ($q)
#	
	if ($search->get_query_param)
	{	$vars->{querylist} = [ map { join(" ", @$_) } @{$search->get_query_param} ];

	print STDERR "querylist: ".Dumper($vars->{querylist})."\n";

		$vars->{query} = join(" ", $vars->{querylist}[0]);
		$vars->{querytext} = join(" or ", @{$vars->{querylist}});
		$vars->{queryurl} = join("&amp;query=", map { encode_entities($_) } @{$vars->{querylist}});

		if ($search->get_result_param('large_result_set'))
		{	$vars->{large_result_set} = 1;
		}
	}
	$vars->{msg_h} = $search->get_msg;
}

$session->save_session;
process_page_template($vars, $session);
exit;
