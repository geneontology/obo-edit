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
if ($error)
{	my $session = new GO::CGI::Session('-q'=>$q, -read_only=>1);
	if ($error eq '413 Request entity too large')
	{	$error .= '.<br>The maximum size for file uploads is '.$max_size.' Kb.';
	}
	$msg_h = set_message($msg_h, 'fatal', $error);
	process_page_template({ msg_h => $msg_h}, $session, 'advanced_search');
	exit;
}

my %params = $q->Vars;
my $ses_type = 'front';
my $vars;
if ($params{search_constraint})
{	if ($params{search_constraint} eq 'term' || $params{search_constraint} eq 'gp' || $params{search_constraint} eq 'spp')
	{	$ses_type = $params{search_constraint}."_search";
	}
	else
	{	my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>'amigo_message');
		print STDERR "search constraint: ".$params{search_constraint}."\n";
		$msg_h = set_message($msg_h, 'fatal', 'unknown_search_type', $params{search_constraint});
		process_page_template({ msg_h => $msg_h }, $session, 'amigo_message');
		exit;
	}
}
elsif ($params{action} && $params{action} eq 'advanced_query')
{	#	turn on the advanced query mode
	$ses_type = 'advanced_query';
}

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);

#print STDERR "session:\n".Dumper($session)."\n";

#
# Perform the query
#

if ($ses_type eq 'front')
{	#	nothing to do for these pages
	process_page_template({ search_constraint => $session->get_param('search_constraint') }, $session);
	exit;
}
elsif ($ses_type eq 'advanced_query')
{	$session->adv_search_sync;
	$session->save_session;

	foreach my $f ('termfields', 'gpfields')
	{	$vars->{searchfields}{$f} = $session->set_option($f);
	}
	$vars->{search_constraint} = $session->get_param('search_constraint');
	process_page_template($vars, $session);
	exit;
}

#	Do the search
$session->search_sync;
$session->save_session;
my $search = new GO::CGI::Search($session->apph);

my $t0 = gettimeofday();
#	my $n = 0;
#	while ($n < 20)
#	{	







my $data = $search->getResultList(-query=>$session->get_current_param('query'), -session=>$session);

if (!$data)
{	my $msg = $search->get_msg;
	print STDERR "msg: ".Dumper($msg)."\n";
	print STDERR "Errors!\n".Dumper($search->get_msg)."\n";
	if ($msg)
	{	$vars->{msg_h} = $msg;
	}
	process_page_template($vars, $session);
	exit;
#		$n++;
}
print STDERR (gettimeofday() - $t0).": new search method done\n";

#print STDERR "search object:\n".Dumper($search)."\n";
if ($search->{cache_me} && $search->cache)
{	$session->save_cached_results($search->cache);
}

#	print STDERR "session object:\n".Dumper($session)."\n";

if ($search->{results}{single_result})
{	$vars = $data;
}
else
{	$vars = {
		data => $data,
		search => $search,
		n_results => $search->get_result_param('n_results'),
		n_pages => $search->get_result_param('n_pages') || 1,
#			search_fields => $search->{params}{select_list},
		search_constraint => $search->{params}{search_constraint},
		cgi => 'search',
	};
	#	prepare the data for the template
	@{$vars->{search_fields}} = map { $session->munger->get_field_name($_) } @{$search->get_param('select_list')} if $search->get_param('select_list');


	#	add the sort parameter and page_size

	if ($search->get_param('exact_match'))
	{	$vars->{exact_match} = 1;
	}
	
	if ($search->get_query_param)
	{	@{$vars->{querylist}} = map { join(" ", $_) } @{$search->get_query_param} ;
		$vars->{query} = $vars->{querylist}[0];
		$vars->{querytext} = join(" or ", @{$vars->{querylist}});
		$vars->{queryurl} = join("&amp;query=", map { encode_entities($_) } @{$vars->{querylist}});

		if ($search->get_result_param('large_result_set'))
		{	$vars->{large_result_set} = 1;
		}
	}
}

my $msg = $search->get_msg;
if ($msg)
{	$vars->{msg_h} = $msg;
}
$session->__save_session(1);
process_page_template($vars, $session);
exit;



sub initialize_search {
	my $self = shift;
	my ($query, $session, $arg_h) = rearrange([qw(query session arg_h)], @_);

###	Check we have a query
	if (!$query)
	{	$self->set_msg('fatal', 'missing_query');
		print STDERR "No query found\n";
#		$self->success(0);
		return;
	}
	else
	{	$self->set_param('query', $query);
	}

	my $sc = $arg_h->{search_constraint} || $session->get_param('search_constraint') || $global->{search_constraint};
	$self->set_param('search_constraint', $sc);

	foreach ($sc.'sort', 'exact_match', 'page_size', 'page') #, 'format')
	{	my $p = $arg_h->{$_} || $session->get_current_param($_);
		$self->set_param($_, $p) if $p;
	}
	
	my $action = $arg_h->{'action'} || $session->get_param('action');
	if ($action && $action eq 'sort')
	{	$self->set_param('sort_me', 1);
	}
#	else
#	{	$self->set_param('action', $action);
#	}

	#	see if we already have results from this query
	if ($self->get_param('page_size') eq 'all' ||
		$self->get_param('page') ||
	#	$self->get_param('format') || #not implemented
		$self->get_param('sort_me'))
	{	#	this is likely to be a cached search.
		my $cache = $session->get_all_caching_params;
		last if (!$cache || !$cache->{result_list} || !$cache->{query});

		#	check that the basic parameters are the same
		#	Does query match?
		
		
		#	Are the filters (and the search fields?) the same?
		
		

		$self->_set_selected($cache->{$sc.'fields'});
		$self->set_param('select_list', $cache->{$sc.'fields'});
		$self->{queryset}{orig} = $cache->{queryset};
		$self->{queryset}{perl} = $cache->{queryset_perl};

		my @perl_qlist = 
		map {
			[
			map { 
				if ($self->{queryset}{perl}{$_})
				{	qr/$self->{queryset}{perl}{$_}/i;
				}
				else
				{	qr/$_/i;
				}
			} @$_ ];
		} @{$self->{queryset}{orig}};
	
		$self->{queryset}{perllist} = \@perl_qlist;

		print STDERR "Using results from cache...\n";
		$self->{from_cache} = 1;
		$self->cache($cache);
	#	foreach (keys %$cache)
	#	{	if ($_ ne 'result_list' && $self->get_param($_))
	#		{	print STDERR "$_ => ".Dumper($self->get_param($_))."\n";
	#		}
	#	}
	}

	if (!$self->cache)
	{	#	load the search fields
		#	if nothing is specified, use the default
		my $fields = $arg_h->{$sc.'fields'} || $session->get_current_params($sc.'fields');
		$self->_set_selected($fields);

		#	check the queryset and turn it into a structure that we can use
		my $success = $self->_set_queryset;
		if (!$success)
		{	#$self->success(0);
			return;
		}
	}

	#	set the filters
	$self->_set_filters;

	if ($sc eq 'term')
	{	$self->set_param('gp_count_ok', $session->gp_count_ok);
		$self->set_param('ont_list', $session->get_ontology_list);
	}
	return 1;
}

our $global = {
	page_size => 50,
	max_search_results => 1000,
	search_constraint => 'term',
	termsort => 'rel',
	gpsort => 'rel',
	sppsort => 'rel',
};
