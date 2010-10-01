#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

BEGIN {

		if (-f "config.pl") {
			require "config.pl";
		}

		# find go perl libraries pre compile time
		if (defined($ENV{GO_ROOT})) {
				;
		} elsif (-f "../cvs/go-dev/") {
		$ENV{GO_ROOT} = "../cvs/go-dev";
		}
}

use lib "$ENV{GO_ROOT}/go-perl";
use lib "$ENV{GO_ROOT}/go-db-perl";
use lib "$ENV{GO_ROOT}/amigo/perl";

use strict;
use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

use GO::CGI::Query qw(get_term_assocs);
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);
use GO::IO::go_assoc;

#	for debugging
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

## Some new stuff to try...
use AmiGO;
my $core = AmiGO->new();

my $verbose = get_environment_param('verbose');

#
# Set up the relevant objects.
#

my $vars;
my $q = new CGI;
my %params = $q->Vars;
$core->kvetch("Starting term-assoc.cgi: ".Dumper($q));

my @term_list = split('\0', $params{term}) if $params{term};
if (!@term_list)
{	$vars->{error} = set_message(undef, 'fatal', 'no_term');
}

#	check we understand the format
my @valid_formats = qw(rdfxml go_assoc);
if ($params{'format'} && !grep { $params{'format'} eq $_ } @valid_formats)
{	$vars->{error} = set_message(undef, 'fatal', 'bad_format', $params{'format'});
}

#	if we've got an error already, die.
if ($vars->{error}{fatal})
{	my $session = new GO::CGI::Session('-q'=>$q, -temp=>1);
	$vars->{page_title} = 'Term Associations';
	$core->status_error_server();
	process_page_template($vars, $session);
	exit;
}

my $ses_type = 'term_assoc';
my $error;

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);

$session->term_assoc_sync;
$session->save_session;

my $option_h;
my $use_cache;

foreach ('page', 'page_size')
{	if (defined $params{$_})
	{	$option_h->{$_} = $params{$_};
		$use_cache ||= 1;
	}
}

for ('gp', 'term')
{	$vars->{$_} = $option_h->{"show_".$_."_counts"} = $session->show_counts($_);
}
$vars->{gp_count_ok} = $option_h->{gp_count_ok} = $session->gp_count_ok;
$vars->{filters_on} = 1 if keys %{$session->apph->filters};

if ($params{'format'})
{	$use_cache ||= 1;
	#	check we understand the format
	if (!grep { $params{'format'} eq $_ } qw(rdfxml go_assoc))
	{	$error = set_message($error, 'fatal', 'bad_format', $params{'format'});
		process_page_template({ error => $error }, $session);
		exit;
	}
	
	#	get the template for the format
	$option_h->{tmpl} = get_tmpl($params{'format'}, undef, $vars->{show_gp_counts});
	$option_h->{'format'} = $params{'format'};
}
else
{	$option_h->{tmpl} = get_tmpl('term_assoc_cgi', undef, $vars->{show_gp_counts});
	$option_h->{die_nicely} = 1;
#	if we are doing a standard html query, we need to sort the terms and gps
#	terms are sorted by name, and GPs are sorted by symbol then name
	$option_h->{termsort} = ['name'];
	$option_h->{gpsort} = ['symbol', 'full_name'];
}

#$core->kvetch("template: ". Dumper($option_h->{tmpl}));

$option_h->{show_all_ass} = 1;
if ($params{term_assocs} && $params{term_assocs} eq 'direct')
{	$option_h->{show_all_ass} = 0;
}

if ($params{action})
{	#	if ($params{action} eq 'override')
	#	{	$option_h->{action} = $params{action};
	#		$use_cache ||= 1;
	#	}
	#	elsif ($params{action} =~ /filter/)
	if ($params{action} =~ /filter/)
	{	$use_cache = 0;
	}
}

if ($use_cache)
{	#	we may already have the results of this query.
	#	Load up the cache and check.
	my $cache_results = $session->load_cached_results;
	if ($cache_results)
	{	#	check that the query is the same
		if ( join(",", sort @term_list) eq $cache_results->{query}{term} &&
			$option_h->{show_all_ass} == $cache_results->{query}{show_all_ass} &&
			$cache_results->{term_product_ids} &&
			scalar @{$cache_results->{term_product_ids}} > 0)
		{	$option_h->{cache} = $cache_results->{term_product_ids};
		}
		else
		{	$session->delete_cached_results;
		}
	}
}

$option_h->{cgi} = 'term-assoc';
$option_h->{session_id} = $session->id;

#	if we are doing a term_assocs query with results output
#	as html, we want to check that we don't have too many
#	results. If the format is set or the action is 'override',
#	we don't do these checks.
#if (!$params{'format'} && (!$option_h->{action} || $option_h->{action} ne 'override'))

$option_h->{page_size} = $session->get_saved_param('page_size') if !$option_h->{page_size};

#	changed this: unless the query results are cached,
#	we have to count the number of results
if (!$option_h->{cache})
{	$option_h->{check_results} = 1;

	$vars->{max_results_html} = $option_h->{max_results_html} = get_environment_param('max_results_html');

	$vars->{max_results_download} = $option_h->{max_results_download} = get_environment_param('max_results_download') || $vars->{max_results_html} * 10;

	$core->kvetch("Check results is ON");
}

# ## Define when big is getting too big. This option will be used to
# ## prevent certain options from appearing when a combination of
# ## variables is present.
# my $max_results_limit =
#   get_environment_param('max_results_pages') *
#   get_environment_param('page_size'); # an arbitrary number
# $core->kvetch("max results limit will be: " . $max_results_limit);
# $vars->{max_results_limit} = $max_results_limit;

#	new stuff
#	if there's a format specified OR the page size is 'all', don't use paging
unless ($params{'format'} || $option_h->{page_size} eq 'all'){
  $core->kvetch("Turning ON use paging");
  $option_h->{use_paging} = 1;
  $option_h->{chunk_by} = 'LIST_ITEM';
}

#
# Perform the query
#

my $result_h = {};
eval{
  $result_h = get_term_assocs({ apph=>$session->apph, 
				term_list=>\@term_list,
				error => $error,
				option_h => $option_h });
};
if( $@ ){
  $core->kvetch("Failing in db/spec: $@");
  $vars->{error} =
    set_message($vars->{error},
 		'fatal',
 		'Currently, users cannot filter by both ' .
 		'"Data source" and "Species" simultaneously. ' .
		'Please go back and select just one.'
 	       );
  process_page_template($vars, $session, 'amigo_message');
  exit;
}


$vars->{error} = $result_h->{error} if defined $result_h->{error};

if (!$result_h->{results}){
  # no results
  process_page_template($vars, $session, 'amigo_message');
  exit;
}

$vars->{data} = $result_h->{results};

if ($result_h->{to_cache})
{	### save cached results
	$session->save_cached_results($result_h->{to_cache});
}

if ($params{'format'})
{	#	check there wasn't a fatal error of some sort
	if ($vars->{error}{fatal}){
	  # output a standard template here because we don't have proper results
	  $core->status_error_server();
	  process_page_template($vars, $session);
	}
	else
	{	#	write out the files if the data is in a specific format
		render_data_in_format($vars->{data}, $params{'format'}, 'yes');
	}
	exit;
}

$vars->{term_assocs} = $params{term_assocs} || 'all';
$vars->{extra_filter}{term_assocs} = $session->set_option_with_value('term_assocs', $vars->{term_assocs});

$vars->{n_pages} = $vars->{data}{n_pages} || 1;
if ($vars->{n_pages} != 1)
{	$vars->{page} = $option_h->{page} || 1;
}

$vars->{cgi} = 'term-assoc';

if ($vars->{data}{term_h})
{	my @valid_terms = grep { exists $vars->{data}{term_h}{$_} } @term_list;
	if (@valid_terms)
	{	$vars->{termlist} = \@valid_terms;
		
		#	set up the URL for paging
		my $url = 'term='
			. join('&amp;term=', @valid_terms);
		if ($vars->{term_assocs} eq 'direct')
		{	$url .= '&amp;term_assocs=direct';
		}
#		if ($params{action} && $params{action} eq 'override')
#		{	$url .= '&amp;action=override';
#		}
		$vars->{url_string} = $url;
	}
}

#$core->kvetch("term assocs: ".Dumper($vars->{extra_filter}{term_assocs}));

process_page_template($vars, $session);
exit;
