#!/usr/bin/perl5.8.6 -w
require 5.8.0;

print STDERR "\n\nStarting term-assoc.cgi\n";

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
use lib "$ENV{GO_ROOT}/new-amigo/perl";

use strict;
use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

use GO::CGI::Query qw(get_term_assocs);
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);
#use GO::CGI::NameMunger;
use GO::IO::go_assoc;

#	for debugging
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

#
# Set up the relevant objects.
#

my $q = new CGI;
my %params = $q->Vars;
my @term_list = split('\0', $params{term}) if $params{term};
my $ses_type = 'term_assoc';
my $vars;
my $msg_h;

print STDERR "cgi: ".Dumper(\%params)."\n";


my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);
$session->term_assoc_sync;
$session->save_session;

if (!@term_list)
{	$msg_h = set_message($msg_h, 'fatal', 'no_term');
	process_page_template({ msg_h => $msg_h }, $session);
	exit;
}

my $option_h;
my $use_cache;
foreach ('action', 'page', 'page_size')
{	if (defined $params{$_})
	{	$option_h->{$_} = $params{$_};
		$use_cache ||= 1;
	}
}

if ($params{'format'})
{	$use_cache ||= 1;
	#	check we understand the format
	if (!grep { $params{'format'} eq $_ } qw(rdfxml go_assoc))
	{	$msg_h = set_message($msg_h, 'fatal', 'bad_format', $params{'format'});
		process_page_template({ msg_h => $msg_h }, $session);
		exit;
	}
	
	#	get the template for the format
	$option_h->{tmpl} = get_tmpl($params{'format'}, undef, $session->gp_count_ok);
	
	$option_h->{page_size} = 'all';
}
else
{	$option_h->{tmpl} = get_tmpl('term_assoc_cgi', undef, $session->gp_count_ok);
	$option_h->{die_nicely} = 1;
#	if we are doing a standard html query, we need to sort the terms and gps
#	terms are sorted by name, and GPs are sorted by symbol then name
	$option_h->{termsort} = ['name'];
	$option_h->{gpsort} = ['symbol', 'full_name'];
}

$option_h->{page_size} = $session->get_saved_param('page_size') if !$option_h->{page_size};
$option_h->{show_all_ass} = 1;
if ($params{term_assocs} && $params{term_assocs} eq 'direct')
{	$option_h->{show_all_ass} = 0;
}

if ($use_cache)
{	#	we may already have the results of this query.
	#	Load up the cache and check.
	my $cache_results = $session->load_cached_results;
	if ($cache_results)
	{	#	check that the query is the same
		if ( join(",", sort @term_list) eq $cache_results->{query}{term} &&
		$option_h->{show_all_ass} == $cache_results->{query}{show_all_ass})
		{	$option_h->{cache} = $cache_results;
		}
		else
		{	$session->delete_cached_results;
		}
	}
}

$option_h->{gp_count_ok} = $session->gp_count_ok;
$option_h->{cgi} = 'term-assoc';
$option_h->{session_id} = $session->id;

#	if we are doing a term_assocs query with results output
#	as html, we want to check that we don't have too many
#	results. If the format is set or the action is 'override',
#	we don't do these checks.
my $check_results;
if (!$params{'format'} && (!$option_h->{action} || $option_h->{action} ne 'override'))
{	$option_h->{check_results} = 1;
	$vars->{max_results_pages} = get_environment_param('max_results_pages');
	print STDERR "Check results is ON\n";
}

#
# Perform the query
#

print STDERR "template: ". Dumper($option_h->{tmpl})."\n";

my $result_h = get_term_assocs(-apph=>$session->apph, 
                               -term_list=>\@term_list,
                               -msg_h => $msg_h,
                               -option_h => $option_h);
$vars->{msg_h} = $result_h->{msg_h};

if (!$result_h->{results})
{	# no results
	process_page_template($vars, $session);
	exit;
}

$vars->{data} = $result_h->{results};
if ($result_h->{to_cache})
{	### save cached results
	$session->save_cached_results($result_h->{to_cache});
}

if ($params{'format'})
{	#	check there wasn't a fatal error of some sort
	if ($vars->{msg_h}{fatal})
	{	#	output a standard template here because we don't have proper results
		process_page_template($vars, $session);
	}
	else
	{	#	write out the files if the data is in a specific format
		if ($params{'format'} eq 'rdfxml')
		{	print "Content-type:text/plain\n\n";
			my $out = new FileHandle(">-");
			$vars->{data}->to_xml($out, -show_associations=>'yes');
		}
		elsif ($params{'format'} eq 'go_assoc')
		{	my $out = new FileHandle(">-");
			my $ga_out = GO::IO::go_assoc->new($out);
			$ga_out->cgi_header;
			$ga_out->write_graph($vars->{data});
		#	print "Content-type:text/plain\n\n";
		#	my $out = new FileHandle(">-");
		#	$graph->export({ format => 'go_assoc' });
		}
	}
	exit;
}

$vars->{term_assocs} = $params{term_assocs} || 'all';
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
		$vars->{url_string} = $url;
	}
}

$vars->{extra_filter}{term_assocs} = $session->set_option('term_assocs');

#print STDERR "term assocs: ".Dumper($vars->{extra_filter}{term_assocs})."\n";

process_page_template($vars, $session);
exit;
