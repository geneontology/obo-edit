#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

print STDERR "\n\nStarting blast.cgi\n";

BEGIN {
		if (-f "config.pl") {
			require "config.pl";
		}
		# find go perl libraries pre compile time
		#hmm, use lib "$ENV{GO_ROOT}/go-perl" imports /go-perl !!!
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
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);
use GO::CGI::Session;
use GO::CGI::Analysis qw(:all);
use GO::CGI::Utilities qw(:std);

use Data::Dumper;
$Data::Dumper::Indent = 1;

my $max_size = 500;		## 500K uploads max.
$CGI::POST_MAX = 1024 * $max_size;

my $msg_h;
my $q = new CGI;
my $error = $q->cgi_error;

if ($error)
{	my $session = new GO::CGI::Session('-q'=>$q, -read_only=>1);
	if ($error eq '413 Request entity too large')
	{	$error .= '.<br>The maximum size for file uploads is '.$max_size.' Kb.';
	}
	$msg_h = set_message($msg_h, 'fatal', $error);
	process_page_template({ msg_h => $msg_h }, $session, 'blast_query');
	exit;
}

my %params = $q->Vars;
print STDERR "params:\n".Dumper(\%params)."\n";

my $ses_type = 'blast';
my $action = $params{action};
my $page = $params{page};

my $vars;
foreach ('max_seq_length', 'max_seq_num')
{	$vars->{$_} = get_environment_param($_);
}

if (!$action && !$page)
{	#	output the blast query form
	$ses_type = 'blast_query';
	$vars->{max_upload_size} = $max_size;
}
elsif ($action && $action eq 'blast')
{	#	blast form has been submitted.
	$ses_type = 'blast_submit';
}

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);
$session->blast_sync;
$session->save_session;

if ($ses_type eq 'blast_submit')
{	#	kick off the blast search

	my $input_h;
	foreach ('seq_id', 'seq', 'upfile', 'sptr_id')
	{	$input_h->{$_} = $params{$_} if $params{$_};
	}

	my $option_h;

	#	get the blast settings from the cgi, from session saved settings,
	#	or use the default
	foreach ('threshold', 'maxhits', 'blast_filter')
	{	$option_h->{$_} = $params{$_} || $session->get_saved_param($_);
	}
	
	if ($params{use_filters} && $params{use_filters} == 1)
	{	$option_h->{use_filters} = 1;
	}

	#	copy those settings into $vars
	$vars = $option_h;

	my $result_h = set_off_blast($session->apph, $msg_h, $session->get_blast_results_dir, $input_h, $option_h);
	$msg_h = $result_h->{msg_h};

	if ($msg_h->{fatal})
	{	$vars->{msg_h} = $msg_h;
		$session->ses_type('blast_query');
	}
	else 
	{	my $inputs = $result_h->{results};
		#	save these, plus our session settings, as a cache file
		#	add $inputs to our option_h and cache that.
		$option_h->{input} = $inputs;
		$session->save_cached_results($option_h, 'blast_params');
		$vars->{n_seqs} = scalar @{$inputs->{seqs}};
	
		#	quickly check to see if the results are already through
		my $blasted = check_blast($session->get_blast_results_dir, scalar @{$inputs->{seqs}});
	
		if ($blasted == 1) {
			print STDERR "BLAST finished... checking the results.\n";
			#	fetch the results (fetch_results subroutine below)
			$vars = fetch_results($session, $msg_h, $option_h, $vars, $page);
			$vars->{n_pages} = $vars->{n_seqs};
		}
		else
		{	$vars->{msg_h} = $msg_h;
		}
	}
}
elsif ($ses_type eq 'blast')
{	#	check for blast results
	#	load the cached blast results
	my $cache = $session->load_cached_results('blast_params');

	print STDERR Dumper($cache);

	if ($cache->{input})
	{	$vars->{n_seqs} = scalar @{$cache->{input}{seqs}};
		if (!$page)
		{	my $blasted = check_blast($session->get_blast_results_dir, $vars->{n_seqs});
			$page = 1 if $blasted == 1;
			print STDERR "BLAST finished... checking the results.\n";
		}
	
		if ($page)
		{	$vars = fetch_results($session, $msg_h, $cache, $vars, $page);
			$vars->{n_pages} = $vars->{n_seqs};
			$session->ses_type('blast_results');
			#	get the blast settings from the cgi, from session saved settings,
			#	or use the default
			foreach ('threshold', 'maxhits', 'blast_filter')
			{	$vars->{$_} = $cache->{$_} || $session->get_saved_param($_);
			}
		}
		else
		{	$session->ses_type('blast_waiting');
		}
	}
	else
	{	print STDERR "lost the cache settings!\n";
		$vars->{msg_h} = set_message($msg_h, 'fatal', 'config_error', 'Your cached BLAST results have been lost. Please try again.');
		$session->ses_type('amigo_message');
	}
}

# get the blast bits for the form
#$vars->{extra_filter} = $session->get_blast_data if $ses_type eq 'blast_query';
if ($session->ses_type eq 'blast_query')
{	print STDERR "getting the bits for the BLAST query form\n";
	foreach my $x ('blast_filter', 'threshold', 'maxhits')
	{	$vars->{extra_filter}{$x} = $session->set_option($x);
	#	print STDERR "extra filter: ".Dumper($vars->{extra_filter}{$x});
	}
}


process_page_template($vars, $session);
exit;

##### subroutines #####

sub fetch_results {
	my $session = shift;
	my $msg_h = shift;
	my $cache_hash = shift;
	my $vars = shift;
	my $page = shift || 1;

	my $result_h = get_blast_results($session->apph, $msg_h, $session->get_blast_results_dir, $page, $session->id, $cache_hash->{use_filters});
	$msg_h = $result_h->{msg_h};
	my $data = $result_h->{results} if $result_h->{results};
	return ($msg_h) if !$data;

	my $cache = $result_h->{to_cache};
	$session->save_cached_results($cache, 'blast_scores') if $cache;
	$session->ses_type('blast_results');

	$vars->{data} = $data;
	$vars->{page} = $page;
	$vars->{msg_h} = $msg_h;
	$vars->{cgi} = 'blast';

	return $vars;
}

