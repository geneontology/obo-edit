#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

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
use lib "$ENV{GO_ROOT}/amigo/perl";

use strict;
use CGI;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);
use GO::CGI::Session;
use GO::CGI::BLAST qw(:all);
use GO::CGI::Utilities qw(:std);

use Data::Dumper;
$Data::Dumper::Indent = 1;

## Some new stuff to try...
use AmiGO;
my $core = AmiGO->new();

my $verbose = get_environment_param('verbose');
print STDERR "\n\nstarting blast.cgi\n" if $verbose;

my $max_size = 500;		## 500K uploads max.
$CGI::POST_MAX = 1024 * $max_size;

my $vars;
my $q = new CGI;
my $cgi_error = $q->cgi_error;

if ($cgi_error)
{	my $session = new GO::CGI::Session('-q'=>$q, -read_only=>1);
	if ($cgi_error eq '413 Request entity too large')
	{	$cgi_error = 'The file you uploaded was too large. The maximum size for file uploads is '.$max_size.' Kb.';
	}
	$vars->{error} = set_message(undef, 'fatal', $cgi_error);
	$vars->{page_title} = 'BLAST Submission Error';
	process_page_template($vars, $session, 'amigo_message');
	exit;
}

my %params = $q->Vars;
print STDERR "params:\n".Dumper(\%params)."\n" if $verbose;

my $ses_type = 'blast';

if (!$params{action} && !$params{page})
{	#	output the blast query form
	$ses_type = 'blast_query';
	$vars->{max_upload_size} = $max_size;
}
elsif ($params{action})
{	if ($params{action} eq 'blast')
	{	#	blast form has been submitted.
		$ses_type = 'blast_submit';
	}
	elsif ($params{action} eq 'get_blast_results')
	{	$ses_type = 'blast_results';
	}
}

foreach ('max_seq_length', 'max_seq_num')
{	$vars->{$_} = get_environment_param($_);
}

my $apph = create_apph;
my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1, -apph=>$apph);
$session->blast_sync($q);
$session->save_session;

if ($session->ses_type eq 'blast_query')
{	# get the blast bits for the form
	print STDERR "getting the bits for the BLAST query form\n" if $verbose;
	foreach my $x ('blast_filter', 'threshold', 'maxhits')
	{	$vars->{$x} = $params{$x} || $session->get_saved_param($x);
		$vars->{extra_filter}{$x} = $session->set_option_with_value($x, $vars->{$x});
	}
	#print STDERR "starting process_page_template...\n";
	process_page_template($vars, $session);
	#print STDERR "processed page template.\n";
	exit;
}
elsif ($ses_type eq 'blast_submit')
{	#	kick off the blast search

	#	get the blast settings from the cgi, from session saved settings,
	#	or use the default
	foreach ('threshold', 'maxhits', 'blast_filter')
	{	$vars->{$_} = $params{$_} || $session->get_saved_param($_);
	}
	if ($params{use_filters} && $params{use_filters} == 1)
	{	$vars->{use_filters} = 1;
	}

	my $input_h;
	foreach ('seq_id', 'seq', 'uniprot_id')
	{	$input_h->{$_} = $params{$_} if $params{$_};
	}
	
	if ($q->upload('seq_file_upload'))
	{	print STDERR "Uploaded seq file found\n" if $verbose;
		my $file = $q->upload('seq_file_upload');
		if (-f $file) # check it's a plain file
		{	my $temp;
			while (<$file>) {
				$temp .= $_;
			}
			$input_h->{seq_from_file} = $temp unless !$temp;
		}
		else
		{	$vars->{error} = set_message($vars->{error}, 'fatal', 'not_plain_file');
			$vars->{page_title} = 'BLAST Submission Error';
			$session->ses_type('amigo_message');
			process_page_template($vars, $session);
			exit;
		}
	}

	my $blast_settings;
	foreach my $s qw(fasta_db blastp blastx blast_method max_seq_num max_seq_length)
	{	$blast_settings->{$s} = get_environment_param($s);
	}
	if (lc($blast_settings->{blast_method}) eq 'pbs')
	{	foreach my $s qw(qsub queue pbs_user)
		{	$blast_settings->{$s} = get_environment_param($s);
		}
	}
	$blast_settings->{blast_dir} = $session->get_blast_results_dir;

	my $result_h = set_off_blast($session->apph, $vars->{error}, $blast_settings, $input_h, $vars);
	$vars->{error} = $result_h->{error};

	if ($vars->{error}{fatal})
	{	$session->ses_type('amigo_message');
		$vars->{page_title} = 'BLAST Submission Error';
		$core->status_error_client();
		process_page_template($vars, $session);
		exit;
	}
	my $inputs = $result_h->{results};
	#	save these, plus our session settings, as a cache file
	#	add $inputs to our option_h and cache that.
	my $cache_h = {
		blast_filter => $vars->{blast_filter},
		threshold => $vars->{threshold},
		maxhits => $vars->{maxhits} };
	$cache_h->{use_filters} = $vars->{use_filters} if $vars->{use_filters};
		
	$cache_h->{input} = $inputs;
	$session->save_cached_results($cache_h, 'blast_params');
	$vars->{n_seqs} = scalar @{$inputs->{seqs}};

	sleep(2);
	#	quickly check to see if the results are already through
	my $blasted = check_blast($session->get_blast_results_dir, scalar @{$inputs->{seqs}});

	if ($blasted == 1) {
		print STDERR "BLAST finished... getting the results.\n" if $verbose;
		#	fetch the results (fetch_results subroutine below)
		$vars = fetch_results($session, $vars, $cache_h, 1);
		$vars->{n_pages} = $vars->{n_seqs};
	}
}
elsif ($ses_type eq 'blast' || $ses_type eq 'blast_results')
{	#	check for blast results

	#	load the cached blast results
	my $cache = $session->load_cached_results('blast_params');

	if (!$cache->{input}){
	  print STDERR "lost the cache settings!\n" if $verbose;
	  $vars->{error} =
	    set_message($vars->{error},
			'fatal',
			'config_error',
			'Your cached BLAST results have been lost. Please try again.');
	  $session->ses_type('amigo_message');
	  $vars->{page_title} = 'BLAST Error';
	  process_page_template($vars, $session);
	  exit;
	}
	my $page = $params{page} || undef;
	print STDERR "cache loaded: ". Dumper($cache) if $verbose;
	$vars->{n_seqs} = scalar @{$cache->{input}{seqs}};

	#	check that the results are through before we do anything else...
	my $blasted = check_blast($session->get_blast_results_dir, $vars->{n_seqs});
	if ($blasted == 1)
	{	print STDERR "BLAST finished... getting the results.\n" if $verbose;
		my $page = $params{page} || 1;
		$vars = fetch_results($session, $vars, $cache, $page);
		$vars->{n_pages} = $vars->{n_seqs};
		$session->ses_type('blast_results');
		#	get the blast settings from the cgi, from session saved settings,
		#	or use the default
		foreach ('threshold', 'maxhits', 'blast_filter')
		{	$vars->{$_} = $cache->{$_};
		}
		$vars->{use_filters} = 1 if $cache->{use_filters};
	}
	else
	{	$session->ses_type('blast_waiting');
	}
}

process_page_template($vars, $session);
exit;

##### subroutines #####

sub fetch_results {
	my ($session, $vars, $cache_hash, $page) = @_;
	$page = 1 if !$page;

	my $result_h = get_blast_results($session->apph, $vars->{error}, $session->get_blast_results_dir, $page, $session->id, $cache_hash->{use_filters});
	$vars->{error} = $result_h->{error};

	if (!$result_h->{results})
	{	#print STDERR "blast.cgi: No results for blast query found\n";
		$session->ses_type('amigo_message');
		$vars->{page_title} = 'BLAST Results Error';
		process_page_template($vars, $session);
		exit;
	}

	my $data = $result_h->{results};
	$session->save_cached_results($result_h->{to_cache}, 'blast_scores') if $result_h->{to_cache};
	$session->ses_type('blast_results');

	$vars->{data} = $data;
	$vars->{page} = $page;
	$vars->{cgi} = 'blast';

	return $vars;
}

sub check_blast {
	my $blast_results_dir = shift;
	my $n_seqs = shift || 1;
	my $result_file = "$blast_results_dir/result.$n_seqs";

	print STDERR "looking for $result_file...\n" if $verbose;

	if (-e $result_file && -f $result_file) {
		print STDERR "Found the file!\n" if $verbose;
		return 1;
	} else {
		print STDERR "Could not find the file!\n" if $verbose;
		return 0;
	}
}


