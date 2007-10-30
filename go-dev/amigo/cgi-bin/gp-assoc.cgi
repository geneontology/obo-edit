#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

print STDERR "\n\nStarting gp-assoc.cgi\n";

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
use FileHandle;
use GO::CGI::Query qw(get_gp_assocs);
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);
#use GO::CGI::NameMunger;
#use GO::Template::Template;
use GO::IO::go_assoc;

use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

#	for debugging
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;
#$Data::Dumper::Maxdepth = 3;

#
# Set up the relevant objects.
#

my $q = new CGI;
my %params = $q->Vars;
my @gp_list = split "\0", $params{gp};

print STDERR Dumper(\@gp_list);

my $msg_h;
my $vars;

#
# Perform the query
#

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>'gp_assoc', -read_only=>1);
$session->gp_sync;
$session->save_session;

if (!@gp_list)
{	$msg_h = set_message($msg_h, 'fatal', 'no_gp');
	process_page_template({ msg_h => $msg_h }, $session);
	exit;
}

my $option_h;
my $cache;
foreach ('action', 'page', 'page_size')
{	if (defined $params{$_})
	{	$option_h->{$_} = $params{$_};
		$cache ||= 1;
	}
}

for ('gp', 'term')
{	$option_h->{"show_".$_."_counts"} = $session->show_counts($_);
}
$option_h->{gp_count_ok} = $session->gp_count_ok;

foreach ('exact_match', 'show_gp_counts', 'show_term_counts')
{	$vars->{$_} = 1 if $option_h->{$_};
}

if ($params{'format'})
{	$cache ||= 1;
	#	check we understand the format
	if (!grep { $params{'format'} eq $_ } qw(rdfxml go_assoc))
	{	$msg_h = set_message($msg_h, 'fatal', 'bad_format', $params{'format'});
		process_page_template({ msg_h => $msg_h }, $session);
		exit;
	}
	
	#	get the template for the format
	$option_h->{tmpl} = get_tmpl($params{'format'}, undef, $session->show_counts('term'));
	$option_h->{tmpl}{return_graph} = 1;

	$option_h->{page_size} = 'all';
}
else
{	#	we're doing a standard html query
	$option_h->{tmpl} = get_tmpl('gp_assoc_cgi', undef, $session->show_counts('term'));
	$option_h->{die_nicely} = 1;
}

if ($cache)
{	#	we may already have the results of this query.
	#	Load up the cache and check.
	my $cache_results = $session->load_cached_results;
	if ($cache_results)
	{	if ( join(",", sort @gp_list) eq $cache_results->{query}{gp} )
		{	$option_h->{cache} = $cache_results;

		}
		else
		{	#	our cached results are out of date. Delete 'em.
			$session->delete_cached_results;
		}
	}
	else
	{	undef $cache;
	}
}

#	if this query came from a blast search,
#	use the order from the cached blast results
if ($params{show_blast_scores} && $params{show_blast_scores} eq 'on')
{	print STDERR "Loading up cached blast scores...\n";
	my $blast_cache = $session->load_cached_results('blast_scores');
	print STDERR "blast scores: ".Dumper($blast_cache)."\n";
	
#	put 
	my @new_gp_list;
	my %gp_lookup;
	@gp_lookup{@gp_list} = (1) x @gp_list;
	my %cache_h;
	foreach (@{$blast_cache->{blast_scores}})
	{	if ( $gp_lookup{$_->[0]} )
		{	push @new_gp_list, $_->[0];
			$cache_h{ $_->[0] } = $_->[1];
		}
	}

	#	check everything in gplist is in the new list
	my @missing = grep { !$cache_h{$_}  } @gp_list;
	{	if (@missing)
		{	print STDERR "lost blast score for ".join(",", @missing)."!\n";
			push @new_gp_list, @missing;
			$msg_h->{warning}{lost_blast_scores} = [@missing];
		}
	}

	#	replace gp_list with new gp_list
	print STDERR "gp_list: ".Dumper(\@gp_list)."\n";
	@gp_list = @new_gp_list;
	#	put the BLAST scores into $vars
	$vars->{score_h} = \%cache_h;
	print STDERR "score_h: ".Dumper(\%cache_h);
	$option_h->{gpsort} = 'ordered_input';
}



if (!$params{'format'})
{	#	if we are doing a standard html query, we need to sort the terms and gps
	#	terms are sorted by name, and GPs are sorted by symbol then name
	$option_h->{termsort} = ['term_type', 'name'];

	if (scalar @gp_list > 1)
	{	#	if we're getting our GPs from blast results, use that order.
		#	Otherwise...
		$option_h->{gpsort} = ['symbol', 'full_name'] unless $vars->{score_h};

		$option_h->{page_size} = 'all';
		#	check we don't have too many GPs
		my $max = $session->get_saved_param('max_selected_gps');
		if (scalar @gp_list > $max)
		{	print STDERR "max selected gps: ".$max."\n";
			@{$msg_h->{warning}{too_many_gps}} = $gp_list[$max..-1];
			$vars->{max_selected_gps} = $max;
			@gp_list = @gp_list[0..$max];
			print STDERR "gplist: ".join(", ", @gp_list)."\n";
		}
	}
}

#print STDERR "template: ". Dumper($option_h->{tmpl})."\n";

$option_h->{page_size} = $session->get_saved_param('page_size') if !$option_h->{page_size};
#$option_h->{gp_count_ok} = $session->gp_count_ok;
#$vars->{gp_count_ok} = $option_h->{gp_count_ok};
#$vars->{show_term_counts} = $option_h->{show_term_counts} = $session->show_counts('term');
$vars->{cgi} = 'gp-assoc';

my $result_h = get_gp_assocs(
			-apph => $session->apph,
			-gp_list => \@gp_list,
			-msg_h => $msg_h,
			-option_h => $option_h,
		);

$vars->{msg_h} = $result_h->{msg_h};

if (!$result_h->{results})
{	# no results
	process_page_template($vars, $session);
	exit;
}

$vars->{data} = $result_h->{results};
#print STDERR "data: ".Dumper($vars->{data});

if ($result_h->{to_cache})
{	#	if we've got blast scores, include them
	$result_h->{to_cache}{blast_cache} = $option_h->{cache}{blast_cache} if $option_h->{cache}{blast_cache};
	### save cached results
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
		render_data_in_format($vars->{data}, $params{'format'});
	}
	exit;
}

#
# Select and process the Template.
#

$vars->{n_pages} = $vars->{data}{n_pages} || 1;

my $page_template_to_use = 'gp_assoc';

if ($vars->{data}{product_h})
{	my @valid_gps = map { $_->xref->xref_dbname .":". $_->xref->xref_key } values %{$vars->{data}{product_h}};
	#	set up the URL for paging
	my $url = 'gp='
		. join('&amp;gp=', @valid_gps);
	$vars->{url_string} = $url;

	if (scalar keys %{$vars->{data}{product_h}} > 1)
	{	$page_template_to_use = 'gp_assoc_multi';
	}
	$vars->{gplist} = \@valid_gps;
}

if ($vars->{data}{term_count})
{	print STDERR "term count for GPs: ".Dumper($vars->{data}{term_count})."\n";
}

#print STDERR "order: ".Dumper($vars->{data}{order})."\n";

process_page_template($vars, $session, $page_template_to_use);
exit;
