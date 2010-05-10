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
use FileHandle;
use GO::CGI::Query qw(get_gp_assocs);
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);
#use GO::Template::Template;
use GO::IO::go_assoc;

use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

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

my $q = new CGI;
my %params = $q->Vars;
print STDERR "\n\nstarting gp-assoc.cgi\nCGI: ".Dumper($q)."\n" if $verbose;

my $vars;
$vars->{page_title} = 'Gene Product Associations';
$vars->{page_title_header} = $vars->{page_title};

my @gp_list = split "\0", $params{gp} if $params{gp};
my $edited_gp_list;

if (!@gp_list)
{	$vars->{error} = set_message(undef, 'fatal', 'no_gp');
}

my @valid_formats = qw(rdfxml go_assoc);
#	check we understand the format
if ($params{'format'} && !grep { $params{'format'} eq $_ } @valid_formats)
{	$vars->{error} = set_message(undef, 'fatal', 'bad_format', $params{'format'});
}

#	if we've got an error already, die.
if ($vars->{error}{fatal})
{	my $session = new GO::CGI::Session('-q'=>$q, -temp=>1);
	$core->status_error_server();
	process_page_template($vars, $session);
	exit;
}

print STDERR Dumper(\@gp_list) if $verbose;

#
# Perform the query
#

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>'gp_assoc', -read_only=>1);
$session->gp_sync;
$session->save_session;

my $option_h;
my $use_cache;    # whether this might be a cached query

for ('gp', 'term')
{	$vars->{$_} = $option_h->{"show_".$_."_counts"} = $session->show_counts($_);
}
$vars->{gp_count_ok} = $option_h->{gp_count_ok} = $session->gp_count_ok;
$vars->{filters_on} = 1 if keys %{$session->apph->filters};

if ($params{'format'})
{	$use_cache ||= 1;
	#	get the template for the format
	$option_h->{'format'} = $params{'format'};
	$option_h->{tmpl} = get_tmpl($params{'format'}, undef, $session->show_counts('term'));
	$option_h->{tmpl}{return_graph} = 1;
}
else
{	#	we're doing a standard html query
	$option_h->{tmpl} = get_tmpl('gp_assoc_cgi', undef, $session->show_counts('term'));
	$option_h->{die_nicely} = 1;
}

foreach ('page', 'page_size')
{	if (defined $params{$_})
	{	$vars->{$_} = $option_h->{$_} = $params{$_};
		$use_cache ||= 1;
	}
}

if ($params{action})
{	if ($params{action} eq 'reset-filters' || $params{action} eq 'filter')
	{	undef $use_cache;  # cache is not going to be valid, so delete it
		$option_h->{'action'} = $params{action};
	}
}

if ($use_cache)
{	#	we may already have the results of this query.
	#	Load up the cache and check.
	my $cache_results = $session->load_cached_results;
	if ($cache_results)
	{	#	check if the cached query matches our current query
		if ( join(",", sort @gp_list) eq $cache_results->{query}{gp} && scalar @{$cache_results->{term_product_ids}} > 0 )
		{	$option_h->{cache} = $cache_results->{term_product_ids};
		}
		else
		{	#	our cached results are out of date. Delete 'em.
			$session->delete_cached_results;
		}
	}
	else
	{	undef $use_cache;
	}
}

#	if this query came from a blast search,
#	use the order from the cached blast results
if ($params{show_blast_scores} && $params{show_blast_scores} eq 'on')
{	print STDERR "Loading up cached blast scores...\n" if $verbose;
	my $blast_cache = $session->load_cached_results('blast_scores');
	print STDERR "blast scores: ".Dumper($blast_cache)."\n" if $verbose;
	
#	order the gp list by blast score
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
		{	print STDERR "lost blast score for ".join(",", @missing)."!\n" if $verbose;
			push @new_gp_list, @missing;
			$vars->{error} = set_message($vars->{error}, 'warning', 'lost_blast_scores', [@missing]);
		}
	}

	#	replace gp_list with new gp_list
	print STDERR "gp_list: ".Dumper(\@gp_list)."\n" if $verbose;
	@gp_list = @new_gp_list;
	#	put the BLAST scores into $vars
	$vars->{score_h} = \%cache_h;
	print STDERR "score_h: ".Dumper(\%cache_h) if $verbose;
	$option_h->{gpsort} = 'ordered_input';
}

## If we are doing a standard html query, we need to sort the terms
## and gps terms are sorted by name, and GPs are sorted by symbol then
## name.
if (!$params{'format'}){
  $option_h->{termsort} = ['term_type', 'name'];

  ## If we're getting our GPs from blast results, use that order.
  ## Otherwise...
  if (scalar @gp_list > 1){
    $option_h->{gpsort} = ['symbol', 'full_name'] unless $vars->{score_h};

    # $option_h->{page_size} = 'all';

    ## Check we don't have too many GPs
    my $max = $session->get_saved_param('max_selected_gps');
    if (scalar @gp_list > $max){
      print STDERR "max selected gps: ".$max."\n" if $verbose;
      $vars->{error} =
	set_message($vars->{error}, 'warning', 'too_many_gps');
      $vars->{max_selected_gps} = $max;

      if ($params{gpset} && $params{gpset} =~ /^\d+$/){
	$option_h->{gpset} = $params{gpset};
      }

      print STDERR "use paging is ON\n" if $verbose;
      my $paged_results =
	get_results_chunk(\@gp_list, 
			  {
			   chunk_size => $max,
			   chunk_n => $option_h->{gpset},
			  });
      $edited_gp_list = $paged_results->{subset};

      if ($paged_results->{n_chunks} > 1){
	$vars->{use_gpset_paging} = 1;
      }

      print STDERR "gplist: ".join(", ", @gp_list)."\n" if $verbose;
    }
  }
}

#print STDERR "template: ". Dumper($option_h->{tmpl})."\n" if $verbose;

$option_h->{page_size} = $session->get_saved_param('page_size') if !$option_h->{page_size};

#	new stuff
#	if there's a format specified OR the page size is 'all', don't use paging
unless ($params{'format'} || $option_h->{page_size} eq 'all')
{	$option_h->{use_paging} = 1;
	$option_h->{chunk_by} = 'LIST_ITEM';
}

#	unless the query results are cached, count the number of results
if (!$option_h->{cache})
{	$option_h->{check_results} = 1;

	$vars->{max_results_html} = $option_h->{max_results_html} = get_environment_param('max_results_html');

	$vars->{max_results_download} = $option_h->{max_results_download} = get_environment_param('max_results_download') || $vars->{max_results_html} * 10;

	print STDERR "Check results is ON\n" if $verbose;
}


$edited_gp_list = \@gp_list if !$edited_gp_list || !@$edited_gp_list;

print STDERR "gplist: ".join(", ", @gp_list)."\nedited: ".join(", ", @$edited_gp_list)."\n" if $verbose;

my $result_h = get_gp_assocs({
			apph => $session->apph,
			gp_list => $edited_gp_list,
			error => $vars->{error},
			option_h => $option_h,
		});

$vars->{error} = $result_h->{error};
$vars->{cgi} = 'gp-assoc';

if (!$result_h->{results})
{	# no results
	print STDERR "error: ".Dumper($vars->{error}) if $verbose;

	process_page_template($vars, $session, 'amigo_message');
	exit;
}

$vars->{data} = $result_h->{results};
print STDERR "data keys: ".join(", ", keys %{$vars->{data}})."\n" if $verbose;

if ($result_h->{to_cache})
{	#	if we've got blast scores, include them
	$result_h->{to_cache}{blast_cache} = $option_h->{cache}{blast_cache} if $option_h->{cache}{blast_cache};
	### save cached results
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
		render_data_in_format($vars->{data}, $params{'format'});
	}
	exit;
}

#
# Select and process the Template.
#

$vars->{n_pages} = $vars->{data}{n_pages} || 1;

#print STDERR "n_pages: ". $vars->{n_pages} . "\n";

my $page_template_to_use = 'gp_assoc';

if ($vars->{data}{product_h})
{	my @valid_gps = map { $_->xref->xref_dbname .":". $_->xref->xref_key } values %{$vars->{data}{product_h}};
	#	set up the URL for paging
	my $url = 'gp='
		. join('&amp;gp=', @valid_gps);
	$vars->{url_string} = $url;

	if (scalar keys %{$vars->{data}{product_h}} > 1 || $vars->{use_gpset_paging})
	{	$page_template_to_use = 'gp_assoc_multi';
	}
	else
	{	my $p = $vars->{data}{order}[0]{gp};
		$vars->{page_title} = $p->symbol;
		$vars->{page_title_header} = $vars->{page_title} . " Associations";
	}
	$vars->{gplist} = \@valid_gps;
}

if ($vars->{data}{term_count})
{	print STDERR "term count for GPs: ".Dumper($vars->{data}{term_count})."\n" if $verbose;
}

#print STDERR "order: ".Dumper($vars->{data}{order})."\n" if $verbose;

process_page_template($vars, $session, $page_template_to_use);
exit;
