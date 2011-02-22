#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

BEGIN {
  if (-f "config.pl") {
    require "config.pl";
  }

  # find go perl libraries pre compile time
  if (defined($ENV{GO_DEV_ROOT})) {
    # nothing
  } elsif (-f "../cvs/go-dev/") {
    $ENV{GO_DEV_ROOT} = "../cvs/go-dev";
  }
}

use lib "$ENV{GO_DEV_ROOT}/go-perl";
use lib "$ENV{GO_DEV_ROOT}/go-db-perl";
use lib "$ENV{GO_DEV_ROOT}/amigo/perl";

use strict;
use FileHandle;
use GO::CGI::Query qw(get_current_graph get_nit get_permalink);
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);
use GO::Dotty::Dotty;
use GO::Parser;

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
print STDERR "\n\nstarting browse.cgi\n" if $verbose;

my $q = new CGI;
my %params = $q->Vars;
my $vars;

my $apph = create_apph;
my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>'browse', -read_only=>1, -apph=>$apph);
my @valid_formats = qw(rdfxml obo go_ont tree);
if ($session->get_default_param('show_graphviz'))
{	push @valid_formats, ('png', 'dot');
	$vars->{show_graphviz} = 1;
}

$vars->{gp_count_ok} = $session->gp_count_ok;
$vars->{show_gp_counts} = $session->show_counts('gp');
$vars->{filters_on} = 1 if keys %{$session->apph->filters};

my $tmpl;
if ($params{'format'}){	# check we understand the format
  if (!grep { $params{'format'} eq $_ } @valid_formats){ # exit!
    $vars->{error} = set_message($vars->{error},
				 'fatal',
				 'bad_format',
				 $params{'format'});
    $core->status_error_client();
    process_page_template($vars, $session, 'amigo_message');
    exit;
  }
  $tmpl = get_tmpl($params{'format'}, 'term', $vars->{show_gp_counts});
}
else
{	$tmpl = get_tmpl('html', 'graph', $vars->{show_gp_counts});
}

my $graph_sync_h;
foreach ('target', 'action', 'format')
{	$graph_sync_h->{$_} = $params{$_} if $params{$_};
}
if ($params{'format'} && !$params{'action'})
{	$graph_sync_h->{action} = 'set-tree';
}

foreach ('open_0', 'open_1', 'closed', 'term')
{	if ($params{$_})
	{	$graph_sync_h->{tree}{$_} = [ split( /\0|,/, $params{$_}) ];
	}
}

my $sync_h = $session->graph_sync($graph_sync_h);
my $tree = $sync_h->{tree};
$session->save_session;
#print STDERR "tree: ".Dumper($tree)."\n" if $verbose;
#
# Perform the query
#

## It's possible two are being tried at the same time. Catch any problems.
my $result_h = {};
eval{
  $result_h = get_current_graph(
	-apph => $session->apph,
	-error => $vars->{error},
	-tree => $tree,
	-option_h => {
		tmpl => $tmpl, 
		gp_count_ok => $vars->{gp_count_ok},
		show_gp_counts => $vars->{show_gp_counts} });
};
if( $@ ){
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

$vars->{graph} = $result_h->{results};

if ($params{'format'})
{	if ($params{'format'} eq 'png' || $params{'format'} eq 'dot')
	{	my $base = {};
		if ($params{'format'} eq 'png')
		{	$base->{base_url} = "term_details?session_id=".$session->id."&amp;term=";
		}

		my $graphviz = GO::Dotty::Dotty::go_graph_to_graphviz
		(	$vars->{graph},
			{	fillcolor => $session->get_saved_param('graph_bgcolor'),
				fontcolor => $session->get_saved_param('graph_textcolor'),
				layout => $session->get_saved_param('layout'),
			},
			$base);

		if ($params{'format'} eq 'png') # a graphviz image
		{	foreach my $x ('graph_bgcolor', 'graph_textcolor', 'layout')
			{	$vars->{extra_filter}{$x} = $session->set_option_with_value($x, $session->get_saved_param($x));
			#	print STDERR "extra filter: ".Dumper($vars->{extra_filter}{$x}) if $verbose;
			}
		
			$vars->{tree} = $tree;
			my $links = get_permalink($vars->{graph}, $tree);
			foreach (keys %$links)
			{	$vars->{$_} = $links->{$_};
			}
			my $gviz_vars = $session->save_graphviz_image($graphviz);
			$vars->{image_url} = $gviz_vars->{img_url};
			$vars->{image_map_html} = $gviz_vars->{html};
			$vars->{cgi} = 'browse';
			process_page_template($vars, $session, 'graphviz');
			exit;
		}
		else
		{	render_data_in_format($graphviz, 'dot');
		}
	}
	render_data_in_format($vars->{graph}, $params{'format'});
}

my $compact;
$vars->{tree_view} = $session->get_saved_param('tree_view');
if ($vars->{tree_view} eq 'compact')
{	$compact = 1;
}
$vars->{nit} = get_nit($vars->{graph}, $tree->{closed}, $compact);
#$vars->{permalink} = make_permalink();
my $links = get_permalink($vars->{graph}, $tree);
foreach (keys %$links)
{	$vars->{$_} = $links->{$_};
}

$vars->{last_action} = $sync_h->{last_action};
$vars->{action_node_list} = $sync_h->{action_node_list} if $sync_h->{action_node_list};

#print STDERR "action node list: ".Dumper($vars->{action_node_list})."\n" if $verbose;
$vars->{cgi} = 'browse';
#$vars->{extra_filter}{tree_view} = $session->set_option('tree_view');
$vars->{extra_filter}{tree_view} = $session->set_option_with_value('tree_view', $vars->{tree_view});

#print STDERR "extra filter: ".Dumper($vars->{extra_filter}{tree_view});

$vars->{tree} = $tree;

###
### New visualization links...
###

## Fish out the terms on the graph.
my $graph = $vars->{graph};
my $terms = $graph->get_all_nodes;
my @all_terms = map { $_->acc } @$terms;
$vars->{term_acc} = join ' ', @all_terms;

## Generate the links (BUG?: this will cause GET problems for large
## graphs, right? Looking at TE, I drew the line at 500 (and that's
## with additional information), so it's unlikely anybody will get
## close).
$vars->{VIZ_STATIC_LINK} =
  $core->get_interlink({mode => 'visualize',
			arg => {data => $vars->{term_acc},format => 'png'}});
## TODO: this does not appear in the code yet...
$vars->{VIZ_INTERACTIVE_LINK} =
  $core->get_interlink({mode => 'visualize',
			arg => {data => $vars->{term_acc},format => 'svg'}});

process_page_template($vars, $session);

exit;
