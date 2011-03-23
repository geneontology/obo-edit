#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

BEGIN { require "config.pl" if -f "config.pl" ; }
use lib "$ENV{GO_DEV_ROOT}/go-perl";
use lib "$ENV{GO_DEV_ROOT}/go-db-perl";
use lib "$ENV{GO_DEV_ROOT}/amigo/perl";
use lib $ENV{GOBO_ROOT};

use strict;

use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);
use GO::CGI::Query qw(get_term_in_graph get_nit get_permalink);
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);
#use GO::Template::Template;
use GO::Dotty::Dotty;
use GO::Parser;
use Data::Dumper;
$Data::Dumper::Indent = 1;

## Some new stuff to try...
use AmiGO;
use AmiGO::External::XML::GONUTS;
my $core = AmiGO->new();

my $verbose = get_environment_param('verbose');

#
# Set up the relevant objects.
#
my $vars;
$vars->{page_title} = 'Term Details';
my $q = new CGI;
my %params = $q->Vars;

print STDERR "\n\nStarting term-details.cgi\n" if $verbose;

my @term_list = split('\0', $params{term}) if $params{term};

if (!@term_list){
  $vars->{error} = set_message(undef, 'fatal', 'no_term');
}

#	check we understand the format
my @valid_formats = qw(rdfxml obo go_ont tree);
if (get_environment_param('show_graphviz') == 1)
{	push @valid_formats, ('png', 'dot');
	$vars->{show_graphviz} = 1;
}

if( $params{'format'} &&
    ! grep { $params{'format'} eq $_ } @valid_formats){
  $vars->{error} = set_message(undef, 'fatal', 'bad_format', $params{'format'});
}

#	if we've got an error already, die.
if ($vars->{error}{fatal}){
  my $session = new GO::CGI::Session('-q'=>$q, -temp=>1);
  $core->status_error_client();
  process_page_template($vars, $session, 'amigo_message');
  exit;
}

my $ses_type = 'term_details';
my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);
my $error;

#	only accept the first term if the list is longer than 1
if (scalar @term_list > 1)
{	$error = set_message($error, 'warning', 'only_one_term');
}

$vars->{term_acc} = $term_list[0];

###
### Link to new page...
###

$vars->{NEW_LINK} =
  $core->get_interlink({mode => 'term_details',
			arg => {acc => $vars->{term_acc}},
			optional => {full => 1}});

###
### New visualization links...
###

$vars->{VIZ_STATIC_LINK} =
  $core->get_interlink({mode => 'visualize_subset',
			arg => {subset => $vars->{term_acc}}});

###
### GONuts query.
###

## Cutoff a year ago (in seconds).
my $gonuts =
  AmiGO::External::XML::GONUTS->new({cutoff_time => 31536000});
my $answer_p = $gonuts->query_term($vars->{term_acc});
$vars->{GONUTS_SUCCESS} = 0;
$vars->{GONUTS_TOTAL_COUNT} = '';
$vars->{GONUTS_RECENT_COUNT} = '';
$vars->{GONUTS_PAGE_TITLE} = '';
$vars->{GONUTS_PAGE_URL} = '';
$vars->{GONUTS_DATE_STRING} = '';
if( $answer_p ){
  $vars->{GONUTS_SUCCESS} = 1;
  $vars->{GONUTS_TOTAL_COUNT} = $gonuts->get_total_count();
  $vars->{GONUTS_RECENT_COUNT} = $gonuts->get_recent_count();
  $vars->{GONUTS_PAGE_TITLE} = $gonuts->get_page_title();
  $vars->{GONUTS_PAGE_URL} = $gonuts->get_page_url();
  $vars->{GONUTS_DATE_STRING} = $gonuts->get_date_string();

  ## DEBUG
  $gonuts->kvetch('GONUTS: got an answer:');
  $gonuts->kvetch("\t" . $vars->{GONUTS_TOTAL_COUNT});
  $gonuts->kvetch("\t" . $vars->{GONUTS_RECENT_COUNT});
  $gonuts->kvetch("\t" . $vars->{GONUTS_PAGE_TITLE});
  $gonuts->kvetch("\t" . $vars->{GONUTS_PAGE_URL});
}

$vars->{gp_count_ok} = $session->gp_count_ok;
$vars->{show_gp_counts} = $session->show_counts('gp');
$vars->{filters_on} = 1 if keys %{$session->apph->filters};

my $tmpl;
if ($params{'format'})
{	$tmpl->{graph} = get_tmpl($params{'format'}, 'term', $vars->{show_gp_counts});
}
else
{	$tmpl = get_tmpl('term_details_cgi', undef, $vars->{show_gp_counts});
}

my $graph_sync_h;
foreach ('open_1', 'closed') # get the current state of the tree
{	if ($params{$_})
	{	$graph_sync_h->{tree}{$_} = [ split( /\0|,/, $params{$_}) ];
	}
}
foreach ('target', 'action', 'format')
{	$graph_sync_h->{$_} = $params{$_} if $params{$_};
}

my $sync_h = $session->graph_sync($graph_sync_h);
my $tree = $sync_h->{tree};
print STDERR "tree: ".Dumper($tree)."\n" if $verbose;

$session->save_session;

$vars->{term_context} = $q->param('term_context') || $session->get_default_param('term_context');

#
# Perform the query
#

my $result_h = {};
eval{
  $result_h = get_term_in_graph({ apph => $session->apph,
				  acc => $vars->{term_acc},
				  error => $error,
				  option_h =>
				  {	tree => $tree,
					tmpl => $tmpl,
					cgi => 'term-details',
					term_context => $vars->{term_context},
					session_id => $session->id,
					gp_count_ok => $vars->{gp_count_ok},
					show_gp_count =>$vars->{show_gp_counts},
				  },
				});
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

if( $params{'format'} &&
    ! $result_h->{results}{graph} &&
    ! $vars->{error}{fatal}){
  $vars->{error} = set_message($vars->{error},
			       'fatal',
			       'no_ont_term',
			       $vars->{term}->name." ; ". $vars->{term}->acc);
}
if( $vars->{error}{fatal} ||
    ! $result_h->{results}){	# a fatal error
  $core->status_error_client();
  process_page_template($vars, $session, 'amigo_message');
  exit;
}

$vars->{term} = $result_h->{results}{term};
$vars->{page_title} = $vars->{term}->name unless !defined $vars->{term}->name;
$vars->{graph} = $result_h->{results}{graph} if $result_h->{results}{graph};
print STDERR "Found the graph. Hurrah!\n" if $verbose;#graph: ".Dumper($vars->{graph});

$session->ses_type($result_h->{ses_type}) if defined $result_h->{ses_type};

if ($params{'format'})
{	if ($params{'format'} eq 'png' || $params{'format'} eq 'dot')
	{	print STDERR "creating GraphViz...\n" if $verbose;
		my $base = {};
		if ($params{'format'} eq 'png')
		{	$base->{base_url} = "term-details.cgi?session_id=".$session->id."&amp;term=";
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
			{	#$vars->{extra_filter}{$x} = $session->set_option($x);
				$vars->{extra_filter}{$x} = $session->set_option_with_value($x, $session->get_saved_param($x));
			#	print STDERR "extra filter: ".Dumper($vars->{extra_filter}{$x}) if $verbose;
			}

			#	add the term parameter to the tree for link-making purposes
			$tree->{term} = [ $term_list[0] ];
			$vars->{tree} = $tree;

			my $links = get_permalink($vars->{graph}, $tree);

			foreach (keys %$links)
			{	$vars->{$_} = $links->{$_};
			}
			my $gviz_vars = $session->save_graphviz_image($graphviz);
			$vars->{image_url} = $gviz_vars->{img_url};
			$vars->{image_map_html} = $gviz_vars->{html};
			$vars->{graph_title} = $vars->{term_acc};
			$vars->{cgi} = 'browse';

			print STDERR "done!\n" if $verbose;

			process_page_template($vars, $session, 'graphviz');
			exit;
		}
		else
		{	render_data_in_format($graphviz, 'dot');
		}
	}
	render_data_in_format($vars->{graph}, $params{'format'});
}

if ($session->ses_type ne 'vocab_details')
{	#	add the term parameter to the tree for link-making purposes
	$tree->{term} = [ $term_list[0] ];
	$vars->{tree} = $tree;

	my $compact;
	$vars->{tree_view} = $session->get_saved_param('tree_view');
	if ($vars->{tree_view} eq 'compact')
	{	$compact = 1;
	}
	$vars->{nit} = get_nit($vars->{graph}, $tree->{closed}, $compact);

	if ($session->ses_type eq 'term_details')
	{	my $links = get_permalink($vars->{graph}, $tree);
		print STDERR "Links: ".Dumper($links) if $verbose;
		foreach (keys %$links)
		{	$vars->{$_} = $links->{$_};
		}
		
		$vars->{last_action} = $sync_h->{last_action} || undef;
		$vars->{action_node_list} = $sync_h->{action_node_list} || undef;
	}
	else
	{	#	this is a subset graph. Find all the focus nodes and put them into open_0
		my $focus = $vars->{graph}->focus_nodes;
		if ($focus)
		{	$vars->{permalink} = "&amp;term=".$vars->{term_acc};
			$vars->{'link'} = "&amp;term=".join("&amp;term=", map { $_->acc } @$focus);
		}
		#	put the focus nodes as 'action' nodes so they're highlighted
		$vars->{action_node_list} = [ map { $_->acc } @$focus ];
	}
	$vars->{extra_filter}{term_context} = $session->set_option_with_value('term_context', $vars->{term_context});
	$vars->{extra_filter}{tree_view} = $session->set_option_with_value('tree_view', $vars->{tree_view});
	$vars->{term_node_list} = [ $vars->{term_acc} ];

	if ($params{'action'} && ($params{'action'} eq 'plus_node' || $params{'action'} eq 'minus_node'))
	{	$vars->{show_reset_link} = 1;
	}
}


$vars->{cgi} = 'term-details';
#print STDERR "extra_filter: ".Dumper($vars->{extra_filter}) if $verbose;
print STDERR "Processing tmpl and exiting...\n" if $verbose;
process_page_template($vars, $session);

exit;
