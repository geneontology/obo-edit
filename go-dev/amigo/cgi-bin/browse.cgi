#!/usr/bin/perl -w
require 5.8.0;

print STDERR "\n\nStarting browse.cgi\n";

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
use GO::CGI::Query qw(get_current_graph get_nit get_permalink);
use GO::CGI::Session;
#use GO::CGI::NameMunger;
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

my $q = new CGI;
my %params = $q->Vars;
my $vars;

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>'browse', -read_only=>1);

my $graph_sync_h;
foreach ('target', 'action', 'format')
{	$graph_sync_h->{$_} = $params{$_} if $params{$_};
}

foreach ('open_0', 'open_1', 'closed', 'term')
{	if ($params{$_})
	{	$graph_sync_h->{tree}{$_} = [ split( /\0|,/, $params{$_}) ];
	}
}

my $tree = $session->new_graph_sync($graph_sync_h);
print STDERR "tree: ".Dumper($tree)."\n";

#$session->graph_sync;

#print STDERR "Session after syncing: ".Dumper($session)."\n";

$session->save_session;

#print STDERR "ENV{PATH} = ".Dumper($ENV{PATH})."\n";

my $tmpl;
if ($params{'format'})
{	#	check we understand the format
	if (!grep { $params{'format'} eq $_ } qw(rdfxml obo go_ont tree png dot))
	{	#	exit!
		$vars->{msg_h} = set_message($vars->{msg_h}, 'fatal', 'bad_format', $params{'format'});
		process_page_template($vars, $session);
		exit;
	}
	$tmpl = get_tmpl($params{'format'}, 'term', $session->gp_count_ok);
}
else
{	$tmpl = get_tmpl('html', 'graph', $session->gp_count_ok);
}

$vars->{gp_count_ok} = $session->gp_count_ok;

#
# Perform the query
#

my $result_h = get_current_graph(
	-apph => $session->apph,
	-msg_h => $vars->{msg_h},
	-tree => $tree,
	-option_h => { tmpl => $tmpl, gp_count_ok => $vars->{gp_count_ok} });

$vars->{msg_h} = $result_h->{msg_h};
if (!$result_h->{results})
{	process_page_template($vars, $session);
	exit;
}

$vars->{graph} = $result_h->{results};

if ($params{'format'})
{	if ($params{'format'} eq 'rdfxml')
	{	print "Content-type:text/plain\n\n";
		my $out = new FileHandle(">-");
		$vars->{graph}->to_xml($out);
	}
	elsif ($params{'format'} eq 'obo')
	{	print "Content-type:text/plain\n\n";
		$vars->{graph}->export({ format => 'obo' });
	}
	elsif ($params{'format'} eq 'go_ont')
	{	print "Content-type:text/plain\n\n";
		$vars->{graph}->to_text_output(-fmt=>'gotext');
	}
	elsif ($params{'format'} eq 'tree')
	{	print "Content-type:text/plain\n\n";
		$vars->{graph}->to_text_output(-fmt=>'tree');
	}
	else # graphviz formats
	{	$ENV{PATH} .= ':/usr/local/graphviz/bin';
		$session->ses_type('graphviz');
		my $base = {};
		if ($params{'format'} eq 'png')
		{	$base->{base_url} = "term-details.cgi?session_id=".$session->id."&amp;term=";
		}
		my $graphviz = GO::Dotty::Dotty::go_graph_to_graphviz
		(	$vars->{graph},
			{	fillcolor => $session->get_saved_param('graph_bgcolor') || undef,
				fontcolor => $session->get_saved_param('graph_textcolor') || undef,
				layout => $session->get_saved_param('layout') || undef,
			},
			$base);
		if ($params{'format'} eq 'dot')
		{	print "Content-type:text/plain\n\n";
			print $graphviz->as_text;
		}
		else # a graphviz image
		{	($vars->{image_url}, $vars->{image_map_html}) = $session->save_graphviz_image($graphviz);
			
		#	foreach (keys %$vars)
		#	{	print STDERR "vars->{".$_."}: ".$vars->{$_}."\n";
		#	}
			foreach my $x ('graph_bgcolor', 'graph_textcolor', 'layout')
			{	$vars->{extra_filter}{$x} = $session->set_option($x);
			}
		#	$vars->{permalink} = get_permalink();
			$vars->{cgi} = 'browse';
			process_page_template($vars, $session);
		}
	}
	exit;
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

#	if we just performed some sort of action, see what the target of
#	the action was and put it in the action node list
if ($params{'action'} && $params{'target'})
{	$vars->{action_node_list} = [ split "\0", $params{'target'} ];
	if ($params{'action'} eq 'plus_node')
	{	$vars->{last_action} = 'Opened '.$params{target};
	}
	elsif ($params{action} eq 'minus_node')
	{	$vars->{last_action} = 'Closed '.$params{target};
	}
}

if ($params{action})
{	if ($params{action} eq 'reset-filters')
	{	$vars->{last_action} = 'Reset filters';
	}
	elsif ($params{action} eq 'filter')
	{	$vars->{last_action} = 'Set filters';
	}
	elsif ($params{action} eq 'permalink')
	{	$vars->{last_action} = 'Created permalink';
	}
	elsif ($params{action} eq 'set-tree')
	{	$vars->{last_action} = 'Set the tree';
#		if ($tree->{open_0} && @{$tree->{open_0}
	}
}
else
{	$vars->{last_action} = 'Reset the tree';
}


#print STDERR "action node list: ".Dumper($vars->{action_node_list})."\n";
$vars->{cgi} = 'browse';
$vars->{term_node_list} = undef;
$vars->{extra_filter}{tree_view} = $session->set_option('tree_view');
$vars->{tree_view} = $session->get_saved_param('tree_view');
$vars->{tree} = $tree;

print STDERR "tree: ".Dumper($tree)."\n";

process_page_template($vars, $session);

exit;
