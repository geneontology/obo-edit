#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

print STDERR "\n\nStarting term-details.cgi\n";

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
use GO::CGI::Query qw(get_term_in_graph get_nit get_permalink);
use GO::CGI::Session;
#use GO::CGI::NameMunger;
use GO::CGI::Utilities qw(:std);
use GO::Template::Template;
use GO::Dotty::Dotty;
use GO::Parser;
use Data::Dumper;
$Data::Dumper::Indent = 1;

#
# Set up the relevant objects.
#

my $q = new CGI;
my %params = $q->Vars;
my @term_list = split('\0', $params{term}) if $params{term};
my $ses_type = 'term_details';
my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);
my $msg_h;
my $vars;

if (!@term_list)
{	$msg_h = set_message($msg_h, 'fatal', 'no_term');
	process_page_template({ msg_h => $msg_h }, $session);
	exit;
}

#	only accept the first term if the list is longer than 1
if (scalar @term_list > 1)
{	$msg_h = set_message($msg_h, 'warning', 'only_one_term');
}
$vars->{term_acc} = $term_list[0];

my @valid_formats = qw(rdfxml obo go_ont tree);
if ($session->get_default_param('show_graphviz'))
{	push @valid_formats, ('png', 'dot');
	$vars->{show_graphviz} = 1;
}

$vars->{gp_count_ok} = $session->gp_count_ok;
$vars->{show_gp_counts} = $session->show_counts('gp');

my $tmpl;
if ($params{'format'})
{	#	what are the other valid formats?
	if (!grep { $params{'format'} eq $_ } @valid_formats)
	{	$msg_h = set_message($msg_h, 'fatal', 'bad_format', $params{'format'});
		process_page_template({ msg_h => $msg_h }, $session);
		exit;
	}
	$tmpl->{graph} = get_tmpl($params{'format'}, 'term', $vars->{show_gp_counts});
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
print STDERR "tree: ".Dumper($tree)."\n";

#$session->term_sync;
$session->save_session;

$vars->{term_context} = $q->param('term_context') || $session->get_default_param('term_context');

#
# Perform the query
#

my $result_h = get_term_in_graph(
	$session->apph,   	# apph
	$vars->{term_acc},	# acc
	$msg_h,           	# msg_h
	{	tree => $tree, 	# option_h
		tmpl => $tmpl,
		cgi => 'term-details',
		term_context => $vars->{term_context},
		session_id => $session->id,
		gp_count_ok => $vars->{gp_count_ok},
		show_gp_count => $vars->{show_gp_counts},
	}
);

$vars->{msg_h} = $result_h->{msg_h};
if ($vars->{msg_h}{fatal} || !$result_h->{results})	# a fatal error
{	process_page_template($vars, $session);
	exit;
}

$vars->{term} = $result_h->{results}{term};
$vars->{graph} = $result_h->{results}{graph} if $result_h->{results}{graph};
print STDERR "Found the graph. Hurrah!\n";#graph: ".Dumper($vars->{graph});

$session->ses_type($result_h->{ses_type}) if defined $result_h->{ses_type};

if ($params{'format'})
{	if (!$vars->{graph})
	{	$vars->{msg_h} = set_message($msg_h, 'fatal', 'no_ont_term', $vars->{term}->name." ; ".$vars->{term}->acc);
		process_page_template($vars, $session, 'amigo_message');
		exit;
	}
	
	if ($params{'format'} eq 'png' || $params{'format'} eq 'dot')
	{	print STDERR "creating GraphViz...\n";
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

		if ($params{'format'} eq 'png') # a graphviz image
		{	foreach my $x ('graph_bgcolor', 'graph_textcolor', 'layout')
			{	$vars->{extra_filter}{$x} = $session->set_option($x);
			#	print STDERR "extra filter: ".Dumper($vars->{extra_filter}{$x});
			}

			#	add the term parameter to the tree for link-making purposes
			$tree->{term} = [ $term_list[0] ];
			$vars->{tree} = $tree;

			my $links = get_permalink($vars->{graph}, $tree);

			foreach (keys %$links)
			{	$vars->{$_} = $links->{$_};
			}
			($vars->{image_url}, $vars->{image_map_html}) = $session->save_graphviz_image($graphviz);
			print STDERR "done!\n";
			$vars->{graph_title} = $vars->{term_acc};
		
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

=cut
if ($params{'format'} && ($params{'format'} eq 'png' || $params{'format'} eq 'dot'))
{	if (!$vars->{graph})
	{	$vars->{msg_h} = set_message($msg_h, 'fatal', 'no_ont_term', $vars->{term}->name." ; ".$vars->{term}->acc);
		process_page_template($vars, $session, 'amigo_message');
		exit;
	}
	
	#$ENV{PATH} .= ':/usr/local/graphviz/bin';
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
	if ($params{'format'} eq 'dot')
	{	print "Content-type:text/plain\n\n";
		print $graphviz->as_text;
	}
	else # a graphviz image
	{	($vars->{image_url}, $vars->{image_map_html}) = $session->save_graphviz_image($graphviz);
		$vars->{graph_title} = $vars->{term_acc};
		foreach my $x ('graph_bgcolor', 'graph_textcolor', 'layout')
		{	$vars->{extra_filter}{$x} = $session->set_option($x);
		}
		my $links = get_permalink($vars->{graph}, $tree);
		print STDERR "Links: ".Dumper($links);
		foreach (keys %$links)
		{	$vars->{$_} = $links->{$_};
		}
		process_page_template($vars, $session, 'graphviz');
#		$session->__save_session;
	}
	exit;
}
=cut

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
	{	
		my $links = get_permalink($vars->{graph}, $tree);
		print STDERR "Links: ".Dumper($links);
		foreach (keys %$links)
		{	$vars->{$_} = $links->{$_};
		}
		
		$vars->{last_action} = $sync_h->{last_action} || undef;
		$vars->{action_node_list} = $sync_h->{action_node_list} || undef;

#		#	if we just performed some sort of action, see what the target of
#		#	the action was and put it in the action node list
#		if ($params{'action'} && $params{'target'})
#		{	$vars->{action_node_list} = [ split "\0", $params{'target'} ];
#		}
#		print STDERR "action node list: ".Dumper($vars->{action_node_list})."\n";
	}
	else
	{	#	this is a subset graph. Find all the focus nodes and put them into open_0
		my $focus = $vars->{graph}->focus_nodes;
		if ($focus)
		{	$vars->{permalink} = "&amp;term=".join("&amp;term=", map { $_->acc } @$focus);
		#	$vars->{'link'} = $vars->{permalink};
		}
		#	put the focus nodes as 'action' nodes so they're highlighted
		$vars->{action_node_list} = [ map { $_->acc } @$focus ];
	}
	$vars->{extra_filter}{term_context} = $session->set_option('term_context');
	$vars->{extra_filter}{tree_view} = $session->set_option('tree_view');
	$vars->{term_node_list} = [ $vars->{term_acc} ];

	if ($params{'action'} && ($params{'action'} eq 'plus_node' || $params{'action'} eq 'minus_node'))
	{	$vars->{show_reset_link} = 1;
	}
}


$vars->{cgi} = 'term-details';
#print STDERR "extra_filter: ".Dumper($vars->{extra_filter});
print STDERR "Processing tmpl and exiting...\n";
process_page_template($vars, $session);

exit;
