#!/usr/local/bin/perl -w
#	redirects links to the new CGIs

require 5.8.0;

print STDERR "\n\n";

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
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

use GO::CGI::Session;

use Data::Dumper;
$Data::Dumper::Indent = 1;

#
# Set up the relevant objects.
#

my $q = new CGI;
my %params = $q->Vars;
my $location;
my $session = $params{'session_id'} || GO::CGI::Session::create_session_id;

if ($params{'search_constraint'} && $params{'search_constraint'} eq 'terms')
{	$params{'search_constraint'} = 'term';
	$q->param(-name=>'search_constraint', -values=>'term');
}

if ($params{'query'})
{	print STDERR "Queries is defined!\n";
	if ($params{'search_constraint'})
	{	if ($params{'search_constraint'} eq 'term' || $params{'search_constraint'} eq 'terms')
		{	if ($params{'action'})
			{	if ($params{'action'} eq 'query')
				{	#	term search
					$location = 'search.cgi?search_constraint=term&query=' . $params{query};
				}
				elsif ($params{'action'} eq 'minus_node'
						|| $params{'action'} eq 'plus_node'
						|| $params{'action'} eq 'replace_tree')
				{	#	this is a standard browse
				}
				elsif ($params{'action'} eq 'dotty')
				{	#	graphviz term graph
					if ($params{'graph_format'} && $params{'graph_format'} eq 'dottext')
					{	$location = 'graphviz.cgi?term=' . $params{query};
						foreach my $p qw(graph_format ... ... )
						{	if ($params{$p})
							{	$location .= "&$p=$params{$p}";
							}
						}
					}
					else
					{	$location = 'graphviz.cgi?term=' . $params{query};
					}
				}
			}
			elsif ($params{'view'} && $params{'view'} eq 'details')
			{	$location = 'term-details.cgi?term='.$params{query};
			}
			elsif ($params{'view'} && $params{'view'} eq 'assoc')
			{	$location = 'term-assoc.cgi?term='.$params{query};

				if ($params{'format'})
				{	if ($params{'format'} eq 'xml')
					{	#	cut this for now?
					}
					elsif ($params{'format'} eq 'go_assoc')
					{	#	cut this for now?
					}
				}

			}
		}
		elsif ($params{'search_constraint'} eq 'gp')
		{	$location = 'search.cgi?search_constraint=gp&query='.$params{query};
		}
	}
	elsif ($params{'action'} eq 'summary')
	{	#	pie chart / graph view
		$location = 'term-chart.cgi?term='.$params{query};
	}
				
	#	this is to remap those query=GO:xxx which are coming from somewhere or other
	if (!$ses_type)
	{	if (!$params{'view'} && !$params{'action'} && !$params{'link'} && !$params{'format'})
		{	# do a term search as default as we have no real way of telling if a string is a gp name or a go term name
			$location = 'search.cgi?search_constraint=term&query=' . $params{query};
		}
	}
}
elsif ($params{'gp'})
{	print STDERR "Could not find any queries.\n";
	if ($params{'format'} && $params{'format'} eq 'fasta')
	{	#	fasta sequences: but maybe should send to the gp details page?
		$location = 'gp-details.cgi?gp=' . $params{gp};
	}
	elsif ($params{'view'} && $params{'view'} eq 'details')
	{	#	set the gp_comp bit later
		$location = 'gp-details.cgi?gp=' . $params{gp};
	}
	#	possible craziness: send to the GP search
	if (!$location)
	{	$location = 'search.cgi?search_constraint=gp&query=' . $params{gp};
	}
}
elsif ($params{'action'})
{	if ($params{'action'} eq 'replace_tree'
		|| $params{'action'} eq 'minus_node'
		|| $params{'action'} eq 'plus_node')
	{	#	this is a standard browse
		$ses_type = 'browse';
	}
	elsif ($params{'action'} eq 'dotty')
	{	#	graphical view
		#	without any known session, returns the 'all' node
		#	maybe should just point this at the browse page instead?
		$location = 'browse.cgi';
	#	if ($params{'graph_format'} && $params{'graph_format'} eq 'dottext')
	#	{	#	dot format
	#	}
	#	else
	#	{	#	graph
	#	}
	}
	elsif ($params{'action'} eq 'get_job_by_id' || $params{'action'} eq 'blast')
	{	#	blast pages
		$location = 'blast.cgi';
	}
}
elsif ($params{'format'})
{	if ($params{'format'} eq 'go_ff')
	{	#	browse, in GO FF format
		#	will only return the root node without a session_id
		$ses_type = 'go_ff';
	}
	elsif ($params{'format'} eq 'xml')
	{	#	browse, in XML format
		#	will only return the root node without a session_id
		$ses_type = 'rdf_xml';
	}
}
elsif ($params{'link'} && $params{'link'} eq 'static')
{	#	permalink
	$location = 'browse.cgi?link=static';
	foreach my $p qw(open_1 open_0 closed)
	{	if ($params{$p})
		{	$location .= "&$p=$params{$p}";
		}
	}
}
elsif ($params{'advanced_query'})
{	$location = 'search.cgi?action=advanced_query';
}

if (!$location)
{	$location = 'search.cgi';
}

print "Location: ".$session->get_param('cgi_url')."/$location\n\n";
