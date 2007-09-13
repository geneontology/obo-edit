#!/usr/bin/perl5.8.6 -w
require 5.8.0;

print STDERR "\n\n";

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
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);

use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

#	for debugging
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

#
# Set up the relevant objects.
#

my $q = new CGI;
my %params = $q->Vars;
my @term_list = split('\0', $params{term});
my $action = $params{action};
my $ses_type = 'amigo_message';
my $msg_h;
#
# Perform the query
#

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);
if (!@term_list || !$action)
{	$session->suicide_message($msg_h, 'no_term') if !@term_list;

	$session->suicide_message($msg_h, 'no_term_action') if !$action;
}
else
{	my $location = "term=" . join("&term=", @term_list);
	if ($action eq 'plus_node')
	{	$location .= "&action=set-tree";
	}
	elsif ($action eq 'reset-tree')
	{	print STDERR "session before: ".Dumper($session)."\n";
		$session->graph_sync;
		print STDERR "session after: ".Dumper($session)."\n";
		$session->save_session;
		$location .= "&action=set-tree";
	}
	elsif ($action eq 'obo')
	{
	
	}
	elsif ($action eq 'rdfxml')
	{	
	
	}
	elsif ($action eq 'goslim')
	{	#	use these terms as a GO slim
	
	}
	else
	{	$session->suicide_message($msg_h, 'no_term_action');
	}

	print "Location: ".$session->get_default_param('cgi_url')."/browse.cgi?$location&session_id=".$session->id."\n\n";
	exit;
}
$session->__save_session;

