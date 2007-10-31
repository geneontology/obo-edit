#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
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
use lib "$ENV{GO_ROOT}/amigo/perl";

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
my @term_list = split('\0', $params{item});
my $action = $params{action};
my $ses_type = 'amigo_message';
my $msg_h;
#
# Perform the query
#

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);

my %action_hash = (
	'plus_node' => sub {
		print "Location: ".$session->get_default_param('cgi_url')."/browse.cgi?action=set-tree&term=".join("&term=", @term_list)."&session_id=".$session->id."\n\n";
	},
	'reset-tree' => sub {
		print "Location: ".$session->get_default_param('cgi_url')."/browse.cgi?action=reset-tree&term=".join("&term=", @term_list)."&session_id=".$session->id."\n\n";
	},
	'set-tree' => sub {
		print "Location: ".$session->get_default_param('cgi_url')."/browse.cgi?action=set-tree&term=".join("&term=", @term_list)."&session_id=".$session->id."\n\n";
	},
	'rdfxml' => sub {
		print "Location: ".$session->get_default_param('cgi_url')."/browse.cgi?action=set-tree&term=".join("&term=", @term_list)."&format=rdfxml&session_id=".$session->id."\n\n";
	},
	'obo' => sub {
		print "Location: ".$session->get_default_param('cgi_url')."/browse.cgi?action=set-tree&term=".join("&term=", @term_list)."&format=obo&session_id=".$session->id."\n\n";
	},
	'go_ont' => sub {
		print "Location: ".$session->get_default_param('cgi_url')."/browse.cgi?action=set-tree&term=".join("&term=", @term_list)."&format=go_ont&session_id=".$session->id."\n\n";
	},
	'tree' => sub {
		print "Location: ".$session->get_default_param('cgi_url')."/browse.cgi?action=set-tree&term=".join("&term=", @term_list)."&format=tree&session_id=".$session->id."\n\n";
	},
	'png' => sub {
		print "Location: ".$session->get_default_param('cgi_url')."/browse.cgi?action=set-tree&term=".join("&term=", @term_list)."&format=png&session_id=".$session->id."\n\n";
	},
	'ont' => sub {
		print "Location: ".$session->get_default_param('cgi_url')."/browse.cgi?action=set-tree&term=".join("&term=", @term_list)."&format=ont&session_id=".$session->id."\n\n";
	},
);

my $msg_h;

if (!@term_list || !$action || !$action_hash{$action})
{	$msg_h = set_message($msg_h, 'fatal', 'no_term') if !@term_list;
	$msg_h = set_message($msg_h, 'fatal', 'no_term_action') if !$action;
	$msg_h = set_message($msg_h, 'fatal', 'invalid_action') if ($action && !$action_hash{$action});
	process_page_template({ msg_h => $msg_h }, $session);
}
else
{	$action_hash{$action}->($session, \@term_list, $msg_h);
}
exit;
