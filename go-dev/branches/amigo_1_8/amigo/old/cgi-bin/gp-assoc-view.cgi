#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
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
use FileHandle;
use GO::CGI::Query qw(get_graph_for_gp get_nit);
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);
#use GO::CGI::NameMunger;
#use GO::Template::Template;

use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

#	for debugging
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

my $q = new CGI;
my %params = $q->Vars;
my @gp_list = split "\0", $params{gp};
my $ses_type = 'gp_assoc_view';

print STDERR Dumper(\@gp_list);

my $msg_h;
my $vars;

#
# Perform the query
#

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);
$session->gp_sync;
$session->save_session;

if (!@gp_list)
{	$msg_h = set_message($msg_h, 'fatal', 'no_gp');
	process_page_template({ msg_h => $msg_h }, $session);
	exit;
}

my $result_h = get_graph_for_gp($session->apph, $msg_h, \@gp_list);
if (!$result_h->{results})
{	process_page_template($result_h, $session);
	exit;
}
$vars->{msg_h} = $result_h->{msg_h};
$vars->{data} = $result_h->{results};

my $compact;
if ($session->get_saved_param('tree_view') && $session->get_saved_param('tree_view') eq 'compact')
{	$compact = 1;
}
$vars->{nit} = get_nit($vars->{data}{graph}, undef, $compact);
$vars->{gp} = \@gp_list;

process_page_template($vars, $session);
exit;
