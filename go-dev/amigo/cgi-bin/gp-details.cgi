#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

print STDERR "\n\nStarting gp-details.cgi\n";

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
use GO::CGI::Query qw(get_gp_details get_term_count_for_gps);
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);
#use GO::CGI::NameMunger;
use GO::Template::Template;

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
my @gp_list = split "\0", $params{gp};
my $ses_type = 'gp_details';

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);
$session->gp_sync;
$session->save_session;

#
# Perform the query
#

my $gp;
my $msg_h;
my $vars;

if (!@gp_list)
{	$msg_h = set_message($msg_h, 'fatal', 'no_gp');
}
else
{	if (scalar @gp_list > 1)
	{	$msg_h = set_message($msg_h, 'warning', 'only_one_gp');
	}
	my $result_h = get_gp_details( -apph => $session->apph, -msg_h => $msg_h, -constr => { gpxref => $gp_list[0] } );
	$vars->{msg_h} = $result_h->{msg_h};
	$vars->{gp} = $result_h->{results}[0] if $result_h->{results};
}

if ($session->show_counts('gp') == 1)
{	my $count = get_term_count_for_gps($session->apph, [ $vars->{gp} ], 1);
	print STDERR "Count: ".Dumper($count);
	$vars->{term_count} = $count->[1];
}

print STDERR "msg_h: ".Dumper($msg_h);

process_page_template($vars, $session);
exit;
