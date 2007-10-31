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
use lib "$ENV{GO_ROOT}/amigo/perl";

use strict;
use FileHandle;
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);
#use GO::Template::Template;

use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

#	for debugging
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;
#$Data::Dumper::Maxdepth = 3;

my $q = new CGI;
my $ses_type = 'prefs';
my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);
my $msg_h;
print STDERR "session before: ".Dumper($session)."\n";

$session->prefs_sync;
$session->save_session;
print STDERR "session after: ".Dumper($session)."\n";
my $vars;
$vars->{extra_filter}{tree_view} = $session->set_single_option('tree_view');
$vars->{msg_h} = $msg_h;
process_page_template($vars, $session);
exit;
