#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

print STDERR "\n\nStarting term-chart.cgi\n";

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
#use FileHandle;
#use GO::SqlWrapper qw(select_hashlist);
use GO::CGI::Query qw(get_data_for_chart);
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);
#use GO::CGI::NameMunger;
#use GO::Template::Template;
#use GO::Utils qw(rearrange);

use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

#	for debugging
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

my $q = new CGI;
my %params = $q->Vars;
my @term_list = split('\0', $params{term}) if $params{term};
my $msg_h;
my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>'term_chart');

#
# Perform the query
#

my $vars;
my $show_term_counts = $session->show_counts('term');

if (!@term_list)
{	$vars->{msg_h} = set_message($msg_h, 'fatal', 'no_term');
}
elsif (!$show_term_counts)
{	$vars->{msg_h} = set_message($msg_h, 'fatal', 'bad_filters');
	$vars->{term} = $term_list[0];
}
else
{	#	only accept the first term if the list is longer than 1
	if (scalar @term_list > 1)
	{	$msg_h = set_message($msg_h, 'warning', 'only_one_term');
	}
	my $result_h = get_data_for_chart(
			$session->apph,     # apph
			$term_list[0],      # acc
			$msg_h,             # msg_h
			{ session_id => $session->id,
			  gp_count_ok => $session->gp_count_ok,
			  show_term_counts => $show_term_counts,
			}
	);
	$vars->{msg_h} = $result_h->{msg_h};
	$vars->{data} = $result_h->{results} if $result_h->{results};
}

print STDERR "msg_h: ".Dumper($vars->{msg_h});

#
# Select and process the Template.
#

process_page_template($vars, $session);
exit;
