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
use GO::CGI::NameMunger;
#use GO::Template::Template;

use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

#	for debugging
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;
#$Data::Dumper::Maxdepth = 3;

#
# Set up the relevant objects.
#

my $q = new CGI;
my $ses_type = 'amigo_message';
my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type);

#print STDERR "session:\n".Dumper($session)."\n";

my $params = $session->get_param_hash;
my @spp_list;
@spp_list = split "\0", $params->{spp} if $params->{spp};
my $action = $params->{action} || undef;
my $msg_h;

if (!@spp_list)
{	$msg_h = set_message($msg_h, 'fatal', 'no_spp');
}
elsif ($action eq 'set-filter' || $action eq 'add-filter')
{	if ($action eq 'add-filter')
	{	my %taxid_h;
		#	get the current species filter list
		my $taxids = $session->apph->filters->{taxid};
		if ($taxids)
		{	#	convert into a hash, and add the spp_list
			map { $taxid_h{$_} = 1 } @$taxids;
		}
		@taxid_h{@spp_list} = (1) x @spp_list;
		#	replace @spp_list with the keys from %taxid_h
		@spp_list = keys %taxid_h;
	}

	#	our new species list is @spp_list
	#	check to see if any of the taxids in it don't appear in our standard set
	my %tax_h;
	my @new_spp;
	my $known_tax_l = $session->get_taxid_list;
	$tax_h{$_} = 1 foreach (@$known_tax_l);

	foreach (@spp_list)
	{	push @new_spp, $_ if (!$tax_h{$_});
	}

	$session->set_param(-field => 'taxid', -query=>1, -value=> [@spp_list]);
	$session->set_param(-field => 'custom_taxid', -query=>1, -value=> [@new_spp]);

	$session->apph->filters->{taxid} = [ @spp_list ];
	
	#	Load a page with the new species filters listed
	$msg_h = set_message($msg_h, 'info', 'spp_filters_set', $session->apph->filters->{taxid});
	#print "Location: ".$session->get_param('cgi_url')."/gp-assoc.cgi?gp=".join("&gp=", @gp_list)."&session_id=".$session->get_param('session_id')."\n\n";
	#exit;
}
elsif ($action eq 'download')
{	
}


#use FileHandle;
#my $out = new FileHandle(">-");
#$session->set_output($out);

#
# Perform the query
#


#
# Select and process the Template.
#


my $vars = {
	session => $session,
	munger => $session->munger,
	filterdata => $session->get_data_for_filters,
	page_name => $ses_type,
	session_id => $session->get_param('session_id'),
	html_url => $session->get_param('html_url'),
	image_dir => $session->get_param('html_url').'/images',
};

my $msg = $session->get_message;
$vars->{msg_h} = $msg if $msg;

print "Content-type:text/html\n\n";

GO::Template::Template->process_template($session, "$ses_type.tmpl", $vars);

$session->__save_session(1);
