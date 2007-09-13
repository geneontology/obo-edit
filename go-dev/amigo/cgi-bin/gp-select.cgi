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
use GO::CGI::Query qw(get_fasta);
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
my $action = $params{action};
my $ses_type = 'amigo_message';

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);

my %action_hash = (
	'gp-assoc' => sub {
		my $ses = shift;
		my $gp_list = shift;
		print "Location: ".$ses->get_default_param('cgi_url')."/gp-assoc.cgi?show_blast_scores=on&gp=".join("&gp=", @$gp_list)."&session_id=".$ses->id."\n\n";
	},
	'rdfxml' => sub {
		my $ses = shift;
		my $gp_list = shift;
		print "Location: ".$ses->get_default_param('cgi_url')."/gp-assoc.cgi?gp=".join("&gp=", @$gp_list)."&format=rdfxml&session_id=".$ses->id."\n\n";
	},
	'go_assoc' => sub {
		my $ses = shift;
		my $gp_list = shift;
		print "Location: ".$ses->get_default_param('cgi_url')."/gp-assoc.cgi?gp=".join("&gp=", @$gp_list)."&format=go_assoc&session_id=".$ses->id."\n\n";
	},
	'fasta' => sub {
		my $ses = shift;
		my $gp_list = shift;
		my $msg_hash = shift;
		my $result_h = get_fasta($ses->apph, $msg_hash, $gp_list);
		if ($result_h->{results})
		{	my $seqs = $result_h->{results};
			my $warning = $result_h->{msg_h}{warning} if $result_h->{msg_h}{warning};
			print "Content-type:text/plain\n\n";
			
			if ($warning)
			{	print STDERR "message:\n".Dumper($warning)."\n";
				if ($warning->{gp_not_found})
				{	print "Warning: the following gene products were not found in the database:\n".join(", ", @{$warning->{gp_not_found}})."\n\n";
				}
				if ($warning->{no_seq})
				{	print "Warning: the following gene products do not have a sequence present in the database:\n".join(", ", @{$warning->{no_seq}})."\n\n";
				}
			}
			foreach my $s (@$seqs)
			{	print $s->to_fasta."\n";
			}
		}
		else
		{	#	fatal error; print out the error
			process_page_template($result_h, $session);
		}
	},
);

my $msg_h;

if (!@gp_list || !$action || !$action_hash{$action})
{	$msg_h = set_message($msg_h, 'fatal', 'no_gp') if !@gp_list;
	$msg_h = set_message($msg_h, 'fatal', 'no_gp_action') if !$action;
	$msg_h = set_message($msg_h, 'fatal', 'invalid_action') if ($action && !$action_hash{$action});
	process_page_template({ msg_h => $msg_h }, $session);
}
else
{	$action_hash{$action}->($session, \@gp_list, $msg_h);
}

exit;
