#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

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
use GO::CGI::Query qw(get_fasta);
#use GO::Template::Template;

use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

#	for debugging
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

my $verbose = get_environment_param('verbose');
print STDERR "\n\n" if $verbose;

#
# Set up the relevant objects.
#

my $q = new CGI;
my %params = $q->Vars;
my $session;
my $error;

print STDERR "cgi: ".Dumper($q)."\n" if $verbose;

my @gp_list = split "\0", $params{item} if $params{item};

if ($params{'reset-filters'} && $params{'reset-filters'} == 1)
{	$session = new GO::CGI::Session('-q'=>$q, -ses_type=>'amigo_message');
	$session->save_session;
}

my $url = get_environment_param('cgi_url');

my $session_id_for_url = '';
if ($params{session_id})
{	$session_id_for_url = '&session_id=' . $params{session_id};
}


my %action_hash = (
	'gp-assoc' => sub {
		$url .= "/gp-assoc.cgi?gp=".join("&gp=", @gp_list);
		if ($params{show_blast_scores} && $params{show_blast_scores} eq 'on')
		{	$url .= "&show_blast_scores=on";
		}
		print "Location: $url".$session_id_for_url."\n\n";;
	},
	'rdfxml' => sub {
		print "Location: ".$url."/gp-assoc.cgi?gp=".join("&gp=", @gp_list)."&format=rdfxml".$session_id_for_url."\n\n";
	},
	'go_assoc' => sub {
		print "Location: ".$url."/gp-assoc.cgi?gp=".join("&gp=", @gp_list)."&format=go_assoc".$session_id_for_url."\n\n";
	},
	te_gp_list => sub {
		print "Location: ".$url."/term_enrichment?gp_list=".join(" ", @gp_list)."\n\n";
	},
	te_bggp_list => sub {
		print "Location: ".$url."/term_enrichment?bggp_list=".join(" ", @gp_list)."\n\n";
	},
	slimmer_gp_list => sub {
		print "Location: ".$url."/slimmer?gp_list=".join(" ", @gp_list)."\n\n";
	},
	'fasta' => sub {
		my $apph = create_apph;
		my $result_h = get_fasta($apph, $error, \@gp_list);
		if ($result_h->{results})
		{	my $seqs = $result_h->{results};
			my $warning = $result_h->{error}{warning} if $result_h->{error}{warning};
			print "Content-type:text/plain\n\n";
			
			if ($warning)
			{	print STDERR "message:\n".Dumper($warning)."\n" if $verbose;
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
		{	if (!$session)
			{	$session = new GO::CGI::Session('-q'=>$q, -ses_type=>'amigo_message', -apph=>$apph, -temp=>1);
			}
			#	fatal error; print out the error
			process_page_template( { %$result_h, page_title => 'FASTA Sequence Retrieval' }, $session );
		}
	},
	'error' => sub {
		if (!$session)
		{	$session = new GO::CGI::Session('-q'=>$q, -ses_type=>'amigo_message', -temp=>1);
		}
		#	fatal error; print out the error
		process_page_template({ error => $error, page_title => 'Gene Product Selector Error', gp_list => \@gp_list, from_select => 1 }, $session );
	}
);

my %actions;
my $action;
foreach (split "\0", $params{action})
{	$actions{$_}++ if $action_hash{$_};
}

if (!@gp_list)
{	$error = set_message($error, 'fatal', 'no_gp');
	$action = 'error';
}
elsif (!keys %actions)
{	#	too many valid options! 
	$action = 'error';
	$error = set_message($error, 'fatal', 'no_gp_action');
}
elsif (scalar keys %actions > 1)
{	$action = 'error';
	$error = set_message($error, 'fatal', 'invalid_gp_action');
}
else
{	#	the action is the hash key
	foreach (keys %actions)
	{	$action = $_;
		last;
	}
}

#print STDERR "action: ". Dumper($action);

if (!$action_hash{$action})
{	$error = set_message($error, 'fatal', 'invalid_gp_action');
	$action = 'error';
}

$action_hash{$action}->();

exit;
