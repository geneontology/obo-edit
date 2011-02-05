#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

BEGIN
{

	if ( -f "config.pl" )
	{
		require "config.pl";
	}

	# find go perl libraries pre compile time
	if ( defined( $ENV{GO_DEV_ROOT} ) )
	{
		;
	}
	elsif ( -f "../cvs/go-dev/" )
	{
		$ENV{GO_DEV_ROOT} = "../cvs/go-dev";
	}
}

use lib "$ENV{GO_DEV_ROOT}/go-perl";
use lib "$ENV{GO_DEV_ROOT}/go-db-perl";
use lib "$ENV{GO_DEV_ROOT}/amigo/perl";

use strict;
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);

use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

#	for debugging
use Data::Dumper;
$Data::Dumper::Indent   = 1;
$Data::Dumper::Sortkeys = 1;

## Some new stuff to try...
## TODO: there error handling here seems to be a little bit off.
use AmiGO;
my $core = AmiGO->new();

my $verbose = get_environment_param('verbose');
print STDERR "\n\n" if $verbose;

#
# Set up the relevant objects.
#

my $q      = new CGI;
my %params = $q->Vars;
my $session;
my $error;

#print STDERR "\n\n\nStarting term-select.cgi\nq: " . Dumper($q) . "\n" if $verbose;

my @term_list = split "\0", $params{item} if $params{item};

if ( $params{'reset-filters'} && $params{'reset-filters'} == 1 )
{
	$session = new GO::CGI::Session( '-q' => $q, -ses_type => 'amigo_message' );
	$session->save_session;
}

my $url = get_environment_param('cgi_url');

my $session_id_for_url = '';
if ( $params{session_id} )
{
	$session_id_for_url = '&session_id=' . $params{session_id};
}

#
# Perform the query
#

my %action_hash = (
### setting / resetting / amending the tree
	'plus_node' => sub {
		print "Location: " 
		. $url
		. "/browse.cgi?action=set-tree&term="
		. join( "&term=", @term_list )
		. $session_id_for_url . "\n\n";
	},
	'reset-tree' => sub {
		print "Location: " 
		. $url
		. "/browse.cgi?action=reset-tree&term="
		. join( "&term=", @term_list )
		. $session_id_for_url . "\n\n";
	},
	'set-tree' => sub {
		print "Location: " 
		. $url
		. "/browse.cgi?action=set-tree&term="
		. join( "&term=", @term_list )
		. $session_id_for_url . "\n\n";
	},
### tree downloads in various formats
	'rdfxml' => sub {
		print "Location: " 
		. $url
		. "/browse.cgi?action=set-tree&term="
		. join( "&term=", @term_list )
		. "&format=rdfxml"
		. $session_id_for_url . "\n\n";
	},
	'obo' => sub {
		print "Location: " 
		. $url
		. "/browse.cgi?action=set-tree&term="
		. join( "&term=", @term_list )
		. "&format=obo"
		. $session_id_for_url . "\n\n";
	},
	'go_ont' => sub {
		print "Location: " 
		. $url
		. "/browse.cgi?action=set-tree&term="
		. join( "&term=", @term_list )
		. "&format=go_ont"
		. $session_id_for_url . "\n\n";
	},
	'tree' => sub {
		print "Location: " 
		. $url
		. "/browse.cgi?action=set-tree&term="
		. join( "&term=", @term_list )
		. "&format=tree"
		. $session_id_for_url . "\n\n";
	},
	'png' => sub {
		print "Location: " 
		. $url
		. "/browse.cgi?action=set-tree&term="
		. join( "&term=", @term_list )
		. "&format=png"
		. $session_id_for_url . "\n\n";
	},
	'dot' => sub {
		print "Location: " 
		. $url
		. "/browse.cgi?action=set-tree&term="
		. join( "&term=", @term_list )
		. "&format=dot"
		. $session_id_for_url . "\n\n";
	},
### set terms as slimmer inputs
	'slimmer' => sub {
		print "Location: " 
		. $url
		. "/slimmer?slim_list="
		. join( " ", @term_list ) . "\n\n";
	},
### association downloads
	'go_assoc' => sub {
		print "Location: " 
		. $url
		. "/term-assoc.cgi?term="
		. join( "&term=", @term_list )
		. "&format=go_assoc"
		. $session_id_for_url . "\n\n";
	},
	'rdfxml-assoc' => sub {
		print "Location: " 
		. $url
		. "/term-assoc.cgi?term="
		. join( "&term=", @term_list )
		. "&format=rdfxml"
		. $session_id_for_url . "\n\n";
	},
	## Are these redirects?
	'coannot' => sub {
	  print "Location: "
	    . $core->get_interlink({mode => 'gp_with_2_terms',
				    optional => {full => 1},
				    arg => {terms => \@term_list}})
	    . "\n\n";
	},
	'error' => sub {
		if ( !$session )
		{
			$session = new GO::CGI::Session(
				'-q'      => $q,
				-ses_type => 'amigo_message',
				-temp     => 1
			);
		}

		#fatal error; print out the error
		$core->status_error_server();
		process_page_template(
			{
				error       => $error,
				page_title  => 'Term Selector Error',
				term_list   => \@term_list,
				from_select => 1
			},
			$session
		);
	}
);

my %actions;
my $action;
foreach ( split "\0", $params{action} )
{
	$actions{$_}++ if $action_hash{$_};
}

if ( !@term_list )
{
	$error = set_message( $error, 'fatal', 'no_term' );
	$action = 'error';
}
elsif ( !keys %actions )
{    #	too many valid options!
	$action = 'error';
	$error = set_message( $error, 'fatal', 'no_term_action' );
}
elsif ( scalar keys %actions > 1 )
{
	$action = 'error';
	$error = set_message( $error, 'fatal', 'invalid_term_action' );
}
else
{    #	the action is the hash key
	foreach ( keys %actions )
	{
		$action = $_;
		last;
	}
}

#print STDERR "term-select.cgi: action: ". Dumper($action);

if ( !$action_hash{$action} )
{
	$error = set_message( $error, 'fatal', 'invalid_term_action' );
	$action = 'error';
}

$action_hash{$action}->();

exit;
