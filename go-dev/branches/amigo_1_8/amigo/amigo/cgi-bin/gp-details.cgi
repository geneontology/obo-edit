#!/usr/local/bin/perl -w

#	#!/usr/bin/perl -w
require 5.8.0;

BEGIN { require "config.pl" if -f "config.pl" ; }
use lib "$ENV{GO_DEV_ROOT}/go-perl";
use lib "$ENV{GO_DEV_ROOT}/go-db-perl";
use lib "$ENV{GO_DEV_ROOT}/amigo/perl";
use lib $ENV{GOBO_ROOT};

use strict;
use FileHandle;
use GO::CGI::Query qw(get_gp_details get_term_count_for_gps);
use GO::CGI::Session;
use GO::CGI::Utilities qw(:std);
#use GO::Template::Template;

use CGI;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

## for debugging
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;


## Some new stuff to try...
#use AmiGO;
use AmiGO::ReferenceGenome;
use AmiGO::Aid::ReferenceGenome;
my $core = AmiGO::Aid::ReferenceGenome->new();

my $verbose = get_environment_param('verbose');
print STDERR "\n\n" if $verbose;

##
## Set up the relevant objects.
##

my $vars;
$vars->{page_title} = 'Gene Product Details';
$vars->{page_title_header} = $vars->{page_title};

my $q = new CGI;
my %params = $q->Vars;
my @gp_list = split "\0", $params{gp} if $params{gp};

if (!@gp_list)
{	$vars->{error} = set_message(undef, 'fatal', 'no_gp');
}
elsif (scalar @gp_list > 1)
{	$vars->{error} = set_message(undef, 'warning', 'only_one_gp');
}

my @valid_formats = ();
#	check we understand the format
if ($params{'format'}) # && !grep { $params{'format'} eq $_ } @valid_formats)
{	$vars->{error} = set_message(undef, 'fatal', 'bad_format', $params{'format'});
}

#	if we've got an error already, die.
if ($vars->{error}{fatal})
{	my $session = new GO::CGI::Session('-q'=>$q, -temp=>1);
	$core->status_error_client();
	process_page_template($vars, $session, 'amigo_message');
	exit;
}

my $ses_type = 'gp_details';

my $session = new GO::CGI::Session('-q'=>$q, -ses_type=>$ses_type, -read_only=>1);
$session->gp_sync;
$session->save_session;

#
# Perform the query
#

my $result_h = get_gp_details( -apph => $session->apph, -error => $vars->{error}, -constr => { gpxref => $gp_list[0] } );
$vars->{error} = $result_h->{error};
if ($result_h->{results}){

  $vars->{gp} = $result_h->{results}[0];
  $vars->{page_title} = $vars->{gp}->symbol;
  $vars->{page_title_header} = $vars->{page_title} . " Details";

  ## New stuff for refgen.
  my $rg = AmiGO::ReferenceGenome->new();
  $vars->{REFGEN_P} = 0;
  if( defined(my $results =
	      $rg->find_refgen_info({gene_product=>$gp_list[0]})) ){
    $vars->{REFGEN_P} = 1;
    $vars->{REFGEN_RHASHES} = $results;

    ## RG species and color on a per set basis.
    $vars->{REFGEN_SPECIES_INFO} = {};
    foreach my $set (@$results){
      my $others = $core->species_information($set->{other_species});
      $vars->{REFGEN_SPECIES_INFO}{$set->{id}} = $others;
      $core->kvetch($set->{id} . ' ' . $others);
    }
  }

  if ($session->show_counts('gp') == 1){
    my $count = get_term_count_for_gps($session->apph, [ $vars->{gp} ], 1);
    #	print STDERR "Count: ".Dumper($count) if $verbose;
    $vars->{term_count} = $count->[1];
  }
}else{
  $session->ses_type('amigo_message');
}

print STDERR "error: ".Dumper($vars->{error}) if $verbose;

process_page_template($vars, $session);
exit;
