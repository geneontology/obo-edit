#!/usr/bin/perl -w
####
#### Run this after installation or after a DB update.
#### Usage:
####    reset; time perl ./refresh.pl
####
#### Note: Test::WWW::Mechanize::CGIApp seems very handy for
#### auto-testing...but in this case, we're just stealing some of the
#### mechanism to get our data with depending on silly ol' apache.
####

BEGIN { require "config.pl"; }
use lib $ENV{GO_ROOT} . '/go-perl';
use lib $ENV{GO_ROOT} . '/go-db-perl';
use lib $ENV{GO_ROOT} . '/amigo/perl';
use lib $ENV{GO_SVN_ROOT} . '/gobo-dbic';

## Bring in necessaries.
use utf8;
use strict;
use Data::Dumper;
#use Test::More qw(no_plan);
use Test::WWW::Mechanize::CGIApp;
use AmiGO::WebApp::HTMLClient;
use Getopt::Std;

#BEGIN { plan tests => 0 }

use vars qw(
	     $opt_h
	     $opt_c
	     $opt_s
	     $opt_g
	     $opt_p
	     $opt_l
	  );

## Setup.
getopts('chsgpl');
my $core = AmiGO->new();
my @errors = ();

## Embedded help through perldoc.
if( $opt_h ){
  system('perldoc', __FILE__);
  exit 0;
}

## Take care of arguments.
my $do_cache = 0;
my $do_summary = 0;
my $do_svg = 0;
my $do_png = 0;
my $do_lucene = 0;
if ( $opt_c ){ $do_cache = 1; }
if ( $opt_s ){ $do_summary = 1; }
if ( $opt_g ){ $do_svg = 1; }
if ( $opt_p ){ $do_png = 1; }
if ( $opt_l ){ $do_lucene = 1; }

## Nothing at all? Then do everything.
if ( ! $opt_c &&
     ! $opt_s &&
     ! $opt_g &&
     ! $opt_p &&
     ! $opt_l ){
  $do_cache = 1;
  $do_summary = 1;
  $do_svg = 1;
  $do_png = 1;
  $do_lucene = 1;
}

#$core->kvetch("Will do cache (" . $do_cache . ", " . $opt_c . ")");
#$core->kvetch("Will do summary (" . $do_summary . ", " . $opt_s . ")");
#$core->kvetch("Will do graphs (" . $do_graphs . ", " . $opt_g . ")");

###
### Update old cache files...
###

if( $do_cache ){

  ll("Making cache files, please wait...");

  ## Species.
  my @args = ("perl",
	      $core->amigo_env('GO_ROOT') . "/amigo/scripts/make_spec_key.pl",
	      $core->amigo_env('CGI_ROOT_DIR'), "50");
  $core->kvetch("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?";

  ## Misc.
  @args = ("perl",
	   $core->amigo_env('GO_ROOT') . "/amigo/scripts/make_misc_key.pl",
	   $core->amigo_env('CGI_ROOT_DIR'));
  $core->kvetch("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?";

  ## 
  @args = ("perl",
	   $core->amigo_env('GO_ROOT') . "/amigo/scripts/make_dblinks.pl",
	   '-f', $core->amigo_env('CGI_ROOT_DIR'));
  $core->kvetch("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?";

  ## Generated JS meta-data.
  @args = ("perl", "./scripts/make_go_meta_js.pl",
	   $core->amigo_env('AMIGO_HTDOCS_ROOT_DIR') .
	   '/js/org/bbop/amigo/go_meta.js');
  $core->kvetch("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?";

  ll("Finished making cache files.");

  ## Places for the new speed caches (clean out the old ones)
  @args = ("perl", "./scripts/make_caches.pl");
  $core->kvetch("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?";

  ll("Finished cleaning old cache files.");
}

###
### Update RG SVG/PNG graphs...
###

my @formats = ();
push @formats, 'svg' if $do_svg;
push @formats, 'png' if $do_png;
if( scalar(@formats) ){

  $core->kvetch("Making graphs, please wait...");

  foreach my $format (@formats){

    my $hs_q = GOBO::DBIC::GODBModel::Query->new({type=>'homolset'});
    my $all_hs = $hs_q->get_all_results();
    $core->kvetch("Snagged a total of " . scalar(@$all_hs) . " homolsets.");
    foreach my $hs (@$all_hs){

      my $set = $hs->id;

      my $fname_no_cache = $core->get_interlink({mode=>'homolset_graph',
						 arg=>{set=>$set,
						       cache=>'no',
						       format=>$format}});
      my $url_no_cache = $core->amigo_env('CGI_URL') . '/' . $fname_no_cache;

      $core->kvetch("Will try: \"$url_no_cache\"");

      ## Get it internally instead of externally.
      my $mech = Test::WWW::Mechanize::CGIApp->new();
      $mech->app("AmiGO::WebApp::HTMLClient");
      $mech->get_ok($fname_no_cache);

      if ( ! $mech->success() ){
	$core->kvetch("Failed with \"$url_no_cache\"");
	push @errors, "ERRORS: $fname_no_cache (" .
	  $mech->response->status_line . ") ... [$!]";
      }else{

	my $output = $mech->content();

	## Output to output dir.
	my $fname = $core->get_interlink({mode=>'homolset_graph',
					  arg=>{set=>$set,
						format=>$format}});
	my $out_fname = $core->amigo_env('PRE_RENDER_DIR') . '/' . $fname;
	open(FILE, "> $out_fname")
	  || die "Couldn't open output file: " . $out_fname;
	print FILE $output;
	close FILE;

	$core->kvetch("Wrote to: \"$out_fname\"");
      }
    }
  }
}

###
### Update RG summary page...
###

if( $do_summary ){

  $core->kvetch("Making summary, please wait...");

  my $summary_try_link = $core->get_interlink({mode=>'homolset_summary',
					       arg=>{cache=>'no'}});
  my $try_url = $core->amigo_env('CGI_URL') . '/' . $summary_try_link;

  $core->kvetch("Will try: \"$try_url\"");

  ## Get it internally instead of externally.
  #$mech->get($try_url);
  my $mech = Test::WWW::Mechanize::CGIApp->new();
  $mech->app("AmiGO::WebApp::HTMLClient");
  $mech->get_ok($summary_try_link);

  if ( ! $mech->success() ){
    $core->kvetch("Failed with \"$try_url\"");
    push @errors, "ERRORS: $try_url (" .
      $mech->response->status_line . ") ... [$!]";
  }else{

    my $output = $mech->content();

    ## Output to output dir.
    my $summary_fname = $core->get_interlink({mode=>'homolset_summary'});
    my $out_fname = $core->amigo_env('PRE_RENDER_DIR') . '/' . $summary_fname;
    open(FILE, "> $out_fname")
      || die "Couldn't open output file: " . $out_fname;
    print FILE $output;
    close FILE;

    #$core->kvetch("\t...got it.");
  }
}

$core->kvetch('Number of errors : ' . scalar(@errors));
foreach my $error (@errors){
  $core->kvetch($error);
}


##
if( $do_lucene ){

  $core->kvetch("Making lucene indexes, please wait...");

  ## Add new indexes; no args needed--from here, luigi knows where to go.
  my @args = ("perl", $core->amigo_env('GO_ROOT') . "/amigo/scripts/luigi");
  $core->kvetch("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?";

  ## TODO/BUG: check/purge old ones.
  

  $core->kvetch("Finished indexing.");
}


###
### Subs.
###

## Just a little printin' when feeling verbose.
sub ll {
  my $str = shift || '';
  print $str . "\n";
}


=head1 NAME

refresh.pl

=head1 SYNOPSIS

refresh.pl [-h] [-c] [-s] [-g] [-p]

=head1 DESCRIPTION

This script creates caches for some of the various
subsystems. Required caches are also created during the installation
process (install.pl) using this script (with just the "-c" option).

As a stand-alone, this script is useful for refreshing caches and
taking some of the load off of the processing needed for Reference
Genome subsystems (which probably aren't really necessary for most
installations of AmiGO, but very much necessary for ones that use the
RG as supplied in the GO).

=head1 OPTIONS

Please note that if you don't specify any of the options, they are all
assumed to be on.

=over

=item -c

Regenerate the species and other misc. caches that were generated
during the initial AmiGO installation process.

=item -s

Generate (or regenerate) the cache file for the Reference Genome
summary page. For this page to be useful, you will probably have to
run this at some point as the page is very large indeed.

=item -g

Generate (or regenerate) the cache files for all of the Reference
Genome interactive graphs (SVGs).

=item -p

Generate (or regenerate) the cache files for all of the Reference
Genome static graphs (PNGs).

=back

=head1 SEE ALSO

http://wiki.geneontology.org/index.php/AmiGO_Manual:_Installation

=cut
