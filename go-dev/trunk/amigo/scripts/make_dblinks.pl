#!/usr/local/bin/perl
###
### Create the best db_links.pl file we can. Try to get it from CVS
### first, otherwise use what we find in the database.
###
### "-n" forces the use of the database fallback method.
###

## Setup environment.
BEGIN {
  if (-f "config.pl") {
    require "config.pl";
  }

  if (defined($ENV{GO_ROOT})) {
  } elsif (-f "../cvs/go-dev/") {
    $ENV{GO_ROOT} = "../cvs/go-dev";
  }
}
use lib "$ENV{GO_ROOT}/go-perl";
use lib "$ENV{GO_ROOT}/go-db-perl";
use lib "$ENV{GO_ROOT}/amigo/perl";
use lib $ENV{GO_SVN_ROOT} . '/gobo-dbic';

use strict;
use Data::Dumper;
use FreezeThaw qw(freeze thaw);
use GOBO::DBIC::GODBModel::Query;
use Getopt::Std;
use File::Temp qw(tempdir);
use Cwd qw(getcwd);

use vars qw(
	    $opt_h
	    $opt_v
	    $opt_n
	    $opt_f
	  );
getopts('hvnt:f:');

## Get path and tmp args or return help.
unless( $opt_f && -d $opt_f ){
  print <<EOM;
  make_dblinks.pl [-n] -f <amigo cgi dir>
EOM
  exit;
}
my $cgi_path = $opt_f;

###
### Do this through direct download if possible.
###

## 
my $database_info = {};
if( ! $opt_n ){
  eval {

    ll("Trying download...");

    ## Read in file. Don't need commented lines and newlines.
    my $curr_dir = getcwd;
    my $tmp_dir = tempdir( CLEANUP => 1 );
    chdir $tmp_dir or die "Couldn't cd to temp directory: $!";
    run('cvs -q -d:pserver:anonymous@cvs.geneontology.org:/anoncvs checkout go/doc/GO.xrf_abbs');
    open(GO_XRF_ABBS, "<" . 'go/doc/GO.xrf_abbs')
      or die "Couldn\'t find GO.xrf_abbs.";
    my @file_buffer = ();
    while (<GO_XRF_ABBS>) {
      if( ! /^\!/ ){
	push @file_buffer, $_;
      }
    }
    close(GO_XRF_ABBS);
    my $top_chunk_str = join('', @file_buffer);

    ## Split on two or more newlines.
    my @top_chunks = split /\n{2,}/s, $top_chunk_str;

    ## Split on newline.
    foreach my $middle_chunk_str (@top_chunks){
      my @middle_chunks = split /\n/s, $middle_chunk_str;

      ## Split on ':' and trim.
      my $tmp_data =
	{
	 id => undef,
	 abbreviation => undef,
	 name => undef,
	 fullname => undef,
	 datatype => undef,
	 generic_url => undef,
	 url_syntax => undef,
	 url_example => undef,
	 uri_prefix => undef,
	};
      foreach my $line (@middle_chunks){

	$line =~ /(.*)\:\s+(.*)/;
	my $tag = $1;
	my $val = $2;

	$tmp_data->{$tag} = $val;
      }
      ## If it looks like it has a key add it to the hash.
      if( $tmp_data->{abbreviation} ){
	my $key = lc($tmp_data->{abbreviation});
	$database_info->{$key} = $tmp_data;
	ll("Added: $key");
      }
    }
    chdir $curr_dir or die "Couldn't cd to original directory: $!";
  };
}
if(@! || $opt_n ){

  ll("Using DB fallback.");

  ###
  ### As a fallback, grab all information about databases and hashify.
  ###

  my $q = GOBO::DBIC::GODBModel::Query->new({type=>'db'});
  my $results = $q->get_all_results({}, {});

  ## 
  foreach my $db (@$results){
    my $key = lc($db->name) || die "NAMELESS DATABASE IN DB TABLE: $!";
    $database_info->{$key} = {};
    $database_info->{$key}{id} = $db->id;
    $database_info->{$key}{name} = $db->name;
    $database_info->{$key}{fullname} = $db->fullname;
    $database_info->{$key}{datatype} = $db->datatype;
    $database_info->{$key}{generic_url} = $db->generic_url;
    $database_info->{$key}{url_syntax} = $db->url_syntax;
    $database_info->{$key}{url_example} = $db->url_example;
    $database_info->{$key}{uri_prefix} = $db->uri_prefix;
  }
}

ll(Dumper($database_info));

## Freeze our finds.
if( keys %$database_info ){
  my $str = freeze $database_info;
  my $f = "$cgi_path/db_info.pl";
  unlink $f if (-f $f);
  open(W, ">$f") or die "can not open $f: $!";
  print W $str;
  close(W);
}

###
### Helper functions.
###

## Runner lifted from other scripts.
sub run{
  my $command = shift;
  die "No command given: $!" if ! $command;
  ll("Starting: ($command).");
  ! system($command)
    or die "Failure: ($command): $!";
};


## Just a little printin' when feeling verbose.
sub ll {
  my $str = shift || '';
  print $str . "\n" if $opt_v;
}
