#!/usr/bin/perl -w

##
##
##

use strict;
use Getopt::Std;
use WWW::Mechanize;
use vars qw(
	    $opt_h
	    $opt_v
	    $opt_u
	    $opt_o
	    $opt_n
	   );

## Sane and easy to modify defaults.
my %local = (
	     URL => 'http://accordion.lbl.gov/cgi-bin/amigo/amigo',

	     OUTPUT_DIR => '/srv/www/htdocs/amigo/pre_render',

	     OUTPUT_NAME => 'output'
	    );

getopts('hvu:o:n:');


##
if ( $opt_h ) {
  usage();
  exit();
}


print "Here.\n";

##
## Preparation with command line arguments.
##

if ( $opt_v ) {
  print "Will be verbose.\n"; }

## Check our options and set variables accordingly.
$local{URL} = $opt_u if $opt_u;
$local{OUTPUT_DIR} = $opt_o if $opt_o;
$local{OUTPUT_NAME} = $opt_n if $opt_n;

## Sanity check.
die "Not a legit output directory:"
  if (! -d $local{OUTPUT_DIR}) || (! -w $local{OUTPUT_DIR});

print "Will use $local{URL} as AmiGO source.\n" if $opt_v;
print "Will direct output to $local{OUTPUT_DIR}.\n" if $opt_v;

##
my $errors = 0;
my $mech = WWW::Mechanize->new();
$mech->timeout(30000);

my $url = $local{URL};
print "Will try: \"$url\"..." if $opt_v;
$mech->get($url);

if ( ! $mech->success() ){
  print "failed.\n" if $opt_v;
  $errors++;
}else{

  my $doc = $mech->content();

  ## Output to output dir.
  my $out_file = $local{OUTPUT_DIR} . '/' . $local{OUTPUT_NAME};
  open(FILE, "> $out_file")
    || die "Couldn't open output file: ";
  print FILE $doc;
  close FILE;

  print "got it.\n" if $opt_v;
}

if( $errors ){
  print "$errors failures occured.\n" if $opt_v;
}else{
  print "No failures occured.\n" if $opt_v;
}


##########
##
## Subroutines.
##
##########


##
sub usage{

  print <<EOC;

  Usage:
     render_page.pl
                 [-h] [-v]
                 [-u <url>]
                 [-o <directory>]
                 [-n <name>]

  General Options:
     -h               Print this message.
     -v               Enable more verbose messages. This is useful for checking
                      installation errors.

  Options:

     -u <url>         URL of a new AmiGO.
     -o <directory>   Output directory.
     -n <name>        Output name.

  Example usage:
     perl render_name.pl -v

  Note:
     ...

EOC

}
