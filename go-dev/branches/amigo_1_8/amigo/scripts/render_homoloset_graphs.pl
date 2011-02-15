#!/usr/bin/perl -w

##
## TODO: use AmiGO.pm to make files and linking easier.
##

use strict;
use Getopt::Std;
use WWW::Mechanize;
use vars qw(
	    $opt_h
	    $opt_v
	    $opt_f
	    $opt_t
	    $opt_u
	    $opt_o
	    $opt_c
	   );

## Sane and easy to modify defaults.
my %local = (
	     FROM => 1,
	     TO => 375,

	     URL => 'http://accordion.lbl.gov/cgi-bin/amigo/amigo',

	     OUTPUT_DIR => '/srv/www/htdocs/amigo/pre_render',

	     FORMAT => 'svg'
	    );

getopts('hvf:t:u:o:c:');


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
$local{FROM} = $opt_f if $opt_f && $opt_f > 0;
$local{TO} = $opt_t if $opt_t && $opt_t > $local{FROM};
$local{URL} = $opt_u if $opt_u;
$local{OUTPUT_DIR} = $opt_o if $opt_o;
$local{FORMAT} = $opt_c if $opt_c;

## Sanity check.
die "Not a legit output directory:"
  if (! -d $local{OUTPUT_DIR}) || (! -w $local{OUTPUT_DIR});

print "Will start at: $local{FROM}.\n" if $opt_v;
print "Will end at: $local{TO}.\n" if $opt_v;
print "Will use $local{URL} as AmiGO source.\n" if $opt_v;
print "Will direct output to $local{OUTPUT_DIR}.\n" if $opt_v;
print "Format will be $local{FORMAT}.\n" if $opt_v;

##
my $errors = 0;
my $mech = WWW::Mechanize->new();
$mech->timeout(30000);

for ($local{FROM}...$local{TO}){

  my $url = 
    $local{URL}. '?mode=homolset_graph&set=' . $_ . '&format=' . $local{FORMAT};
  my $fname = 
    'amigo?mode=homolset_graph&set=' . $_ . '&format=' . $local{FORMAT};

  print "Will try: \"$url\"..." if $opt_v;
  $mech->get($url);

  if ( ! $mech->success() ){
    print "failed.\n" if $opt_v;
    $errors++;
  }else{

    my $svg = $mech->content();

    ## Output to output dir.
    my $out_file =
      $local{OUTPUT_DIR} . '/' . $fname;
    open(FILE, "> $out_file")
      || die "Couldn't open output file: ";
    print FILE $svg;
    close FILE;

    print "got it.\n" if $opt_v;
  }
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
sub foo {

  ## Init bot.
  my $bar = shift;

  return $bar;
}


##
sub usage{

  print <<EOC;

  Usage:
     render_homolset_graphs.pl
                 [-h] [-v]
                 [-c <format>]
                 [-f <number>] [-t <number>]
                 [-u <url>]
                 [-o <directory>]

  General Options:
     -h               Print this message.
     -v               Enable more verbose messages. This is useful for checking
                      installation errors.

  Options:

     -c <format>      GraphViz format. Should be png, svg, or dot.
     -f <number>      Start number.
     -t <number>      End number.
     -u <url>         URL of a new AmiGO.
     -o <directory>   Output directory.

  Example usage:
     perl render_homolset_graphs.pl -v

     perl ./render_homoloset_graphs.pl -v -f 0 -t 500 -u http://localhost/cgi-bin/amigo/amigo -o /srv/www/htdocs/amigo/pre_render -c svg

  Note:
     ...

EOC

}
