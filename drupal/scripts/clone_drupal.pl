#!/usr/bin/perl -w

# updates the ontology and defs files
# *************************************************************

use strict;
use vars qw(
            $opt_h
            $opt_v
            $opt_s
            $opt_d
            $opt_t
            $opt_a
           );
use Getopt::Std;
use File::Temp;

getopts('hvs:d:t:a:');

if( $opt_h ){

  print <<EOC;
  Usage:
     save-db.pl [-h] [-v] -s <server> -d <dbname> -t <location> [-a <address>]

  Options:
     -h            Print this message.
     -v            Enable more verbose messages. Useful for error checking.
     -d <location> Location of the drupal directory
     -t <location> Location of the dump directory

  Example usage:
     TODO

EOC

}else{

  ## Check our options.
  print "Will print verbose messages.\n" if $opt_v;

  ## Check our arguments.
  die "No drupal directory defined, use -d option." if ! $opt_d;
  die "No dump directory defined, use -t option." if ! $opt_t;

  die "Specified drupal directory not a directory: $!" if ! -d $opt_d;
  die "Specified dump directory not a directory: $!" if ! -d $opt_t;
  die "Specified dump directory not writable: $!" if ! -W $opt_t;

  my $drupal_dir = $opt_d;
  my $dump_dir = $opt_t;

  ## Bark.
  print "Drupal: $drupal_dir\n" if $opt_v;
  print "Dump: $dump_dir\n" if $opt_v;

  ## Go!
  my $cmd = 'cp -p -r ' . $drupal_dir . ' ' . $dump_dir . '/drupal_clone' . '.' . randy();
  run($cmd);
  print "Done.\n" if $opt_v;

  ## Super secret option.
  if ( $opt_a) {
    run('echo "Success." | nail -s "' . $cmd . '" ' . $opt_a);
  }
}


###################################################
####              subroutine zone              ####
###################################################


##
sub run{
  my $command = shift;
  die "No command given: $!" if ! $command;
  print "Starting: ($command).\n" if $opt_v; # sorry about the scoping
  ! system($command)
    or die "Failure: ($command): $!";
};


## Generate a random ass number for shoveling.
sub randy {
  #my $numstr = '';
  #for( my $i = 0; $i < 10; $i++ ) {
  #  $numstr .= int(rand(10));
  #}
  #my $tstr = join('',localtime());
  #my $trash_num = $tstr . '_' . $numstr;
  #my $trash_num = join('',localtime());
  my $trash_num = scalar(localtime());
  $trash_num =~ tr/ /_/;
  $trash_num =~ tr/:/./;

  return $trash_num;
}
