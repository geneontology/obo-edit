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
     -s <location> Database server.
     -d <dbname>   Database name.
     -t <location> Location of the dump directory
     -a <address>  Address for notice

  Example usage:
     /users/sjcarbon/local/src/cvs/obo/website/utils/save-db.pl -v -s localhost -d wikidb -t /users/sjcarbon/.Trash -a sjcarbon\@berkeleybop.org

EOC

}else{

  ## Check our options.
  print "Will print verbose messages.\n" if $opt_v;

  ## Check our arguments.
  die "No server defined, use -s option." if ! $opt_s;

  die "No database name defined, use -d option." if ! $opt_d;

  die "No trash directory defined, use -t option." if ! $opt_t;
  die "Specified trash directory not a directory: $!" if ! -d $opt_t;
  die "Specified trash directory not writable: $!" if ! -W $opt_t;

  my $database_server = $opt_s;
  my $database_name = $opt_d;
  my $trash_dir = $opt_t;

  ## Bark.
  print "Database server: $database_server\n" if $opt_v;
  print "Database name: $database_name\n" if $opt_v;
  print "Trash: $trash_dir\n" if $opt_v;

  ## Go!
  my $cmd = 'mysqldump -h ' . $database_server . ' ' . $database_name . ' > ' . $trash_dir . '/' . $database_name . '.' . randy();
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
