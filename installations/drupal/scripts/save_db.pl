#!/usr/bin/perl -w
####
#### Dumps the specified mysql db to a specified location. Makes sure
#### that the copied directory has a unique name.
####

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

## Embedded help through perldoc.
if( $opt_h ){
  system('perldoc', __FILE__);
  exit 0;
}

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


=head1 NAME

save_db.pl

=head1 SYNOPSIS

save_db.pl [-h] [-v] -s <server> -d <dbname> -t <location> [-a <address>]

=head1 DESCRIPTION

Dumps the specified mysql db to a specified location.

=head1 OPTIONS

=over

=item -h

Print this message.

=item -v

Enable more verbose messages. Useful for error checking.

=item -s <location>

Database server.

=item -d <dbname>

Database name.

=item -t <location>

Location of the dump directory

=item -a <address>

Email address for completion notice (think cron-type jobs)
