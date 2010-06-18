#!/usr/bin/perl -w
####
#### Copies the specified drupal directories to a specified
#### location. Makes sure that the copied directory has a unique name.
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

clone_drupal.pl

=head1 SYNOPSIS

clone_drupal.pl [-h] [-v] -d <path> -t <path> [-a <address>]

=head1 DESCRIPTION

Copies the specified drupal directory to a specified location.

=head1 OPTIONS

=over

=item -h

Print this message.

=item -v

Enable more verbose messages. Useful for error checking.

=item -d <path>

Location of the drupal directory

=item -t <path>

Location of the dump directory

=item -a <address>

Email address for completion notice (think cron-type jobs)
