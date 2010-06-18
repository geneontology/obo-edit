#!/usr/bin/perl -w
####
#### Interactively maintains a drupal installation in a given
#### directory. Requires that drush is in the path somewhere.
####
#### This aims to be a one-stop maintenance script for our drupal
#### installations and a complete replacement for the (hijacked from
#### other places) save_db.pl and drupal_clone.pl scripts.
####

use strict;
use vars qw(
            $opt_h
            $opt_v
            $opt_d
            $opt_b
           );
use Getopt::Std;
use Time::Format;
use IO::Compress::Gzip qw(gzip $GzipError);
use File::Find;
use Archive::Tar;

# my $fh = File::Temp->new();
# my $fname = $fh->filename;
# use File::Temp;

getopts('hvs:d:b:');

## Embedded help through perldoc.
if( $opt_h ){
  system('perldoc', __FILE__);
  exit 0;
}

## Check our options.
print "Will print verbose messages.\n" if $opt_v;

## Check our drupal argument.
die "No drupal directory defined, use -d option." if ! $opt_d;
die "Specified drupal directory not a directory: $!" if ! -d $opt_d;
my $drupal_dir = $opt_d;
print "Drupal installation at: $drupal_dir\n" if $opt_v;

## Check our backup argument and prep a new backup directory in the
## backup directory.
die "No backup directory defined, use -b option." if ! $opt_b;
die "Specified backup directory not a directory: $!" if ! -d $opt_b;
die "Specified backup directory not writable: $!" if ! -W $opt_b;
my $backup_dir = $opt_b . '/' . randy();
mkdir $backup_dir;
die "Created backup directory not writable: $!" if ! -W $backup_dir;
print "Backups at: $backup_dir\n" if $opt_v;

## Bring down site.
{
  my $cmd = "drush --yes  -r $drupal_dir vset site_offline 1";
  run($cmd);
}

## Make sure that the cron jobs are up to date and display status.
{
  my $cmd = "drush -r $drupal_dir cron";
  run($cmd);
  $cmd = "drush -r $drupal_dir status";
  run($cmd);
}

## Zipped SQL dump.
{
  my $dump_file = "$backup_dir/dump.sql";
  my $zipped_dump_file = "$backup_dir/dump.sql.gz";
  my $cmd = "drush -r $drupal_dir sql-dump > $dump_file";
  run($cmd);
  gzip $dump_file => $zipped_dump_file or die "gzip failed: $GzipError\n";
  unlink $dump_file;
}

## Zipped file tree clone.
{
  my $tarball_dump_dir = "$backup_dir/file_tree.tar.gz";

  ## Gather files with Find::File.
  my @drupal_files = ();
  find(
       sub {
	 push @drupal_files, $File::Find::name
       }, $drupal_dir);

  ## Tarball 'em.
  my $tballer = Archive::Tar->new();
  $tballer->create_archive($tarball_dump_dir, COMPRESS_GZIP, @drupal_files);
}

## Total upgrade...
{
  my $cmd = "drush -r $drupal_dir pm-update";
  run($cmd);
}

## Restart site.
{
  my $cmd = "drush --yes  -r $drupal_dir vset site_offline 0";
  run($cmd);
}

##
print "Done.\n" if $opt_v;


####
#### Subs.
####


##
sub run{
  my $command = shift;
  die "No command given: $!" if ! $command;
  print "Starting: ($command).\n" if $opt_v; # sorry about the scoping
  ! system($command)
    or die "Failure: ($command): $!";
};


## Generate a time-based random string for shoveling.
sub randy {

  my $dt = $time{'yyyymmdd_hhmmss_mmm'};

  ## 
  my $numstr = '';
  # for( my $i = 0; $i < 10; $i++ ) {
  #   $numstr .= int(rand(10));
  # }
  $dt .= '_' . $numstr if $numstr;

  return $dt;
}


=head1 NAME

maintain.pl

=head1 SYNOPSIS

maintain.pl [-h] [-v] -d <path> -b <path>

=head1 DESCRIPTION

Interactively maintains a drupal installation in a given
directory. Requires that drush is in the path somewhere.

This aims to be a one-stop maintenance script for our drupal
installations and a complete replacement for the (hijacked from other
places) save_db.pl and drupal_clone.pl scripts.

=head1 OPTIONS

=over

=item -h

Print this message.

=item -v

Enable more verbose messages. Useful for error checking.

=item -d <path>

Location of the drupal installation.

=item -b <path>

Location of the writable backup directory.

=back

=head1 EXAMPLE

/home/user/local/bin/maintain.pl -v -d /srv/www/htdocs/drupal -b /tmp
