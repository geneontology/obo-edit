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
            $opt_o
            $opt_b
           );
use Getopt::Std;
use Time::Format;
use IO::Compress::Gzip qw(gzip $GzipError);
use File::Copy;
use File::Find;
use Archive::Tar;

# my $fh = File::Temp->new();
# my $fname = $fh->filename;
# use File::Temp;

getopts('hvbd:o:');

## Embedded help through perldoc.
if( $opt_h ){
  system('perldoc', __FILE__);
  exit 0;
}

## Check our options.
print "Will print verbose messages.\n" if $opt_v;

## Are we only doing backups?
my $just_backups_p = 0;
$just_backups_p = 1 if $opt_b;
if( $just_backups_p ){
  print "Will only do backups.\n";
}else{
  print "Will do backups and updates.\n";
}

## Check our drupal argument.
die "No drupal directory defined, use -d option." if ! $opt_d;
die "Specified drupal directory not a directory: $!" if ! -d $opt_d;
my $drupal_dir = $opt_d;
print "Drupal installation at: $drupal_dir\n" if $opt_v;

## Check our backup argument and prep a new backup directory in the
## backup directory.
die "No backup directory defined, use -b option." if ! $opt_o;
die "Specified backup directory not a directory: $!" if ! -d $opt_o;
die "Specified backup directory not writable: $!" if ! -W $opt_o;
my $backup_dir = $opt_o . '/' . randy();
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
  my $dump_file = "$backup_dir/file_tree.tar";
  my $zipped_dump_file = "$backup_dir/file_tree.tar.gz";

  ## Gather files with Find::File.
  my @drupal_files = ();
  find(
       sub {
	 push @drupal_files, $File::Find::name
       }, $drupal_dir);

  ## Tarball 'em.
  ## This first one doesn't seem to be present in older versions...so
  ## we fall back to the separated one.
  #$tballer->create_archive($tarball_dump_dir, COMPRESS_GZIP, @drupal_files);
  my $tballer = Archive::Tar->new();
  $tballer->add_files(@drupal_files);
  $tballer->write($dump_file);
  gzip $dump_file => $zipped_dump_file or die "gzip failed: $GzipError\n";
  unlink $dump_file;
}

## Total upgrade...but only if we are not just doing backups.
if( ! $just_backups_p ){

  ## Move the volatile files somewhere nice.
  my @backupable = ('.htaccess', 'robots.txt');
  my $tmp_ext = randy();
  foreach my $bfile (@backupable){
    my $old_file = $drupal_dir . '/' . $bfile;
    my $new_file = $old_file . '.' . $tmp_ext;
    if( -f $old_file ){
      print "Copying current $bfile to: $new_file\n" if $opt_v;
      copy($old_file, $new_file) or die "Copy failed: $!";
      print "Remember to diff $bfile and $new_file\n" if $opt_v;
    }else{
      print "No $bfile to take care of.\n" if $opt_v;
    }
  }

  ## Actually do backup.
  ## Switch to this after we're sure that it's not crazy.
  #my $cmd = "drush --yes -r $drupal_dir pm-update";
  my $cmd = "drush -r $drupal_dir pm-update";
  run($cmd);
}

## Restart site.
{
  my $cmd = "drush --yes -r $drupal_dir vset site_offline 0";
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

maintain.pl [-h] [-v] [-b] -d <path> -o <path>

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

=item -b

Only perform backups--don't fiddle with the drupal installation.

=item -d <path>

Location of the drupal installation.

=item -o <path>

Location of the writable backup output directory.

=back

=head1 EXAMPLES

/home/user/local/bin/maintain.pl -v -d /srv/www/htdocs/drupal -o /tmp
