#!/usr/local/bin/perl

use strict;
use DirHandle;
my $verbose = 1;

my $cgi_path = shift @ARGV;
$cgi_path =~ s/\/$// if $cgi_path; # cut-off last slash

print STDERR "cgi_path: $cgi_path\n" if $verbose;

if (! -f "$cgi_path/config.pl")
{
#unless ($cgi_path) {
	&help();
	exit;
}

eval {
    require "$cgi_path/config.pl";
};

print STDERR "Cleaning up old sessions...\n" if $verbose;

my $cgi_root_dir = $ENV{'AMIGO_CGI_ROOT_DIR'} ||
  die "AMIGO_CGI_ROOT_DIR not defined: $!";
my $session_dir = $ENV{'AMIGO_SESSION_DIR'} ||
  die "AMIGO_SESSION_DIR not defined: $!";
my $session_dir = $cgi_root_dir .'/'. $session_dir;

my $tmp_image_dir = $ENV{'AMIGO_TEMP_IMAGE_DIR'} ||
  die "AMIGO_TEMP_IMAGE_DIR not defined: $!";

my $session_timeout = $ENV{'AMIGO_SESSION_TIMEOUT'} ||
  die "AMIGO_SESSION_TIMEOUT not defined: $!";

my $max_sessions = $ENV{'AMIGO_MAX_SESSIONS'} ||
  die "AMIGO_MAX_SESSIONS not defined: $!";

print STDERR "session_dir: $session_dir\n" if $verbose;
print STDERR "tmp_image_dir: $tmp_image_dir\n" if $verbose;
print STDERR "session_timeout: $session_timeout\n" if $verbose;
print STDERR "max_sessions: $max_sessions\n" if $verbose;
sleep 2;

## The old tests.
if( ! $session_dir ||
    ! $tmp_image_dir ||
    ! $session_timeout ||
    ! $max_sessions ){
  die("cleaner.pl: something still borked!.\n");
}

## Clean out sessions in here
## Clean out temporary images in here.

my $time = time;

foreach my $dir ($session_dir, $tmp_image_dir) {

  my $dh = new DirHandle($dir) ||
    die("cleaner.pl: Could not find the directory $dir\n");

  print STDERR "[IN]" . "$dh" . "\n";

  my @file_l = split ('\n', `ls $dir`);
  if ((scalar @file_l) > $max_sessions) {
    while (my $ses = $dh->read) {

      print STDERR "[CHECK]" . "$dir/$ses" . "\n";

      my @stat = lstat("$dir/$ses");
      my $a = $stat[9];
      if ($time - $a > $session_timeout ) {
	eval {
	  if ($ses && !($ses =~ /\W/)) {
	    if ($ses ne '.' && $ses ne '..' && $ses ne 'data' && $ses ne '') {
	      print STDERR "[COMMAND]" . "rm -rf $dir/$ses" . "\n";
	      `rm -rf $dir/$ses`;
	      #	my $data_dir = get_param('data_dir');
	      #	my $command = "rm -rf $data_dir/$ses"."_blast";
	      #	`$command`;
	    }
	  }
	};
      }
    }
  }
}
print STDERR "Done!\n" if $verbose;

exit;

sub get_param {
	my $var = shift;
	if (defined($ENV{uc("GO_$var")})) {
		return $ENV{uc("GO_$var")};
	}
	return;
}

sub help {
    print <<EOM;
AmiGO cleaner script

Removes old session data and user-generated images

Invocation:

	cleaner.pl amigo_cgi_dir

where amigo_cgi_dir is the full path to the cgi directory where
the AmiGO CGIs and the configuration file config.pl reside

e.g.
	cleaner.pl /www/cgi-bin/amigo

EOM
}
