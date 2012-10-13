#!/usr/bin/perl -w
####
#### TODO: ???
#### BUG: ???
####

use utf8;
use strict;
use File::Basename;
use File::Find;
use Cwd;
use Data::Dumper;
use vars qw(
	     $opt_h
	     $opt_v
	     $opt_i
	     $opt_o
	     $opt_n
	     $opt_d
	     $opt_r
	  );
use Getopt::Std;


## Since internal checks are done, get ready for user input.
getopts('hvi:o:n:d:r:');

## Embedded help through perldoc.
if( $opt_h ){
  system('perldoc', __FILE__);
  exit 0;
}

## Check our options.
ll("Will print verbose messages.");

## Should only be run in the top directory.
if ( ! -f "./scripts/release-js.pl" ){
  ll("This does not seem to be the correct base directory!");
  ll("Please run this script from the base like: ./scripts/release-js.pl");
  exit 0;
}

## Make sure we have the necessary flags to get our input and output
## in the right places.
die "need -i option--use -h flag for help" if ! $opt_i;
die "need -o option--use -h flag for help" if ! $opt_o;
die "need -n option--use -h flag for help" if ! $opt_n;
die "need -d option--use -h flag for help" if ! $opt_d;
die "need -r option--use -h flag for help" if ! $opt_r;

###
### Get oriented.
###

my $base = getcwd();
my $file_map_fname = $base . '/' . $opt_i;
my $bundle_output_fname = $base . '/' . $opt_o;
my $namespace = $opt_n;
my $version_info_dest_fname =  $base . '/' . $opt_d;
my $revision = $opt_r;

## Verify what we can.
if( ! -f $file_map_fname ){
  die "Could not find file map";
}else{
  ll("Input map: " . $file_map_fname);
}

###
### Generate the version file and put it in the right place.
###

## Generate a release date.
my ($rday, $rmonth, $ryear) = (localtime)[3,4,5];
my $release = sprintf "%.4d%.2d%.2d", $ryear+1900, $rmonth+1, $rday;
version_to_js($revision, $release, $namespace, $version_info_dest_fname);

###
### Now we're going to try grabbing all of the mapped JS files and tossing
### them into a single file.
###

## Open final target.
my $exported_file_count = 0;
open(OUTFILE, ">$bundle_output_fname") or
  die "cannot open $bundle_output_fname: $!";

## Cycle through all of the mapped filenames.
open(MAPFILE, "<$file_map_fname") or die "cannot open $file_map_fname: $!";
while( my $js_file_fname = <MAPFILE> ){

  ## Go through each js file line by line.
  open(JSFILE, "<$js_file_fname") or die "cannot open $js_file_fname: $!";
  while( <JSFILE> ){

    ## TODO: Remove bbop.core.require lines from the input.

    ## Dump to target site.
    print OUTFILE $_;
  }
  $exported_file_count++;
  close(JSFILE);
}

close(MAPFILE);
close(OUTFILE);

ll("Created JS lib file: ". $bundle_output_fname .
   ' with '. $exported_file_count .' files(s).');

###
### Try to transform our single file into minified/versioned versions.
###

## TODO:

  # ## Drop one bundle in the HTDOCS dir.
  # my $bundle_output_fname = $env_conf{AMIGO_HTDOCS_ROOT_DIR} .
  #   '/javascript/bbop-amigo.js';

  # ## Drop one bundle in HTDOCS for ourselves.
  # my $bundle_output_fname = $env_conf{AMIGO_HTDOCS_ROOT_DIR} .
  #   '/javascript/bbop-amigo.js';
  # drop_js_files($bundle_output_fname, $export_js_files);

  # ## Drop the head version in the staging dir.
  # my $head_out_fname = $staging_dir . '/bbop-amigo.js';
  # drop_js_files($head_out_fname, $export_js_files);
  # ## And compress it for CDN life.
  # make_compressed_js($head_out_fname, $staging_dir .'/bbop-amigo.min.js');

  # ## Drop a versioned version in the staging dir.
  # my $rel_out_fname = $staging_dir . '/bbop-amigo_' . $release_version . '.js';
  # drop_js_files($rel_out_fname, $export_js_files);
  # ## And compress it for CDN life.
  # make_compressed_js($rel_out_fname,
  # 		     $staging_dir .'/bbop-amigo_'. $release_version .'.min.js');

  ## TODO: just convert this by the command line--no reading tricks
  ## TODO: check that we have the yui-compressor utility.
  #die;

###
### Helper functions.
###

## Create a JS file: version.js
sub version_to_js {

  ## Incoming argument is the version.
  my $revision = shift || die 'wot? we need a revision argument';
  my $release = shift || die 'wot? we need a release argument';
  my $namespace = shift || die 'wot? we need a namespace argument';
  my $location = shift || die 'wot? we need a location argument';

  ## If the file is already there, blow it away.
  unlink $location if -f $location;
  open(FILE, ">$location") or die "cannot open $location: $!";

  ## 
  print FILE <<EOJS;
/* 
 * Package: version.js
 * 
 * Namespace: $namespace.version
 * 
 * This package was automatically created during the release process
 * and contains its version information--this is the release of the 
 * API that you have.
 */

/*
 * Variable: version
 *
 * The version information for this library. It contain the revision
 * (major/minor version numbers) and release (date-like) information.
 */
$namespace.version = {}
$namespace.version['revision'] = "$revision";
$namespace.version['release'] = "$release";
EOJS

  ## Close file.
  close(FILE);
  make_readable($js_file_str);
  ll("Created release version file: \"$location\".");
}


# ##
# sub safe_make_dir {

#   my $dir_to_make = shift || die "no first arg";

#   ## Make the new session directory if necessary.
#   if ( ! -e  $dir_to_make ) {
#     my @args = ("mkdir", $dir_to_make);
#     ll("System: \"@args\"");
#     system(@args) == 0 || die "system @args failed: $?" if ! $opt_t;
#   }
# }


# ##
# sub force_copy {

#   my $from = shift || die "no first arg";
#   my $to = shift || die "no second arg";

#   #my @args = ("cp", "-r", "-f", $from, $to);
#   my @args = ("rsync", "-r",
# 	      "--exclude=.svn",
# 	      "--exclude=.emacs.desktop",
# 	      $from, $to);
#   ll("System: \"@args\"");
#   system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;
# }


# ##
# sub make_executable {

#   my $file = shift || die "no first arg";

#   my @args = ("chmod", "a+x", $file);
#   ll("System: \"@args\"");
#   system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;
# }


# ##
# sub make_permissive {

#   my $file = shift || die "no first arg";

#   my @args = ("chmod", "777", $file);
#   ll("System: \"@args\"");
#   system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;
# }


##
sub make_readable {

  my $file = shift || die "no first arg";

  my @args = ("chmod", "644", $file);
  ll("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;
}

## TODO/BUG: ALPHAish experiment.
sub make_compressed_js {

  my $in_file = shift || die "no first arg";
  my $out_file = shift || die "no second arg";

  #my @args = ("shrinksafe", $in_file, ">", $out_file);
  my @args = ("yui-compressor", "--nomunge", "--type", "js",
	      "-o", $out_file, $in_file);
  ll("System: \"@args\"");
  system(@args) == 0 || die "System \"@args\" failed: $?" if ! $opt_t;
}

## Just a little printin' when feeling verbose.
sub ll {
  my $str = shift || '';
  print $str . "\n" if $opt_v;
}

# ## Just a little printin' when feeling afraid.
# sub ww {
#   my $str = shift || '';
#   print STDERR $str . "\n";
# }


=head1 NAME

release-js.pl

=head1 SYNOPSIS

release-js.pl [-h] [-v] -i <path> -o <output directory/name (full path) for the release> -n <namespace to use in the version file> -d <version.js file destination>

=head1 DESCRIPTION

This is the main AmiGO installation script--it moves files to the
proper location with the proper permissions.

Please see README.txt and INSTALL.txt for more details.

=head1 OPTIONS

=over

=item -v

Enable more verbose messages. This is useful for checking installation errors.

=item -h

Print this help message.

=item -i

Full path to the input map. This file contains the location of the
files to release, one per line and relative to the base of the
checkout.

=item -o

Full path for the location of the JS release bundle file. This will be
modified for minification and versioning.

=item -n

The namespace of the versioning file. ".version" will be added to the
end of it.

=item -d

The destination of the version file.

=item -r

The major.minor version number to use (e.g. "0.9", "1.2").

=back

=head1 SEE ALSO

http://wiki.geneontology.org/index.php/AmiGO_2

=cut
