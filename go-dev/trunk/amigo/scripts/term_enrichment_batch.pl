#!/usr/bin/perl -w

####
#### Script for running batch term enrichment jobs against the beta
#### AmiGO server on ash. A gp_file is just a file containing gene
#### product identifiers separated by whitespace.
####
#### Usage: perl term_enrichment_batch.pl <gp_file1> ... <gp_fileN>
####
#### The output for each batch will be outputted to
#### "gp_fileI.amigo_results", where 1 <= I <= N.
####
#### NOTE/BUG: ash seems to get "tired" after a few, should look into
#### it deeper...
####

use strict;
use LWP::UserAgent;

## Base variables.
my $base_url = "http://amigo.berkeleybop.org/cgi-bin/amigo/term_enrichment";
my $options = "?output=normal&format=tab&min_gps=2&cutoff=0.01&request=results&speciesdb=FB&force=yes&gp_list=";

print STDERR "Will target: $base_url\n";
print STDERR "Will use options: $options\n";

## File loop.
foreach my $file (@ARGV) {

  my $output_file = $file . '.amigo_results';
  print STDERR "Will operate on: $file and output to $output_file\n";

  ## Open file.
  open INFILE, $file or die $!;
  my $gp_string = '';
  while(<INFILE>){
    $gp_string .= $_;
  }
  ## All whitespace minimized spaces...
  $gp_string =~ tr/\n/ /;
  $gp_string =~ s/\s*/ /;
  $gp_string =~ s/^\s//;
  $gp_string =~ s/\s$//;
  $gp_string =~ s/ /%20/g; # ...and spaces to URL safe.

  ## Open output file.
  open OUTFILE, ">", $output_file or die $!;

  ## Run web user agent.
  my $ua = LWP::UserAgent->new;
  $ua->timeout(3000);
  my $full_url = $base_url . $options . $gp_string;
  print STDERR "Will operate hit URL: $full_url\n";
  my $response = $ua->get($full_url);
  if ($response->is_success) {

    ## Output to file.
    print OUTFILE $response->content;

  }else{
     die $response->status_line;
  }

  close INFILE;
  close OUTFILE;

  print STDERR "Napping...\n";
  sleep 2; # a little nap for the AmiGO server...
}
