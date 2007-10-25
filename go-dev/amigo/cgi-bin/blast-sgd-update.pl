#!/usr/local/bin/perl -w

require 5.8.0;

use CGI qw(:standard) ;
$CGI::DISABLE_UPLOADS = 1;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);

my $blast_dir = param('blast_dir') || die("blast dir not defined!");
print STDERR "blast dir: ".$blast_dir."\n";
#my $output_file = "$blast_dir/result";
#my $tmp_file = $tmp_dir."/$$.blast";

#my $temp_dir = param('temp_dir');
#my $command_file = "$temp_dir/command.sh";
my $command_file = "$blast_dir/command.sh";

print header;
print start_html();
system ("/bin/sh $command_file &"); # or die("Error: $?");
print $?, "\n";
print end_html();

#system ("touch /tmp/gost-sgd_ran");
