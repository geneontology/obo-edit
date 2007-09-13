#!/usr/bin/perl5.8.6 -w

use strict;
use warnings;

#
# Must basically launch blast command contained in a file
# and write results back to a file in the same directory
#

use CGI qw(:standard);

#system ("touch /tmp/gost-sgd_started");
my $tmp_dir = $ENV{DOCUMENT_ROOT}."/tmp";
#my $data_dir = param('data_dir');
my $blast_dir = param('blast_dir');
my $command_file = "$blast_dir/command.sh";
my $output_file = "$blast_dir/result";

my $tmp_file = $tmp_dir."/$$.blast";

print header;
print start_html();
system ("/bin/sh $command_file &");

print $?, "\n";
print end_html();

#system ("touch /tmp/gost-sgd_ran");
