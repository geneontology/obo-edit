#!/usr/local/bin/perl -w

use strict;

use lib "$ENV{SQLFAIRY_HOME}/lib";

use SQL::Translator;

my $translator = SQL::Translator->new(
				      );
my $DIR = "../modules";

opendir DIR, '../modules';

my $sqlfile;
my $files;
while ($sqlfile = readdir(DIR)) {
    if ($sqlfile =~ m/\.sql/) {
	push @$files, "../modules/$sqlfile";
    }
}

my $output = $translator->translate(
				    from       => 'PostgreSQL',
				    to         => 'HTML',
				    filename   => $files 
				    ) 
	    or die $translator->error;
	
my $file = ">tables.html";
open FILE, $file;
print FILE $output;
close FILE;


my $output = $translator->translate(
				    from       => 'PostgreSQL',
				    to         => 'GraphViz',
				    filename   => '../modules/associations.sql',
				    producer_args => {
					add_color => 1,
					show_constraints => 1,
					make_natural_join=>1,
					show_datatypes => 1,
					show_col_sizes => 1,
					}
				    ) 
	    or die $translator->error;
	
my $file = ">tables.png";
open FILE, $file;
print FILE $output;
close FILE;

