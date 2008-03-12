#!/usr/bin/perl
use strict;
use Cwd;

my $dir_cmd =  "basename `pwd`";
my $dir = `$dir_cmd`;
chomp($dir);
print "Project name is: " . $dir . "\n";
if ($dir eq "OBDWS"){
	print "Update script can't (and shouldn't) be run on project OBDWS\n";
	exit;
}

my $command = "find . -name .svn -type d -exec chmod -R +w {} \\\;";

print "Making directories writable\n";
`$command`;

my $command = "find . -name .svn -type d -exec rm -r {} \\\;";

print "Detatching from Subversion\n";

`$command`;

my $project_file = ".project";
my $plugin_file = ".settings/org.eclipse.wst.common.component";
my $webxml_file = "WebContent/WEB-INF/web.xml";

print "Replacing OBDWS in $project_file\n";
replace_words($project_file,$dir);

print "Replacing OBDWS in $plugin_file\n";
replace_words($plugin_file,$dir);

print "Replacing OBDWS in $webxml_file\n";
replace_words($webxml_file,$dir);



sub replace_words {
	my ($filename,$new_project_name,) = @_;
	undef $/;
	open INFILE, $filename or die "Couldn't open file " . $filename . "\n";
	my $file_text = <INFILE>;
	close INFILE;
	$file_text =~ s/OBDWS/$new_project_name/g;
	open OUTFILE, ">$filename" or die "Couldn't open file " . $filename . "\n";
	print OUTFILE $file_text;
	close OUTFILE;
}




