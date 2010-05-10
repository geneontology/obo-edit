#!/usr/local/bin/perl5.8.0

=head1 Synopsis

doc_maker.pl -xml path_to/amigo_userguide.xml -xsl path_to/docbook.xsl

=head2 Description

doc_maker.pl is a command line tool for doing XSLT 
transformations of an XML document.  The first argument
is an xml file.  The second is an XSLT file.

For purposes of creating amigo documentation, the xml_file
will be "go-dev/amigo/amigo/docs/amigo_userguide.xml".  The
XSL file should be "html/docbook.xsl" from the latest docbook
XSL distribution which can be downloaded at :

http://sourceforge.net/project/showfiles.php?group_id=21935


To pass xslt params, use "--xslt_param name=value".  This
can be repeated, ie:

--xslt_param tag_name=seq --xslt_param tag_value=AEIOU

=cut

use strict;
use XML::LibXML;
use XML::LibXSLT;
use Getopt::Long;

my $xml_file;
my $xsl_file;
my %xslt_params;

GetOptions(
           "xml=s"=>\$xml_file,
           "xsl=s"=>\$xsl_file,
	   "xslt_param=s%" => \%xslt_params
          );


my $parser = XML::LibXML->new();
my $source;
if ($xml_file eq '-') {
    $source = $parser->parse_fh(\*STDIN);
} elsif ($xml_file) {
    $source = $parser->parse_file($xml_file);
} else {
    die "No XML input file!";
}

if (!$xsl_file) {
    die "No XSL input file!";
}

my $xslt = XML::LibXSLT->new();
my $styledoc = $parser->parse_file($xsl_file);
my $stylesheet = $xslt->parse_stylesheet($styledoc);

my @args;
foreach my $param( keys %xslt_params) {
    push @args, XML::LibXSLT::xpath_to_string($param=>%xslt_params->{$param});
}
my $results = $stylesheet->transform($source, @args);

print $stylesheet->output_string($results);


sub usage {
    print "you must specify...";
}
