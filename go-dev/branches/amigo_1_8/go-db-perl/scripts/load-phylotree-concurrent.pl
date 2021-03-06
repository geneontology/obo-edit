#!/usr/local/bin/perl -w
use warnings;
use strict;
use IO::Uncompress::AnyUncompress qw/$AnyUncompressError/;
use Net::FTP;
use File::Temp qw/tempfile/;

use GO::AppHandle;

my $apph = GO::AppHandle->connect(\@ARGV);
my $dbh = $apph->dbh();

my $file;
if (1 == scalar(@ARGV)) {
    $file = shift @ARGV;
} elsif (0 == scalar(@ARGV)) {
    (undef, $file) = tempfile(undef, UNLINK=>1);
    my $host = 'ftp.geneontology.org';
    my $path = '/go/doc/paint';
    my $get  = 'panther-target-lits-all.csv';
    my $user = 'anonymous';
    my $pass = '';
    my $ftp  = Net::FTP->new($host, Passive => 1) or die $@;
    $ftp->login($user, $pass) or die $ftp->message;
    #$ftp->binary()            or die $ftp->message;
    $ftp->cwd($path)          or die $ftp->message;
    $ftp->get($get, $file)    or die $ftp->message;
    $ftp->close()
} else {
    die;
}

my $split = qr/,/;
my $xref_dbname = 'PantherDB';
my $property_key = 'concurrent';

my $xref_key_head = '"family"';
my $property_val_head = '';

my $csv =  IO::Uncompress::AnyUncompress->new($file)
  or die "AnyUncompress failed opening 'file': $AnyUncompressError\n";
my @head = split(m/$split/, readline($csv));
chomp $head[-1];


my $get_phylotree_id = $dbh->prepare(<<SQL);
SELECT phylotree.id FROM phylotree
JOIN dbxref ON(dbxref_id=dbxref.id)
WHERE xref_dbname=? AND xref_key=?
SQL

my $insert_phylotree_property = $dbh->prepare(<<SQL);
INSERT INTO phylotree_property(phylotree_id,property_key,property_val)
VALUES(?,?,?);
SQL

while (<$csv>) {
    my %line;
    chomp;
    @line{@head} = map {
	s/^"//;
	s/"$//;
	$_;
    } split(m/$split/, $_);

    $get_phylotree_id->execute($xref_dbname, $line{$xref_key_head}) or die;
    my $rows = $get_phylotree_id->rows();
    if (1 != $rows) {
	warn "$line{$xref_key_head}\tfound $rows groups\n";
	next;
    }
    my ($phylotree_id) = $get_phylotree_id->fetchrow_array;
    $insert_phylotree_property->execute
      ($phylotree_id, $property_key,  $line{$property_val_head});
}
$csv->close();
