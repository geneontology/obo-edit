#!/usr/bin/perl -w

## Need a little knowledge of the "AmiGO" environment.
BEGIN{ require "config.pl"; }

## Bring in necessaries.
use utf8;
use strict;

##
use CGI;
use DBI;

## Just enough CGI to grab the incoming arg.
$CGI::POST_MAX = 0;
my $q = new CGI;

my $FILE_ALL = 'cache_all.db';
my $FILE_COUNT = 'cache_50.db';
my $big = 0;
my $FILE = $FILE_COUNT;
if( $q->query('big') eq '1' ){
    $big = 1;
    $FILE = $FILE_ALL;
}

## Connect/create database and load schema.
my $dbh =
  DBI->connect('dbi:SQLite:dbname=' . $FILE,'','',{ RaiseError => 1, })
  || die "Database connection not made: $DBI::errstr";


my $sth = $dbh->prepare('SELECT count(*) FROM species')
  or die "Couldn't prepare statement: " . $dbh->errstr;
$sth->execute()
  or die "Couldn't execute statement: " . $sth->errstr;
##
my $count = ($sth->fetchrow_array())[0];
$sth->finish;

$sth = $dbh->prepare('SELECT * FROM species LIMIT 1 OFFSET ?')
  or die "Couldn't prepare statement: " . $dbh->errstr;
my $rand = int(rand($count));
$sth->execute($rand)
  or die "Couldn't execute statement: " . $sth->errstr;
my @row = $sth->fetchrow_array();
my $a = $row[0] || '';
my $b = $row[1] || '';
my $c = $row[2] || '';

##
undef $sth;
$dbh->disconnect;


print <<EOC;
content-type:text/plain

EOC

print $big;
print "\n";
print $count;
print "\n";
print $rand;
print "\n";
print $a;
print "\n";
print $b;
print "\n";
print $c;
print "\n";
