#!/usr/local/bin/perl
####
#### Sort and dump all external links in the AmiGO database (will be
#### used for comparison against different versions of
#### make_dblinks.pl).
####
#### Usage:
####   ./scripts/dump_xref_links.pl
####

## Try to get the local environment sane. This is necessary for *any*
## operation, so we're really going to die.
BEGIN { require "config.pl" if -f "config.pl" ; }
use lib $ENV{GO_DEV_ROOT} . '/amigo/perl';
use lib $ENV{GOBO_ROOT};

## Bring in necessaries.
use utf8;
use strict;
use AmiGO;
#use GOBO::DBIC::GODBModel::Query;
use DBD::SQLite;

## 
my $core = AmiGO->new();
#my $q = GOBO::DBIC::GODBModel::Query->new({type=>'dbxref_lazy'});
#my $results = $q->get_all_results({}, {});

## Connect to db and execute statement.
#print "" . $core->db_connector() . "\n";
my $dbh = DBI->connect($core->db_connector(), { RaiseError => 1, })
  || die "Database connection not made: $DBI::errstr";
my $sth = $dbh->prepare('SELECT DISTINCT xref_dbname, xref_key FROM dbxref;')
  or die "Couldn't prepare statement: " . $dbh->errstr;
$sth->execute()
  or die "Couldn't execute statement: " . $sth->errstr;

## Create and store links from db listing.
my $store = [];
while( my @foo = $sth->fetchrow_array() ){
  my $db = $foo[0];
  my $key = $foo[1];
  my $link = $core->database_link($db, $key);
  if( defined $link && $link ){
    push @$store, $link;
    #print STDERR $link . "\n";
  }
}
$sth->finish;
undef $sth;
$dbh->disconnect;

## Sort and print links in store.
my @sorted = sort @$store;
foreach my $line (@sorted){
  print $line . "\n";
}
