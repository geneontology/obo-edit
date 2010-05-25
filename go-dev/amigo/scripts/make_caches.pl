#!/usr/local/bin/perl
####
#### SQLite3 cache database creation.
####

## Setup environment.
BEGIN { require "config.pl" if -f "config.pl" ; }
use lib $ENV{GO_ROOT} . '/amigo/perl';
use lib $ENV{GO_SVN_ROOT} . '/gobo-dbic';

use AmiGO::Cache::GONavi;
use AmiGO::Cache::ART;
use AmiGO::Cache::Species;
use AmiGO::Cache::ReferenceGenome;

##
print "Making GONavi data store...\n";
my $navi_cache = AmiGO::Cache::GONavi->new();
$navi_cache->build();

##
print "Making ART data store...\n";
my $art_cache = AmiGO::Cache::ART->new();
$art_cache->build();

##
my $specnum = 50;
print "Making species cache (top $specnum)...\n";
# my $sp_cache = AmiGO::Cache::Species->new();
my $sp_cache = AmiGO::Cache::Species->new($specnum);
$sp_cache->build();

## Clean out old cach
## TODO: Automate this by using KVStore registery.
print "Removing old cache file...\n";
my $amigo = AmiGO->new();
my $c1 = $amigo->amigo_env('AMIGO_CACHE_DIR') . '/akv_'.  'qg_ont' . '.db';
unlink $c1 if -f $c1;

##
# print "Making reference genome cache...\n";
# print "This is the problematically long one so it is last, feel free to kill...\n";
# my $rg_cache = AmiGO::Cache::ReferenceGenome->new();
# $rg_cache->build();
