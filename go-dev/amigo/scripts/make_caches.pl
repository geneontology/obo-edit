#!/usr/local/bin/perl
####
#### SQLite3 cache database creation and cleaning. Because of the way
#### that things shook out recently, all this script actually does not
#### is remove the current KVStores being used. Also see
#### make_exp_caches.pl.
####

## Setup environment.
BEGIN { require "config.pl" if -f "config.pl" ; }
use lib $ENV{GO_ROOT} . '/amigo/perl';
use lib $ENV{GO_SVN_ROOT} . '/gobo-dbic';

use AmiGO::KVStore;

## Clean out old caches.
my $core = AmiGO->new();
$core->kvetch("Removing old KVStore caches...");
foreach my $ca (@{ AmiGO::KVStore::list() }){
  $core->kvetch("Removing: $ca");
  unlink $ca if -f $ca;
}
