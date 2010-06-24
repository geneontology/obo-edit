#!/usr/local/bin/perl
####
#### SQLite3 and filesystem cache creation and cleaning. Because of
#### the way that things shook out recently, this script actually does
#### is remove the current KVStore(and ::Filesystem) caches being used. Also see
#### make_exp_caches.pl.
####

## Setup environment.
BEGIN { require "config.pl" if -f "config.pl" ; }
use lib $ENV{GO_ROOT} . '/amigo/perl';
use lib $ENV{GO_SVN_ROOT} . '/gobo-dbic';

use File::Path qw(remove_tree);
use AmiGO::KVStore;
use AmiGO::KVStore::Filesystem;

my $core = AmiGO->new();

## Clean out old KVStore caches.
$core->kvetch("Removing old KVStore caches...");
foreach my $ca (@{ AmiGO::KVStore::list() }){
  $core->kvetch("Removing: $ca");
  unlink $ca if -f $ca;
}

## Clean out old KVStore::Filesystem caches (not (currently) a
## subclass of KVStore, so not on its list).
$core->kvetch("Removing old KVStore::Filesystem caches...");
foreach my $ca (@{ AmiGO::KVStore::Filesystem::list() }){
  $core->kvetch("Removing: $ca");
  remove_tree( $ca, {safe => 1} );
}
