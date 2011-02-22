#!/usr/bin/perl -w

## Need a little knowledge of the "AmiGO" environment.
BEGIN{ require "config.pl"; }
use lib $ENV{GO_DEV_ROOT} . '/go-perl';
use lib $ENV{GO_DEV_ROOT} . '/go-db-perl';
use lib $ENV{GO_DEV_ROOT} . '/amigo/perl';
use AmiGO;

my $core = AmiGO->new();
my $species = $core->species();

my @all_keys = keys %$species;
my $count = scalar(@all_keys);
my $rand = int(rand($count));
my $key = $all_keys[$rand];
my $spec = $species->{$key};

print <<EOC;
content-type:text/plain

EOC

print $count;
print "\n";
print $rand;
print "\n";
print $key;
print "\n";
print $spec;
print "\n";
