#!/usr/local/bin/perl
use warnings;
use strict;
use Data::Dumper;

sub BEGIN{
    if ($ARGV[0]) {
	require shift(@ARGV);
    } elsif (-f 'config.pl') {
	require 'config.pl';
    } else {
	die 'Found no config.pl file';
    }
}

use lib "$ENV{GO_DEV_ROOT}/amigo/perl";
use lib $ENV{GOBO_ROOT};

use AmiGO::Worker::Phylotree;
use AmiGO::Cache::PhylotreeSummary;
use AmiGO::Aid::PantherDB;

my $work = new AmiGO::Worker::Phylotree(dbname => 'PANTHER');
my @cache_me = $work->id4();

my @species = AmiGO::Aid::PantherDB->reference_genome();
my $cache = new AmiGO::Cache::PhylotreeSummary(@species);
$cache->build();

$cache->open();
while (scalar @cache_me) {
    my $me = shift @cache_me;
    my @data = ($me->{dbname}, $me->{key}, $me->last_annotated(), $me->{number_of_members},
		$me->{number_of_refg_members}, $me->{exp}, map {
	$_->{count};
    } @{ $me->{dist} });

    $cache->cache_data(@data);
}
$cache->close();
