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

use AmiGO::Worker::PT;
use AmiGO::Cache::PhylotreeSummary;
use AmiGO::Aid::PantherDB;

my $work = new AmiGO::Worker::PT(dbname => 'PANTHER');
my @cache_me = sort {
    my ($A, $B) = map {
	my $o = $_->{key};
	$o =~ s/[a-z]//gi;
	$o;
    } ($a, $b);
    $A <=> $B;
} $work->groups();

my @refg = AmiGO::Worker::PT::reference_genome;
my $cache = new AmiGO::Cache::PhylotreeSummary(@refg);

$cache->build();
$cache->open();
while (@cache_me) {
    my $cm = shift @cache_me;

    my @data = ($cm->{dbname}, $cm->{key}, ($cm->last_annotated() || ''),
		$cm->number_of_members(), $cm->number_of_refg_members(),
		$cm->exp(), map {
		    $cm->{by_species}->{$_->ncbi_taxon_id} || 0;
		} @refg);
    $cache->cache_data(@data);
    #print join("\t", @data) . "\n";
}
$cache->close();

__END__

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
