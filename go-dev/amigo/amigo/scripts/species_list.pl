#!/usr/local/bin/perl -w

BEGIN {
    if (defined($ENV{GO_ROOT})) {
	use lib "$ENV{GO_ROOT}/perl-api";
    }
}

use strict;

use GO::Parser;
use GO::AppHandle;
use Getopt::Long;
use GO::SqlWrapper qw(:all);
use Data::Dumper;
use FreezeThaw qw(freeze thaw);

# Get args

my $apph = GO::AppHandle->connect(\@ARGV);

my $dbh = $apph->dbh;

my $fn = shift @ARGV;
my %binom = ();
my %count = ();
open(F, $fn) || die($fn);
while(<F>) {
    chomp;
    my ($id, $name, $xx, $type) = 
      map {s/^\s*//;s/\s*$//;$_ }
	split(/\|/, $_);

    if ($type eq 'scientific name') {
	my $spec_num = select_vallist($dbh,
	   ['species', 'gene_product'],
	   [' gene_product.species_id = species.id',
	    "species.ncbi_taxa_id = $id"],
	   ['species.id']
	   );

	my $n = scalar(@$spec_num);
	if ($n > 10) {
	    $name =~ s/(\w)\w*\ /$1\.\ /;
	    $binom{$name} = $id;
	    $count{$id} = scalar(@$spec_num);
	}
    }
}
close(F);

    print freeze (\%binom);
#foreach my $spec (sort keys %binom) {
#    print $spec . " : ".%binom->{$spec}."\n";
#}

#foreach my $c (sort keys %count) {
#     print %count->{$c}." : $c\n";
# }

print STDERR "Done!\n";
