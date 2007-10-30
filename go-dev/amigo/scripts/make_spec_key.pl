#!/usr/local/bin/perl

BEGIN {

	if (-f "config.pl") {
		require "config.pl";
	}

		if (defined($ENV{GO_ROOT})) {
		} elsif (-f "../cvs/go-dev/") {
		$ENV{GO_ROOT} = "../cvs/go-dev";
		}
}

use lib "$ENV{GO_ROOT}/go-perl";
use lib "$ENV{GO_ROOT}/go-db-perl";
use lib "$ENV{GO_AMIGO_ROOT}/perl";
#use lib "$ENV{GO_ROOT}/new-amigo/perl";

use strict;
use GO::SqlWrapper qw(:all);
use GO::CGI::Session;
use FreezeThaw qw(freeze thaw);

my $cgi_path = shift @ARGV;
unless ($cgi_path) {
    &help();
    exit;
}

eval {
    require "$cgi_path/config.pl";
};
my $session = new GO::CGI::Session(-read_only=>1);


my $dbh = $session->apph->dbh;

my $hl = select_hashlist
  ($dbh,
   ["gene_product g", "association a", "species"],
   ["g.species_id = species.id", "g.id=a.gene_product_id"],
   ["count(*) as c", "species.genus", "species.species", "species.ncbi_taxa_id"],
   "c desc",
   ["ncbi_taxa_id", "genus", "species"],
  );

my %hash;

my $top = shift @ARGV;
my $i = 1;
foreach (@{$hl || []}) {
    next unless (length($_->{genus}) || length($_->{species}));
#    my $g = length($_->{species})?substr($_->{genus}, 0, 1).".":$_->{genus};
#    my $k = "$g $_->{species}";
#    $k =~ s/\s+$//;
    #print STDERR "$k: $_->{c}\n";
#    $hash{$k} = $_->{ncbi_taxa_id};
     if ($_->{species} =~ /\w/)
     {	$hash{$_->{ncbi_taxa_id}} = [ $_->{genus}, $_->{species} ];
     }
     else
     {	$hash{$_->{ncbi_taxa_id}} = [ $_->{genus} ];
     }
     
    last if ($top && $i++ >= $top);
}
printf  STDERR "%s\nnum species: %d\n", join("\n",map{"$_ => ".join(" ", @{$hash{$_}})}keys %hash), scalar(keys %hash) if ($ENV{DEBUG});

my $h = \%hash;

my $str = freeze $h;
my $f = "$cgi_path/spec_keys.pl";
unlink $f if (-f $f);
open(W, ">$f") or die "can not open $f: $!";
print W $str;
close(W);

$dbh->disconnect;
printf STDERR "$0 Done (%d)\n",scalar(keys %hash);
exit;

sub help {
    print <<EOM;
  make_spec_key.pl amigo_cgi_dir [nn]
    where amigo_cgi_dir is full_dir_path where go.cgi resides,
    and nn is top nn species with most of associations
EOM
}
