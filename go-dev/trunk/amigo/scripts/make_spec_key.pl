#!/usr/local/bin/perl

BEGIN { require "config.pl" if -f "config.pl" ; }
use lib "$ENV{GO_DEV_ROOT}/go-perl";
use lib "$ENV{GO_DEV_ROOT}/go-db-perl";
use lib "$ENV{GO_DEV_ROOT}/amigo/perl";

use strict;
use Data::Dumper;
use GO::SqlWrapper qw(:all);
use GO::CGI::Utilities;
use FreezeThaw qw(freeze thaw);

my $cgi_path = shift @ARGV;
unless ($cgi_path) {
    &help();
    exit;
}

eval {
    require "$cgi_path/config.pl";
};

my $apph = GO::CGI::Utilities::create_apph;
my $dbh = $apph->dbh;

my $hl = select_hashlist
  ($dbh,
   ["gene_product g", "association a", "species"],
   ["g.species_id = species.id", "g.id=a.gene_product_id"],
   ["count(*) as c", "species.genus", "species.species", "species.ncbi_taxa_id"],
   "c desc",
   ["ncbi_taxa_id", "genus", "species"],
  );

my %hash;
my %seen_ncbi_taxa_id = (); # this is for the experimental hack below...

my $top = shift @ARGV;
my $i = 1;
foreach (@{$hl || []}) {
  next unless (length($_->{genus}) || length($_->{species}));
  if ($_->{species} =~ /\w/){
    $hash{$_->{ncbi_taxa_id}} = [ $_->{genus}, $_->{species} ];
  }else{
    $hash{$_->{ncbi_taxa_id}} = [ $_->{genus} ];
  }
  $seen_ncbi_taxa_id{$_->{ncbi_taxa_id}} = 1;
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

undef $str;

### HACK ALERT! ###
#	creating a mapping of species to db to overcome the issues with
#	getting gp counts when we have both spp and db filters on.

my $results = $dbh->selectall_arrayref("SELECT DISTINCT ncbi_taxa_id, xref_dbname FROM gene_product, species, dbxref WHERE gene_product.dbxref_id=dbxref.id AND gene_product.species_id = species.id");
my $results_h;

foreach (@$results)
{	#if ($results_h->{$_->[0]})
	#	we have a one-to-many or many-to-many spp-to-db relationship
	#{	print STDERR "result exists for ".$_->[0].": ". $results_h->{$_->[0]}[0] . " vs ". $_->[1] ."!\n";
	#}
	push @{$results_h->{$_->[0]}}, $_->[1];
}

if (keys %$results_h)
{	$str = freeze $results_h;
	$f = "$cgi_path/spp_db_map.pl";
	unlink $f if (-f $f);
	open(W, ">$f") or die "can not open $f: $!";
	print W $str;
	close(W);
}
### END HACK ###

###
### Another (experimental) hack.
###
### Create mapping of species_id to ncbi_taxa_id--the join is very
### expensive when done live in db.
###

my $sp_results = $dbh->selectall_arrayref("SELECT DISTINCT id, ncbi_taxa_id FROM species");
my $sp_results_h;
foreach (@$sp_results){
  ## Restrict to ones found above in the species list.
  if( defined $seen_ncbi_taxa_id{$_->[1]} ){
    $sp_results_h->{$_->[0]} = $_->[1];
  }
}
if (keys %$sp_results_h){
  my $str = freeze $sp_results_h;
  my $f = "$cgi_path/spec_ncbi_map.pl";
  unlink $f if (-f $f);
  open(W, ">$f") or die "can not open $f: $!";
  print W $str;
  close(W);
}
### End.

$dbh->disconnect;
#printf STDERR "$0 Done (%d)\n",scalar(keys %hash);
exit;

sub help {
    print <<EOM;
  make_spec_key.pl amigo_cgi_dir [nn]
    where amigo_cgi_dir is full_dir_path where go.cgi resides,
    and nn is top nn species, sorted by number of associations
EOM
}
