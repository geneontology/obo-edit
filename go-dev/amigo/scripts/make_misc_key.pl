#!/usr/local/bin/perl

#generated key val pair for various options display in AmiGO

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
use FreezeThaw qw(freeze thaw);
use GO::SqlWrapper qw(:all);
use GO::CGI::Session;

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

my $hash = {};

#datasource = speciesdb
my $hl = select_hashlist
  ($dbh,
   ["gene_product g", "association a", "dbxref xref"],
   ["g.dbxref_id=xref.id", "g.id=a.gene_product_id"],
   ["distinct xref.xref_dbname"],
   "xref.xref_dbname", #order by
  );
#key = val
#$hash->{speciesdb} = {map{$_->{xref_dbname}=>$_->{xref_dbname}}@{$hl || []}};
$hash->{speciesdb} = [ 
	map { (split("\0", $_))[1] }
	sort 
	map { join("\0", lc($_->{xref_dbname}), $_->{xref_dbname}) }
	@{$hl || []}
];


#ontology
$hl = select_hashlist
  ($dbh,
   ["term t", "term2term t2t"],
   ["t.id=t2t.term1_id", "t.is_root <> 1"],
   "distinct term_type",
  );
#key = val
#$hash->{ontology} = {map{$_->{term_type}=>$_->{term_type}}@{$hl || []}};
$hash->{ontology} = [ 
	map { (split("\0", $_))[1] }
	sort 
	map { join("\0", lc($_->{term_type}), $_->{term_type}) }
	@{$hl || []}
];

#reltype
$hl = select_hashlist
  ($dbh,
   ["term t", "term2term t2t"],
   ["t.id=t2t.relationship_type_id"],
   "distinct t.name",
  );
#$hash->{reltype} = {map{$_->{name}=>$_->{name}}@{$hl || []}};
$hash->{reltype} = [ 
	map { (split("\0", $_))[1] }
	sort 
	map { join("\0", lc($_->{name}), $_->{name}) }
	@{$hl || []}
];

#evcode
$hl = select_hashlist
  ($dbh,
   ["association a", "evidence ev"],
   ["a.id=ev.association_id"],
   "distinct ev.code",
  );
#$hash->{evcode} = {map{$_->{code}=>$_->{code}}@{$hl || []}};
$hash->{evcode} = [ 
	map { (split("\0", $_))[1] }
	sort 
	map { join("\0", lc($_->{code}), $_->{code}) }
	@{$hl || []}
];

#gptype
$hl = select_hashlist
  ($dbh,
   ["gene_product gp", "term t"],
   ["gp.type_id=t.id"],
   "distinct t.name",
  );
#$hash->{gptype} = {map{$_->{name}=>$_->{name}}@{$hl || []}};
$hash->{gptype} = [ 
	map { (split("\0", $_))[1] }
	sort 
	map { join("\0", lc($_->{name}), $_->{name}) }
	@{$hl || []}
];

#association qualifiers
$hl = select_hashlist
  ($dbh,
   ["association_qualifier aq", "term t"],
   ["aq.term_id=t.id"],
   "distinct t.name",
  );
#$hash->{qual} = {map{$_->{name}=>$_->{name}}@{$hl || []}};
$hash->{qual} = [ 
	map { (split("\0", $_))[1] }
	sort 
	map { join("\0", lc($_->{name}), $_->{name}) }
	@{$hl || []}
];

#assigned by
$hl = select_hashlist
  ($dbh,
   ["association a", "db"],
   ["a.source_db_id=db.id"],
   "distinct db.name",
  );
#$hash->{assby} = {map{$_->{name}=>$_->{name}}@{$hl || []}};
$hash->{assby} = [ 
	map { (split("\0", $_))[1] }
	sort 
	map { join("\0", lc($_->{name}), $_->{name}) }
	@{$hl || []}
];

##syn_type
#$hl = select_hashlist
# ($dbh,
#   ["term_synonym ts", "term t"],
#   ["ts.synonym_type_id=t.id"],
#   "distinct t.name",
#  );
#$hash->{syn_type} = {map{$_->{name}=>$_->{name}}@{$hl || []}};

if ($ENV{DEBUG}) {
	use Data::Dumper;
	$Data::Dumper::Indent = 1;
	print STDERR Dumper($hash);
#    map{printf STDERR "$_\n%s\n",join("\t",keys %{$hash->{$_}})}keys %$hash;
}

my $str = freeze $hash;
my $f = "$cgi_path/misc_keys.pl";
unlink $f if (-f $f);
open(W, ">$f") or die "can not open $f: $!";
print W $str;
close(W);

$dbh->disconnect;
print STDERR "$0 Done\n";
exit;

sub help {
    print <<EOM;
  make_misc_key.pl amigo_cgi_dir
    where amigo_cgi_dir is the full path to the cgi directory where the AmiGO cgis reside
EOM
}
