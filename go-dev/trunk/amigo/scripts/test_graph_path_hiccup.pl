#!/usr/local/bin/perl -w
####
#### Check for terms in lead where the longest graph path is not to a
#### root term.
####

## Setup environment.
BEGIN { require "config.pl" if -f "config.pl" ; }
use lib $ENV{GO_DEV_ROOT} . '/amigo/perl';
use lib $ENV{GOBO_ROOT};

use DBI;
use AmiGO;
my $core = AmiGO->new();

## Clean out old KVStore caches.
$core->kvetch("Starting check...");

my $term_query = <<SQL;
SELECT term.acc FROM term WHERE term.acc != 'all' AND term.term_type IN ('biological_process', 'cellular_component', 'molecular_function')
SQL

my $path_query = <<SQL;
SELECT sub.name AS sub, graph_path.relationship_type_id, obj.name AS obj, relation_distance, distance FROM graph_path, term AS sub, term AS obj WHERE graph_path.term2_id = sub.id AND graph_path.term1_id = obj.id AND sub.acc = ? AND obj.acc != 'all' ORDER BY distance
SQL

my $dbh = DBI->connect("DBI:mysql:go_latest_lite:localhost", "", "")
  or die "Couldn't connect $!";

my $term_sth = $dbh->prepare($term_query)
  or die "Couldn't prepare statement (1): " . $dbh->errstr;
$term_sth->execute()
  or die "Couldn't execute statement (1): " . $term_sth->errstr;

my $path_sth = $dbh->prepare($path_query)
  or die "Couldn't prepare statement (2): " . $dbh->errstr;

##
my $i = 0;
my $good_count = 0;
my $bad_count = 0;
while( my @term_row = $term_sth->fetchrow_array() ){

  $core->kvetch("Finished " . $i) if $i % 1000 == 0;

  my $acc = $term_row[0];

  ## Test terms, where we currently see the problem and not.
  # if( $acc eq 'GO:0072028' ||
  #     $acc eq 'GO:0022008' ){

    $path_sth->execute($acc)
      or die "Couldn't execute statement (2): " . $path_sth->errstr;

    my $path_row = $path_sth->fetchall_arrayref();

    my $count = scalar(@$path_row);

    ## DEBUG.
    #    $core->kvetch("See a: " . $acc);
    #    $core->kvetch("See b: " . path_row);
    #    $core->kvetch("See c: " . $count);
    #    $core->kvetch("See d: " . $$path_row[$count -1][2]);

    if( $$path_row[$count -1][2] eq 'molecular_function' ||
	$$path_row[$count -1][2] eq 'cellular_component' ||
	$$path_row[$count -1][2] eq 'biological_process' ){
      $good_count++;
    }else{
      $bad_count++;
    }
  #  }

  $i++;
}

$core->kvetch("Good: " . $good_count);
$core->kvetch("Bad: " . $bad_count);
$core->kvetch("Total: " . $i);

$dbh->disconnect;
