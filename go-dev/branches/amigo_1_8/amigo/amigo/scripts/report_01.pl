#!/usr/bin/perl -w
####
#### Pull all the experimental MF and BP annotations for human genes, and
#### separately for mouse genes, and generate a table (or better yet, two
#### tables, one for MF and one for BP) where each row is a molecular function
#### or biological process, and the columns are:
#### 1. Term
#### 2. Number of mouse genes annotated to this term or a subclass
#### 3. Number of human genes annotated to this term or a subclass
#### 4. (if possible) p-value comparing the numbers in columns 2 and 3.  We
#### could do this with an enrichment tool, or even in Excel pretty
#### straightforwardly.
####

use strict;
use DBI;

my $dbname = 'go_latest_lite';
my $dbhost = 'localhost';
my $uname = 'sjcarbon';
my $upass = '';

###
###
###

## Connection.
my $dbh = DBI->connect("DBI:mysql:$dbname;host=$dbhost", $uname, $upass) ||
  die "Could not connect to database: $DBI::errstr";

## The Query.
my $query = <<SQL;
SELECT
 term.acc AS acc,
 COUNT(distinct gene_product.id) AS cnt
FROM
 term,
 graph_path,
 association,
 gene_product
WHERE
 term.id=graph_path.term1_id AND
 graph_path.term2_id=association.term_id AND
 association.gene_product_id = gene_product.id AND
 NOT term.acc = 'all' AND
 association.id IN
 (SELECT
   assoc.id
  FROM
   association AS assoc,
   evidence,
   gene_product AS gp,
   species,
   term AS t
  WHERE
   assoc.id = evidence.association_id AND
   assoc.gene_product_id = gp.id AND
   gp.species_id = species.id AND
   assoc.term_id = t.id AND
   species.ncbi_taxa_id = ? AND
   evidence.code IN ('EXP', 'IDA', 'IPI', 'IMP', 'IGI', 'IEP') AND
   t.term_type = ?)
GROUP BY
 term.acc
ORDER BY cnt DESC
SQL

##
sub run_on_db {
  my $ncbi_taxa_id = shift || die "!";
  my $ontology = shift || die "!";

  my $rethash = {};
  my $sth = $dbh->prepare($query);
  $sth->execute($ncbi_taxa_id, $ontology);
  while (my $ref = $sth->fetchrow_hashref()) {
    #print "acc = $ref->{'acc'}, count = $ref->{'cnt'}" . "\n";
    $rethash->{$ref->{'acc'}} = $ref->{'cnt'};
  }
  $sth->finish();

  return $rethash;
}

## Gather all of the terms from the sets and dump BP.
sub dump_data {
  my $ontology = shift || die "!";
  my $human_hash = shift || die "!";
  my $mouse_hash = shift || die "!";

  my $acc_cache = {};
  foreach my $acc (keys %$human_hash){ $acc_cache->{$acc} = 1; }
  foreach my $acc (keys %$mouse_hash){ $acc_cache->{$acc} = 1; }
  foreach my $acc (keys %$acc_cache){
    my $human_count = 0;
    my $mouse_count = 0;
    if( defined $human_hash->{$acc} ){ $human_count = $human_hash->{$acc}; }
    if( defined $mouse_hash->{$acc} ){ $mouse_count = $mouse_hash->{$acc}; }
    print $acc ."\t". $human_count ."\t". $mouse_count ."\t". $ontology ."\n";
  }
}

## Get all of the acc=>count hashes.
my $bp_human = run_on_db('10090', 'biological_process');
my $bp_mouse = run_on_db('9606', 'biological_process');
my $mf_human = run_on_db('10090', 'molecular_function');
my $mf_mouse = run_on_db('9606', 'molecular_function');

## And get them to STDOUT.
dump_data('biological_process', $bp_human, $bp_mouse);
dump_data('molecular_function', $mf_human, $mf_mouse);

## Done.
$dbh->disconnect();
