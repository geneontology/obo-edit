=head1 AmiGO::Cache::ReferenceGenome

TODO: Cache all of the visible RG data.
TODO: Access the above.
BUG: Still incomplete.

Note: after build, this will still be a write-only cache.

=cut

package AmiGO::Cache::ReferenceGenome;

use base 'AmiGO::Cache';
use GOBO::DBIC::GODBModel::Query;
use AmiGO::Aid::ReferenceGenome;
use GOBO::DBIC::GODBModel::Query;
use AmiGO::Worker::GPInformation::HomolsetInformation;


=item new

Args:
Returns:

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new('rg');

  $self->{AID}= AmiGO::Aid::ReferenceGenome->new();

  bless $self, $class;
  return $self;
}


=item build

TODO

Args:
Returns:

=cut
sub build {

  my $self = shift;

  ## TODO: should there also be a probe method?
  ## Wipe.
  $self->initialize();

  ##
  $self->open();

  ## Connect/create database and load schema.
  my @schemas = ();
  $schemas[0] = qq{
 CREATE TABLE rg_set (
    id INTEGER PRIMARY KEY,
    symbol VARCHAR2(128) NOT NULL
 );
};
  $schemas[1] = qq{
 CREATE TABLE term (
    id INTEGER PRIMARY KEY,
    rg_set_id INTEGER NOT NULL,
    acc VARCHAR2(128) NOT NULL
 )
};
  $schemas[2] = qq{
 CREATE TABLE gene_product (
    id INTEGER PRIMARY KEY,
    rg_set_id INTEGER NOT NULL,
    ncbi_taxa_id INTEGER NOT NULL,
    acc VARCHAR2(128) NOT NULL,
    symbol VARCHAR2(128) NOT NULL
 )
};
  $schemas[3] = qq{
 CREATE TABLE evidence (
    id INTEGER PRIMARY KEY,
    gene_product_id INTEGER NOT NULL,
    exp INTEGER NOT NULL,
    good INTEGER NOT NULL,
    odd INTEGER NOT NULL
 )
};
  $schemas[4] = qq{
 CREATE TABLE exp_evidence (
    id INTEGER PRIMARY KEY,
    evidence_id INTEGER NOT NULL,
    code VARCHAR2(128) NOT NULL
 )
};
  foreach my $schema (@schemas){
    $self->{CACHE_DBH}->do( $schema )
      or die $self->{CACHE_DBH}->errstr;
  }

  ###
  ###
  ###

  my $term_id = 0;
  my $gene_product_id = 0;
  my $evidence_id = 0;
  my $exp_evidence_id = 0;

  ## Iterate over all homolsets.
  my $hsq = GOBO::DBIC::GODBModel::Query->new({type=>'homolset'});
  my $all_hs = $hsq->get_all_results();
  foreach my $hs (@$all_hs){
    my $rg_set_id = $hs->id;
    my $hs_symbol = $hs->symbol;

    $self->_set_insert($rg_set_id, $hs_symbol);

    # if( $rg_set_id > 1 &&
    # 	$rg_set_id < 5 ){
    # if( $rg_set_id <= 20 ){
    if( 1 == 1 ){

      #$self->kvetch("HSS: Working on $hs_id ($hs_symbol)...");

      ## TODO: collect RG data.
      my $rg_info =
	AmiGO::Worker::GPInformation::HomolsetInformation->new({skip_roots=>1});
      $rg_info->calculate($rg_set_id) || die "!!!!";

      my $matrix = $rg_info->get_matrix();
      foreach my $acc (keys %$matrix){

	## Term insert.
	$term_id++;
	$self->_term_insert($term_id, $rg_set_id, $acc);

	foreach my $ncbi_taxa_id (keys %{$matrix->{$acc}}){

	  # $self->kvetch("\ttaxa: " . $ncbi_taxa_id);

	  ## Compactions (as in the templates) starts here.
	  my $compactor = {};
	  foreach my $a_id (keys %{$matrix->{$acc}{$ncbi_taxa_id}}){

	    # $self->kvetch("\t\tassoc: " . $a_id);

	    foreach my $prop (keys %{$matrix->{$acc}{$ncbi_taxa_id}{$a_id}}){

	      my $a_struct = $matrix->{$acc}{$ncbi_taxa_id}{$a_id};
	      my $gp_acc = $a_struct->{gene_product_id};
	      my $gp_sym = $a_struct->{gene_product_symbol} || '???';

	      if( ! defined $compactor->{$gp_acc} ){
		$compactor->{$gp_acc} = {};
		$compactor->{$gp_acc}{symbol}= $gp_sym;
		$compactor->{$gp_acc}{direct_p} = 0;
		$compactor->{$gp_acc}{has_exp_p} = 0;
		$compactor->{$gp_acc}{has_good_iss_p} = 0;
		$compactor->{$gp_acc}{has_odd_iss_p} = 0;
		$compactor->{$gp_acc}{exp_evidence} = {};
		#$compactor->{$gp_acc}{has_non_p} = 0;
	      }

	      my $obj = $compactor->{$gp_acc};
	      if( $a_struct->{has_exp_p} >= 1 ){
		$obj->{has_exp_p} = 1;
	      }
	      if( $a_struct->{has_good_iss_p} >= 1 ){
		$obj->{has_good_iss_p} = 1;
	      }
	      if( $a_struct->{has_odd_iss_p} >= 1 ){
		$obj->{has_odd_iss_p} = 1;
	      }
	      if( $a_struct->{direct_p} >= 1 ){
		$obj->{direct_p} = 1;
	      }
	      if( scalar(@{$a_struct->{exp_evidence}}) ){
		foreach my $code (@{$a_struct->{exp_evidence}}){
		  $obj->{exp_evidence}{$code} = 1;
		}
	      }
	    }
	  }

	  ## Post-compaction GP insert.
	  foreach my $gp_acc (keys %$compactor){
	    $gene_product_id++;
	    #$self->kvetch("\tgp: " . $gp_acc . ' ' . $gene_product_id);
	    $self->_gene_product_insert($gene_product_id, $rg_set_id,
					$ncbi_taxa_id, $gp_acc,
					$compactor->{$gp_acc}{symbol});

	    ## Adding for noteworthy evidence.
	    if( $compactor->{$gp_acc}{has_exp_p} ||
		$compactor->{$gp_acc}{has_good_iss_p} ||
		$compactor->{$gp_acc}{has_odd_iss_p} ){
	      $evidence_id++;
# 	      $self->kvetch("1 " . $evidence_id);
# 	      $self->kvetch("2 " . $gene_product_id);
# 	      $self->kvetch("3 " . $compactor->{$gp_acc}{has_exp_p});
# 	      $self->kvetch("4 " . $compactor->{$gp_acc}{has_good_iss_p});
# 	      $self->kvetch("5 " . $compactor->{$gp_acc}{has_odd_iss_p});
	      $self->_evidence_insert($evidence_id,
				      $gene_product_id,
				      $compactor->{$gp_acc}{has_exp_p},
				      $compactor->{$gp_acc}{has_good_iss_p},
				      $compactor->{$gp_acc}{has_odd_iss_p});

# 	      ## Adding evidence codes for things EXP.
# 	      if( scalar(keys %{$compactor->{$gp_acc}{exp_evidence}}) ){
# 		#     $self->kvetch("\t\t" .
# 		#	    join(' ',
# 		#		 keys %{$compactor->{$gp_acc}{evidence}}));
# 	      }

	      ## Post-compaction evidence insert.
	      foreach my $code (keys %{$compactor->{$gp_acc}{evidence}}){
		$exp_evidence_id++;
		$self->_exp_evidence_insert($exp_evidence_id,
					    $evidence_id, $code);
	      }
	    }
	  }
	}
      }
    }
  }

  ## Done.
  $self->close();
}


=item species

TODO

Args:
Returns: a hashref of all species in cache (<ncbi_taxa_id> => <readable_name>)

=cut
sub species {

  my $self = shift;
  my $ret = {};

  $self->open();

  ## Grab data.
  my $sth = $self->{CACHE_DBH}->prepare('SELECT DISTINCT ncbi_taxa_id FROM gene_product;')
    or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  $sth->execute()
    or die "Couldn't execute statement: " . $sth->errstr;

  ## Group results.
  while( my @ids = $sth->fetchrow_array() ){
    my $id = $ids[0];
    #$ret->{$id} = $self->{AID}->taxid2readable({spec_id=>$id,web_safe=>1});
    $ret->{$id} = $self->{AID}->taxid2readable({spec_id=>$id});
  }

  $sth->finish;
  undef $sth;
  $self->close();

  return $ret;
}


=item reference_genome_sets

TODO

Args:
Returns: a hashref of all homolsets in cache (<rg_set_id> => <rg_set_symbol>)

=cut
sub reference_genome_sets {

  my $self = shift;
  my $ret = {};

  $self->open();

  ## Grab data.
  my $sth = $self->{CACHE_DBH}->prepare('SELECT * FROM rg_set;')
    or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  $sth->execute()
    or die "Couldn't execute statement: " . $sth->errstr;

  ## Group results.
  while( my @set = $sth->fetchrow_array() ){
    my $id = $set[0];
    my $sym = $set[1];
    $ret->{$id} = {symbol => $sym};
  }

  $sth->finish;
  undef $sth;
  $self->close();

  return $ret;
}


=item gene_products

TODO

Args:
Returns: a hashref of all gene products in cache (<gp_acc> => <gp_symbol>)

=cut
sub gene_products {

  my $self = shift;
  my $ret = {};

  $self->open();

  ## Grab data.
  my $sth = $self->{CACHE_DBH}->prepare('SELECT acc, symbol FROM gene_product;')
    or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  $sth->execute()
    or die "Couldn't execute statement: " . $sth->errstr;

  ## Group results.
  while( my @gp = $sth->fetchrow_array() ){
    my $acc = $gp[0];
    my $sym = $gp[1];
    $ret->{$acc} = {symbol => $sym };
  }

  $sth->finish;
  undef $sth;
  $self->close();

  return $ret;
}


=item summary

TODO

Args:
Returns: a hashref of set id -> gp id -> ev code.

=cut
sub summary {

  my $self = shift;
  my $ret = {};

  $self->open();

  ## Grab data.
  my $sth = $self->{CACHE_DBH}->prepare('SELECT rg_set.id, gene_product.ncbi_taxa_id, gene_product.acc, evidence.exp, evidence.good, evidence.odd FROM rg_set INNER JOIN gene_product ON (gene_product.rg_set_id = rg_set.id) INNER JOIN evidence ON (evidence.gene_product_id = gene_product.id);')
    or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  $sth->execute()
    or die "Couldn't execute statement: " . $sth->errstr;

  ## Group results.
  while( my @set = $sth->fetchrow_array() ){
    my $rg_id = $set[0];
    my $taxa_id = $set[1];
    my $gp_acc = $set[2];
    my $exp_p = $set[3];
    my $good_p = $set[4];
    my $odd_p = $set[5];

    if( ! defined $ret->{$rg_id} ){
      $ret->{$rg_id} = {};
    }
    if( ! defined $ret->{$rg_id}{$taxa_id} ){
      $ret->{$rg_id}{$taxa_id} = {};
    }
    if( ! defined $ret->{$rg_id}{$taxa_id}{$gp_acc} ){
      $ret->{$rg_id}{$taxa_id}{$gp_acc} = {};
    }
    if( ! defined $ret->{$rg_id}{$taxa_id}{$gp_acc}{exp_p} ){
      $ret->{$rg_id}{$taxa_id}{$gp_acc}{exp_p} = 0;
    }
    if( ! defined $ret->{$rg_id}{$taxa_id}{$gp_acc}{good_p} ){
      $ret->{$rg_id}{$taxa_id}{$gp_acc}{good_p} = 0;
    }
    if( ! defined $ret->{$rg_id}{$taxa_id}{$gp_acc}{odd_p} ){
      $ret->{$rg_id}{$taxa_id}{$gp_acc}{odd_p} = 0;
    }
    if( $exp_p ){ $ret->{$rg_id}{$taxa_id}{$gp_acc}{exp_p} = 1; }
    if( $good_p ){ $ret->{$rg_id}{$taxa_id}{$gp_acc}{good_p} = 1; }
    if( $odd_p ){ $ret->{$rg_id}{$taxa_id}{$gp_acc}{odd_p} = 1; }
  }

  $sth->finish;
  undef $sth;
  $self->close();

  return $ret;
}


=item test

Returns 1 for okay, 0 for badness.

=cut
sub test {

  my $self = shift;
  my $ret = 1;

  $self->open();

  ## Grab data.
  my $sth = undef;
  eval {
    $sth = $self->{CACHE_DBH}->prepare('SELECT count(*) FROM rg_set;')
      or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
    $sth->execute()
      or die "Couldn't execute statement: " . $sth->errstr;
  };
  if( $@ ){
    $ret = 0;
  }

  ##
  if( defined $sth ){
    $sth->finish;
  }
  undef $sth;
  $self->close();

  return $ret;
}


##
sub _set_insert {
  my $self = shift;
  my $id = shift || die "no id: $!";
  my $symbol = shift || die "no symbol: $!";

#  $self->kvetch("set: " . $id . ' ' . $symbol);

  ## Insert set information.
  my $sth =
    $self->{CACHE_DBH}->prepare('INSERT INTO rg_set (id, symbol) VALUES (?,?)')
      or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  my $undef_set_p = $sth->execute($id, $symbol)
    or die "Couldn't execute statement: " . $sth->errstr;

  ## Done.
  $sth->finish;
  undef $sth;
}


##
sub _term_insert {
  my $self = shift;
  my $id = shift || die "no id: $!";
  my $rg_set_id = shift || die "no rg_set_id: $!";
  my $acc = shift || die "no acc: $!";

#  $self->kvetch("term: id: " . $id . ' rg_set_id: ' . $rg_set_id . ' acc: ' . $acc);

  ## Insert set information.
  my $sth =
    $self->{CACHE_DBH}->prepare('INSERT INTO term (id,rg_set_id,acc) VALUES (?,?,?)')
      or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  my $undef_set_p = $sth->execute($id, $rg_set_id, $acc)
    or die "Couldn't execute statement: " . $sth->errstr;

  ## Done.
  $sth->finish;
  undef $sth;
}


##
sub _gene_product_insert {
  my $self = shift;
  my $id = shift || die "no id: $!";
  my $rg_set_id = shift || die "no rg_set_id: $!";
  my $ncbi_taxa_id = shift || die "no taxa: $!";
  my $acc = shift || die "no acc: $!";
  my $sym = shift || die "no sym: $!";

#   $self->kvetch("gp: id: " . $id . ' rg_set_id: ' . $rg_set_id .
# 		' taxa_id: ' . $ncbi_taxa_id . ' acc: '. $acc . ' sym: ' .);

  ## Insert set information.
  my $sth =
    $self->{CACHE_DBH}->prepare('INSERT INTO gene_product (id,rg_set_id,ncbi_taxa_id,acc,symbol) VALUES (?,?,?,?,?)')
      or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  my $undef_set_p = $sth->execute($id, $rg_set_id, $ncbi_taxa_id, $acc, $sym)
    or die "Couldn't execute statement: " . $sth->errstr;

  ## Done.
  $sth->finish;
  undef $sth;
}


##
sub _evidence_insert {
  my $self = shift;
  my $id = shift || die "no id: $!";
  my $gp_id = shift || die "no gp_id: $!";
  my $exp_p = shift || 0;
  my $good_iss_p = shift ||0;
  my $odd_iss_p = shift || 0;

#  $self->kvetch("ev: id: " . $id . ' gp_id: ' . $gp_id . ' code: '. $code);

  ## Insert set information.
  my $sth =
    $self->{CACHE_DBH}->prepare('INSERT INTO evidence (id,gene_product_id,exp,good,odd) VALUES (?,?,?,?,?)')
      or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  my $undef_set_p = $sth->execute($id, $gp_id, $exp_p, $good_iss_p, $odd_iss_p)
    or die "Couldn't execute statement: " . $sth->errstr;

  ## Done.
  $sth->finish;
  undef $sth;
}


##
sub _exp_evidence_insert {
  my $self = shift;
  my $id = shift || die "no id: $!";
  my $ev_id = shift || die "no ev_id: $!";
  my $code = shift || die "no code: $!";

#  $self->kvetch("ev: id: " . $id . ' ev_id: ' . $ev_id . ' code: '. $code);

  ## Insert set information.
  my $sth =
    $self->{CACHE_DBH}->prepare('INSERT INTO exp_evidence (id,evidence_id,code) VALUES (?,?,?)')
      or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  my $undef_set_p = $sth->execute($id, $gp_id, $code)
    or die "Couldn't execute statement: " . $sth->errstr;

  ## Done.
  $sth->finish;
  undef $sth;
}



1;
