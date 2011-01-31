=head1 AmiGO::LabelResolve

This will be a (hopefully) temporary package that contains functions
that operate directly with the DB because of issues with the API and/or speed.

Practically, there is some functionality shared between the slimmer
and TE that converts a random list of GPs into a term list while
counting things like dupes, not-founds, oks, etc.

Just to keep this a bit forward looking, we'll be a subclass of the
new AmiGO top-level (a new change in AmiGO::Slimmer as well).

=cut

use strict;

package AmiGO::LabelResolve;

use base ("AmiGO");


sub new {

  my $class = shift;
  my $incoming_dbh = shift || undef;
  my $incoming_species = shift || [];
  my $self  = $class->SUPER::new();

  $self->{DBH} = $incoming_dbh;
  $self->{GOOD_GPS} = [];
  $self->{MISSING_GPS} = [];
  $self->{DUPLICATE_GPS} = []; # keep immediate textual dupes.

  $self->{DUPLICATE_GPS_CACHE} = {}; # for keeping track of id
                                     # dupes. will later add with
                                     # textual dupes for final return.
  $self->{REPEAT_GPS_CACHE} = {};

  ## Init, but we want to make sure that the default is
  ## defined. Default to handling the uppercase.
  $self->{SPECIES} = {};
  foreach (@$incoming_species) {
    $self->{SPECIES}{uc($_)} = 1;
  }

  ## Start catching errors.
  $self->{SUCCESS} = 1;
  $self->{ERROR_MESSAGE} = 'n/a';

  bless $self, $class;
  return $self;
}


##
sub convert_list {

  my $self = shift;
  my $raw_gp_list = shift || [];

  $self->kvetch("AmiGO::LabelResolve::convert: #raw: " .
		scalar(@$raw_gp_list));

  ## Remove all the top-level dupes and make note of them.
  my %dedupe = ();
  foreach( @$raw_gp_list ){
    if( ! $dedupe{$_} ){
      $dedupe{$_} = 1;
    }else{
      ## Directly add textual dupes.
      push @{$self->{DUPLICATE_GPS}}, $_;
    }
  }
  my @gp_list = keys %dedupe;

  $self->kvetch("AmiGO::LabelResolve::convert_list: #dedupe: " .
		scalar(@gp_list));

  ## Make a hash of all of the GP labels and make them "unseen".
  my %gp_label_hash = ();
  foreach my $gp_label (@gp_list){
    # #$gp_label_hash{$gp_label} = undef;
    #$gp_label_hash{lc($gp_label)} = undef;
    ## BUG/TODO: since we're possible losing uniqueness with this, the
    ## fool-proof method would be to add a second method to deal with
    ## both the db and key (the current configuration coming from when
    ## the filer was required). If there are too many hits, adding a
    ## DB filter would take them out. I think this is sufficient until
    ## the DBIx::Class stuff is ready.
    my $new_label = $gp_label;
    if( $gp_label =~ /([.^\:]*):(.*)/ ){
      $self->kvetch("___hmmmm: " . $2 . ' from ' . $gp_label);
      $new_label = $2;
    #}else{
    #  $self->kvetch("___clear: " . $gp_label);
    #  $new_label = $gp_l
    }
    $gp_label_hash{lc($new_label)} = undef;
  }

  $self->kvetch("AmiGO::LabelResolve::convert_list: #label norm: " .
		scalar(keys %gp_label_hash));

  ## First DEBUG check.
  #my $rem_gp_list = $self->get_unbool_keys(\%gp_label_hash);
  #$self->kvetch('_start_unbool_count_ : '  . scalar(@$rem_gp_list), 0);

  ## WARNING: Yes, side-effects in %gp_label_hash...
  look_in_db('symbol', \%gp_label_hash, $self->{DBH}, $self->{SPECIES}, $self)
    if scalar( @{$self->get_unbool_keys(\%gp_label_hash) });
  look_in_db('acc', \%gp_label_hash, $self->{DBH}, $self->{SPECIES}, $self)
    if scalar( @{$self->get_unbool_keys(\%gp_label_hash) });
  look_in_db('synonym', \%gp_label_hash, $self->{DBH}, $self->{SPECIES}, $self)
    if scalar( @{$self->get_unbool_keys(\%gp_label_hash) });

  #       ## TODO: Try again for the bad ones (MGI:)
  #       ## Is this mecessary anymore?
  #       $rem_gp_list = $self->get_unbool_keys(\%gp_label_hash);
  #       $self->kvetch('_mid_unbool_count_ : ' . scalar(@$rem_gp_list), 0);
  #       foreach my $bad_key (@$rem_gp_list){
  # 	#$self->kvetch('_bad_key_ : ' . $bad_key, 0);
  # 	#if( ! $bad_key =~ /([^:]*?:)(.*)/ ){
  # 	if( $bad_key =~ /MGI:MGI:(.*)/ ){
  # 	  my $alt_key = 'MGI:' . $1;
  # 	  $self->kvetch('_bad_key_try_ : '.$bad_key.'->'.$alt_key, 0);
  # 	  ## TODO delete bad key, add alt key.
  # 	}
  #       }

  ## Final DEBUG check.
  #$rem_gp_list = $self->get_unbool_keys(\%gp_label_hash);
  #$self->kvetch('_final_unbool_count_ : '  . scalar(@$rem_gp_list), 0);
  #foreach my $label (keys %gp_label_hash){
  #if( defined($gp_label_hash{$label}) &&
  #    $gp_label_hash{$label} ){
  #  $self->kvetch('IN : ' . $gp_label_hash{$label}, 0);
  #}else{
  #  $self->kvetch('OUT : ' . $label, 0);
  #}
  #}

  ## Make note of all of the ones still missing.
  my $rem_gp_list = $self->get_unbool_keys(\%gp_label_hash);
  $self->kvetch("___final missing list size: " . scalar(@$rem_gp_list));
  if( scalar(@$rem_gp_list) ){
    push @{$self->{MISSING_GPS}}, @$rem_gp_list;
  }

  ## TODO: better reexamine for dupes
  #       if( @$term_l ){
  # 	foreach my $gp_label ( @$term_l ){

  # 	  if( ! $complete_gp_hash{$gp_label} ){
  # 	    $complete_gp_hash{$gp_label} = 1;
  # 	    push @$gp_term_l, @$term_l;
  # 	  }else{
  # 	    push @{$self->{DUPLICATE_GPS}}, $gp_label;
  # 	    $self->{DISPLAY_DUPLICATE_GPS} = 1;
  # 	  }
  # 	}else{
  # 	}
  #       }

  ## Make note of all of the good ones.
  $self->{GOOD_GPS} = $self->extract_accs_from_hash(\%gp_label_hash);

  $self->kvetch("AmiGO::LabelResolve::convert_list: #label norm 2: " .
		scalar(keys %gp_label_hash));
  $self->kvetch("AmiGO::LabelResolve::convert_list: #extracted: " .
		scalar(@{$self->{GOOD_GPS}}));

  #$self->{SUCCESS} = 0;
  #$self->{ERROR_MESSAGE} = 'a term list ref is a necessary argument';}
}


##
sub get_good_gps {
  my $self = shift;
  return $self->{GOOD_GPS};
}
sub get_missing_gps {
  my $self = shift;
  return $self->{MISSING_GPS};
}
sub get_duplicate_gps {
  my $self = shift;

#   ## Get the dupes we found later in the process.
#   my %duple = ();
#   foreach( keys %{$self->{DUPLICATE_GPS_CACHE}} ){
#     my $foo = $self->{DUPLICATE_GPS_CACHE}{$_};
#     $duple{$foo} = $foo;
#   }
#   push @{$self->{DUPLICATE_GPS}}, keys %duple;

  return $self->{DUPLICATE_GPS};
}
sub get_repeat_gps_hash {
  my $self = shift;
  $self->kvetch("AmiGO::LabelResolve::get_repeat_gps_hash");
  $self->kvetch("\t" . scalar(keys %{$self->{REPEAT_GPS_CACHE}}));

  my $return_gps = {};
  foreach my $key (keys %{$self->{REPEAT_GPS_CACHE}}){
    my $sub_hash = $self->{REPEAT_GPS_CACHE}{$key};
    if( scalar(keys %$sub_hash) > 1 ){
      $return_gps->{$key} = $sub_hash;
    }
  }

  return $return_gps;
}


## Returns 1 or 0 after all operations
sub success {
  my $self = shift;
  return $self->{SUCCESS};
}


## Returns an error message related to the reason for failure for all
## operations.
sub error_message {
  my $self = shift;
  return $self->{ERROR_MESSAGE};
}


##Return all gene product accs for the selected filters.
sub all_accs {

  my $self = shift;
  my $full_p = shift || 0;
  my $dbh = $self->{DBH};

  $self->kvetch("AmiGO::LabelResolve::all_accs");

  ##
  my $query = <<SQL;
      SELECT dbxref.xref_dbname, dbxref.xref_key FROM gene_product, dbxref
      WHERE (gene_product.dbxref_id = dbxref.id)
      AND
SQL

  ## Add the filter over species.
  my @filters = ();
  foreach my $s (keys %{$self->{SPECIES}}){
    push @filters, 'dbxref.xref_dbname = "' . $s . '"';
  }

  my $complete_query = $query . ' (' . join(' OR ', @filters) . ')';
  $self->kvetch("\t" . $complete_query);

  ##
  my $sth = $dbh->prepare($complete_query)
    or die "Couldn't prepare statement: " . $dbh->errstr;
  $sth->execute()
    or die "Couldn't execute statement: " . $sth->errstr;

  ##
  my $ret = [];
  while( my @row = $sth->fetchrow_array() ){

    my $db = $row[0];
    my $key = $row[1];

    my $pacc = undef;
    if( $full_p ){
      $pacc = $db . ':' . $key;
    }else{
      $pacc = $key;
    }

    #$self->kvetch("AmiGO::LabelResolve::all_accs: " . $pacc);
    push @$ret, $pacc;
  }
  $sth->finish();

  return $ret;
}


###
### Helper functions/things not in the published API:
###


## Return a list of still unbool hash keys.
## TODO: How fast can this go?
sub get_unbool_keys {

  my $self = shift;
  my $hashref = shift || {};
  my $bool_retlist = [];
  my $unbool_retlist = [];

  $self->kvetch("AmiGO::LabelResolve::get_unbool_keys");

  foreach my $key (keys %$hashref){
    my $val =  $hashref->{$key} || '(undef)';
    # $self->kvetch("\t\t" . $key . '=>' . $val);
    if( ! defined($hashref->{$key}) ){
      ##      push @$bool_retlist, $key;
      ##    }else{
      push @$unbool_retlist, $key;
    }
  }

  $self->kvetch("\tunbool_retlist size: " . scalar(@$unbool_retlist));

  #return ($bool_retlist, $unbool_retlist);
  return $unbool_retlist;
}


sub extract_accs_from_hash {

  my $self = shift;
  my $hashref = shift || {};
  my @retlist = ();

  $self->kvetch("AmiGO::LabelResolve::extract_keys_from_hash start");
  $self->kvetch("AmiGO::LabelResolve::extract_keys_from_hash in #: " .
	       scalar(keys %$hashref));

  # my $i = 1; # for debugging only.
  foreach my $key (keys %$hashref){
    if( defined($hashref->{$key})){
      my $sub_hash = $hashref->{$key};
      foreach my $sub_key (keys %$sub_hash){
	if( defined($sub_hash->{$sub_key}) &&
	    $sub_hash->{$sub_key} ){
	  # $self->kvetch("\t$i sub_key = $sub_key");
	  push @retlist, $sub_key;
	}
      }
    }
    # $i++
  }

  $self->kvetch("AmiGO::LabelResolve::extract_keys_from_hash out #: " .
	       scalar(@retlist));
  $self->kvetch("AmiGO::LabelResolve::extract_keys_from_hash end");

  return \@retlist;
}


sub look_in_db {

  my $column = shift || die "need to define a column: $!";
  my $hashref = shift || die "need to define a hashref: $!";
  my $dbh = shift || die "need to define a dbh: $!";
  my $species = shift || die "need to define a species hash: $!";
  my $self = shift || die "need to define a self: $!";

  my $col_name = 'NIL';
  my $col_number = -1;
  if( $column eq 'acc' ){
    $col_name = 'dbxref.xref_key';
    $col_number = 8;
  }elsif( $column eq 'synonym' ){
    $col_name = 'gene_product_synonym.product_synonym';
    $col_number = 12;
  }elsif( $column eq 'symbol' ){
    $col_name = 'gene_product.symbol';
    $col_number = 1;
  }else{
    die "need to define a proper column :$!";
  }

  $self->kvetch("AmiGO::LabelResolve::look_in_db");
  $self->kvetch("\tcolumn = $column");

  my $rem_gp_list = $self->get_unbool_keys($hashref);
  $self->kvetch("\t_count_ : "  . scalar(@$rem_gp_list));
  ## These are the three, almost identical queries, that we're going to
  ## use to squeeze out the product ACCs for our process.
  my $query = <<SQL;
      SELECT * FROM gene_product
        LEFT JOIN dbxref
          ON gene_product.dbxref_id = dbxref.id
        LEFT JOIN gene_product_synonym
          ON gene_product.id = gene_product_synonym.gene_product_id
        WHERE $col_name IN
SQL

  my $complete_query = $query . ' ('. join(',', map{'?'} @$rem_gp_list) . ')';
  $self->kvetch("\t_sql_: " . $complete_query);
  $self->kvetch("\t_sql_args_: " . join(', ', @$rem_gp_list));
  my $sth = $dbh->prepare($complete_query)
    or die "Couldn't prepare statement: " . $dbh->errstr;
  $sth->execute(@$rem_gp_list)
    or die "Couldn't execute statement: " . $sth->errstr;

  ##
  while( my @row = $sth->fetchrow_array() ){
    my $id = $row[0] || '?';
    my $dbname = uc($row[7]); # remember--default to uppercase
    my $acc = $row[8];
    my $flexi_col = lc($row[$col_number]);

#     ## DEBUG:
#     #my $foo = $id . $dbname . ':' . $acc;
#     if ( defined( $self->{DUPLICATE_GPS_CACHE}{$id} ) ) {
#       $self->kvetch("\t...dupe..." . $id);
#     }else {
#       $self->{DUPLICATE_GPS_CACHE}{$id} = $flexi_col;
#     }

    ## Next, do filtering by species and make note of sp repeats.
    # $self->kvetch("\t...if..." . $acc);
    # $self->kvetch("\t...if..." . scalar(keys %{$self->{SPECIES}}));
    # $self->kvetch("\t...if..." . defined($self->{SPECIES}{$dbname}));
    if( $acc &&
	( scalar(keys %{$self->{SPECIES}}) == 0 ||
	  defined($self->{SPECIES}{$dbname}) )){

      ## Make sure that flexi col is at least defined.
      if( ! defined( $hashref->{$flexi_col} )){
	$hashref->{$flexi_col} = {};
      }

      # $self->kvetch("\t".$column.':'.$id.':'.$flexi_col.':'.$dbname.':'.$acc);
      ## BUG: SQL may search case-insensitive, but our keys...
      #$hashref->{$flexi_col} = $acc;
      ## BUG: This one misses things like gene:XXXXXXX as a true acc...
      #$hashref->{lc($flexi_col)} = $acc;
      $hashref->{$flexi_col}{$acc} = $dbname;

      ## Find repeats and store them for later.
      my $number_of_species = scalar(keys %{$hashref->{$flexi_col}} );
      # $self->kvetch("\t...".$number_of_species." repeat on: ".$flexi_col);
      #if( $number_of_species > 1 ){
      #$self->kvetch("\t...".$number_of_species." repeat on: ".lc($flexi_col));
      $self->{REPEAT_GPS_CACHE}{$flexi_col} = {}
	if ! defined($self->{REPEAT_GPS_CACHE}{$flexi_col});
      $self->{REPEAT_GPS_CACHE}{$flexi_col}{$acc} = $dbname;
      #}
    }
  }
  $sth->finish();

  return $hashref;
}



1;
