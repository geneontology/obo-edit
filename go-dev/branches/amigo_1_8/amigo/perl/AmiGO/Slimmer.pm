=head1 AmiGO::Slimmer

Taken from Chris's script.

=cut

use strict;

package AmiGO::Slimmer;

#require Exporter;
#my @ISA = qw(Exporter);
#my @EXPORT = qw(new success error_message
#		get_ontology_mappings get_association_mappings
#		get_counts get_columns get_missed add_list
#		get_ontology_roots_association_hash);
#my @EXPORT_OK = qw(map_to_subset);

use base ("AmiGO");
use Data::Dumper;

## Takes an ontology graph, slim graph, and a binary argument for
## whether or not to use buckets.
sub new {

  my $class = shift;
  my $self  = $class->SUPER::new();

  my $ontology_graph = shift || undef;
  my $slim_graph = shift || undef;
  my $slim_set = shift || undef;
  my $ontology_roots = shift || undef;
  my $bucket = shift || 0;

  ## TODO: error checking
  #print STDERR '>>>[type]' . $type . "\n";
  #sleep 2;
  #my $self = {};

  $self->{ONTOLOGY_GRAPH} = $ontology_graph;
  $self->{SLIM_GRAPH} = $slim_graph;
  $self->{SLIM_GRAPH_TERM_HASH} = {};
  $self->{SLIM_SET} = $slim_set;

  $self->{ONTOLOGY_ROOTS_HASH} = $ontology_roots;
  $self->{ONTOLOGY_ROOTS_ASSOC_HASH} = {};

  $self->{MEMOIZED_RESULTS} = {};

  $self->{TERMS_ASSOCIATED_WITH_GPS} = {};
  $self->{GPS} = {};
  $self->{NEW_GA_COLUMNS} = [];
  #$self->{NEW_GA_LINES} = [];

  $self->{GP_MAPPINGS} = {}; # gp map to terms
  $self->{MISSED_TERMS} = {}; #
  $self->{MISSED_GPS} = {}; # gps that failed to be mapped to the slim
  # $self->{MISSED_GPS_ARARY} = []; # gps that failed to be mapped to the slim

  $self->{BUCKET} = 0;
  $self->{BUCKET} = 1 if $bucket && $bucket == 1;

  ## Start catching errors.
  $self->{SUCCESS} = 1;
  $self->{ERROR_MESSAGE} = 'n/a';

  ## Basic checking
  if( ! $ontology_graph || ! $slim_graph || ! $slim_set || ! $ontology_roots ){
    $self->{SUCCESS} = 0;
    $self->{ERROR_MESSAGE} = 'all four necessary arguments were not supplied';
  }else{

    ## Add buckets if we want them.
    $self->{SLIM_GRAPH}->add_buckets if $self->{BUCKET};

    ##
    %{$self->{SLIM_SET_HASH}} = map {$_->acc => $_} @{$self->{SLIM_SET}};
    $self->kvetch('SLIM_SET_HASH keys:');
    #$self->kvetch(Dumper(\(keys %{$self->{SLIM_SET_HASH}})));

    ## Generate a hash of the term array for the slims.
    my $slimterms = $self->{SLIM_GRAPH}->get_all_terms;
    %{$self->{SLIM_GRAPH_TERM_HASH}} = map {$_->acc => $_} @$slimterms;

    ## Initialize the counts. (Chris's script gets wacky...)
    #my $self->{COUNT_DIRECT_SLIM} = map { ($_ => 0) } keys %{$self->{SLIM_GRAPH_TERM_HASH}};
    #my %countall = %countleaf;
    $self->{COUNTED} = {};
    $self->{ALLH} = {};
    $self->{LEAFH} = {};
  }

  bless $self, $class;
  return $self;
}


## Adds a subgraph to the Slimmer object. Probably from a go_assoc file.
sub add_list {

  my $self = shift;
  my $term_l = shift || undef;
  my $from_apph_p = shift || 0;

  ## Arg check.
  if( ! $term_l ){
    $self->{SUCCESS} = 0;
    $self->{ERROR_MESSAGE} = 'a term list ref is a necessary argument';}
  if( $from_apph_p != 0 && $from_apph_p != 1 ){
    $self->{SUCCESS} = 0;
    $self->{ERROR_MESSAGE} = 'a bool of origin is a necessary argument';}

  ## If we've been successful.
  if( $self->{SUCCESS} ){

    ## Walk down through list and pull out all of the
    ## association info. Store it and add as we go.
    foreach my $term (@$term_l){

      #$self->kvetch('term->acc: ' . $term->acc);

      ## TODO: Discuss this: Lame? necessary evil? I'm at the point
      ## where I think that apph handle remembering its state is a bad
      ## idea. Just pass in the filters every time, make this all a
      ## lot easier.
      my $assoc_l = [];
      if( $from_apph_p ){
	$assoc_l = $term->selected_association_list;
      }else{
	$assoc_l = $term->association_list;
      }

      foreach my $assoc (@$assoc_l){

	## Get evidence info.
	my $ev_listref = $assoc->evidence_list;
	foreach my $ev (@$ev_listref) {

	  ## Start skipping if we're failing.
	  next if ! $self->{SUCCESS};

	  ## Data holder.
	  my @cols = ();

	  ## Get gp info.
	  my $gp = $assoc->gene_product;

	  ## Gotta get 'em all.
	  push @cols, $gp->speciesdb; # 1 DB
	  push @cols, $gp->acc; # 2 DB_Object_ID
	  push @cols, $gp->symbol; # 3 DB_Object_Symbol

	  #$self->kvetch('gp->acc: ' . $gp->acc);

	  # 4 NOT (optional)
	  if ( $assoc->is_not ) {
	    push @cols, 'NOT';
	  }else{
	    push @cols, '';
	  }

	  push @cols, $term->acc;# 5 GOid

	  # 6 DB:Reference
	  my $xref_listref = $ev->xref_list;
	  my @minibuf = ();
	  foreach my $xref ( @$xref_listref ) {
	    push @minibuf, $xref->dbname . ':' . $xref->xref_key; }
	  my $str = join '|', @minibuf;
	  push @cols, $str;

	  push @cols, $ev->code;# 7 Evidence

	  # 8 With/From (optional)
	  if(  $ev->seq_acc ){
	    push @cols, $ev->seq_acc;
	  }else{
	    push @cols, '';
	  }

	  ## Get aspect from the main ontology.
	  # 9 Aspect
	  my $aspect = '?';
	  $aspect = $term->get_code_from_namespace if
	    $term->get_code_from_namespace;
	  #my $full_term = $self->{ONTOLOGY_GRAPH}->get_term($term->acc);
	  #if( $full_term ){
	  #  $aspect = $full_term->get_code_from_namespace;
	  #}else{
	  #  $full_term =
	  #    $self->{ONTOLOGY_GRAPH}->get_term({synonym => $term->acc});
	  #  if( $full_term ){
	  #    $aspect = $full_term->get_code_from_namespace;
	  #  }
	  #}

	  ##
	  if ( $aspect eq 'cellular_component' ||
	       $aspect eq 'C' || $aspect eq 'c' ) {
	    push @cols, 'C';
	  } elsif ( $aspect eq 'molecular_function' ||
		    $aspect eq 'F' || $aspect eq 'f' ) {
	    push @cols, 'F';
	  } elsif ( $aspect eq 'biological_process' ||
		    $aspect eq 'P' || $aspect eq 'p' ) {
	    push @cols, 'P';
 	  } else {
	    ##
 	    $self->{SUCCESS} = 0;
 	    $self->{ERROR_MESSAGE} =
 	      'AmiGO::Slimmer::add_list, unknown aspect: ' . $term->acc.
		' (' . $term->namespace . ')';
 	    push @cols, '?';
	  }

	  # 10 DB_Object_Name (optional)
	  if ( $gp->full_name ) {
	    push @cols, $gp->full_name;
	  }else{
	    push @cols, '';
	  }

	  # 11 Synonym (optional)
	  my $syn_listref = $gp->synonym_list;
	  if ( $syn_listref && @$syn_listref ) {
	    @minibuf = ();
	    foreach my $syn (@$syn_listref) {
	      push @minibuf, $syn; }
	    $str = join '|', @minibuf;
	    push @cols, $str;
	  }else{
	    push @cols, '';
	  }

	  # TODO/BUG: catch cases like this...
	  eval{

	    push @cols, $gp->type;# 12 DB_Object_type
	    push @cols, 'taxon:' . $gp->species->ncbi_taxa_id; # 13 Taxon
	    push @cols, $assoc->assocdate; # 14 association.assoc_date
	    push @cols, $assoc->assigned_by || ''; # 15 assigned_by
	  };
	  if( $@ ){
	    $self->kvetch('die: term->acc: ' . $term->acc);
	    die "gp ncbi death triggered";
	  }

	  ## I got 'em, right?
	  #if( scalar(@cols) != 15 ){
	  if( scalar(@cols) < 15 ){ # new cols in new vers, right?

	    $self->{SUCCESS} = 0;
	    $self->{ERROR_MESSAGE} =
	      'AmiGO::Slimmer::add_list, failed to read line';

	    #}elsif( @cols ){
	    #  my $s = join "\t", @cols;
	    #  print STDERR $s . "\n";
	    #  sleep 1;

	  }else{

	    ##
	    ## Now that we've parsed the list, process the data.
	    ##

	    ## Split into the columns and get the bits we need.
	    #my $prod = $cols[1];
	    my $prod = $cols[0] . ':' . $cols[1];
	    my $prod_symbol = $cols[2];
	    #my $type = $cols[2];
	    #my $type = $cols[2];
	    my $is_not = $cols[3];
	    my $acc = $cols[4];

	    ## ## Chris's juggling? TODO: Why?
	    ## my $term = $self->{ONTOLOGY_GRAPH}->get_term_by_name($type);

	    ## Check for input errors and skip conditions before
	    ## dropping to main.
	    if( ! $acc ){
	      $self->{ERROR_MESSAGE} = 'faulty line, no acc: ';
	      $self->{SUCCESS} = 0;
	    }elsif( ! $prod || $prod =~ /^\:/ || $prod =~ /\:$/ ){
	      $self->{ERROR_MESSAGE} = 'faulty line, no product: ';
	      $self->{SUCCESS} = 0;

	      #}elsif( ! $term ){
	      #  $self->{ERROR_MESSAGE} = 'could not find term in ontology: (' .
	      #	$type . '): ' .
	      #	  $line;
	      #$self->{SUCCESS} = 0;

	    }elsif( $is_not && $is_not =~ /^not$/i ){
	      ## Skip things with a NOT qualifier.
	    }else{

	      ## If it is a direct association at the root...
	      if ( $self->{ONTOLOGY_ROOTS_HASH}{$acc} ) {
		$self->{ONTOLOGY_ROOTS_ASSOC_HASH}{$acc} = {}
		  if ! defined( $self->{ONTOLOGY_ROOTS_ASSOC_HASH}{$acc} );
		$self->{ONTOLOGY_ROOTS_ASSOC_HASH}{$acc}{$prod} = $prod_symbol;
	      }


	      ## Use better name and map the annotated GO term up to
	      ## the slim term(s).  $acc = $term->acc;
	      #$self->kvetch('call map_to_subset');
	      my ($leaf_pnodes, $all_pnodes) = $self->map_to_subset($acc);

	      #$self->{GPS}{$prod} = 1;
	      $self->{GPS}{$prod} = $prod_symbol;
	      $self->{TERMS_ASSOCIATED_WITH_GPS}{$acc} = 1;

	      ## Add interesting bits to self: count and mark the gene
	      ## product as belonging to that slim term.
	      $self->{COUNTED}{$acc . $prod} = 1;
	      foreach my $leaf (@$leaf_pnodes){
		$self->{LEAFH}{$leaf}->{$prod} = 1;
		#$self->kvetch('LEAF: (' . $leaf . ", " . $prod . ")");
	      }
	      foreach my $anc (@$all_pnodes){
		$self->{ALLH}{$anc}->{$prod} = 1;
		#$self->kvetch('ANC: (' . $anc . ", " . $prod . ")");
		## Associate an acc with a list of gps.
		if( ! $self->{GP_MAPPINGS}{$anc} ){
		  $self->{GP_MAPPINGS}{$anc} = {};
		}
		if( ! $self->{GP_MAPPINGS}{$anc}{$prod} ){
		  my $best = $prod_symbol || $prod;
		  $self->{GP_MAPPINGS}{$anc}{$prod} = $best;
		  #$self->kvetch('GPMAP: ('. $anc .", ".$prod.', '. $best .")");
		}
	      }

	      ## Use the leaves to bump out the 
	      foreach my $replacement_acc (@$leaf_pnodes) {

		$cols[4] = $replacement_acc;
		my @new_row = (
			       $cols[0], $cols[1], $cols[2],
			       $cols[3], $cols[4], $cols[5],
			       $cols[6], $cols[7], $cols[8],
			       $cols[9], $cols[10], $cols[11],
			       $cols[12], $cols[13], $cols[14]
			      );
		push @{$self->{NEW_GA_COLUMNS}}, \@new_row;
		#push @{$self->{NEW_GA_LINES}}, join("\t", @cols);
		#print STDERR join("\t", @cols), "\n";
	      }
	    }
	  }
	}
      }
    }
  }else{
    if( ! $term_l ){
      $self->{ERROR_MESSAGE} = 'failed to supply necessary arguments';
      $self->{SUCCESS} = 0;
    }
  }
}


##
sub get_number_of_added_terms {
  my $self = shift;
  return scalar( keys %{$self->{TERMS_ASSOCIATED_WITH_GPS}} );
}


##
sub get_number_of_added_gps {
  my $self = shift;
  return scalar( keys %{$self->{GPS}} );
}


## Returns a hash ref of acc to list of gps
sub get_terms_to_gps_mappings {
  my $self = shift;

  my $answer = {};
  foreach my $acc (keys %{$self->{GP_MAPPINGS}} ){

    $answer->{$acc} = [];

    if ( defined( $self->{ONTOLOGY_ROOTS_HASH}{$acc} )) {

      foreach my $prod (keys %{$self->{ONTOLOGY_ROOTS_ASSOC_HASH}{$acc}} ){
	push @{$answer->{$acc}},
	  {
	   ACC => $prod,
	   SYMBOL => $self->{ONTOLOGY_ROOTS_ASSOC_HASH}{$acc}{$prod},
	   IS_ROOT_ASSOC => 1,
	  };
      }

    }else{

      foreach my $prod (keys %{$self->{GP_MAPPINGS}{$acc}} ){
	push @{$answer->{$acc}},
	  {
	   ACC => $prod,
	   SYMBOL => $self->{GP_MAPPINGS}{$acc}{$prod},
	   IS_ROOT_ASSOC => 0,
	  };
      }
    }

    ## Sort it.
    my $final_ret = $answer->{$acc};
    $answer->{$acc} =
      [sort {
	my $x = $a->{SYMBOL};
	my $y = $b->{SYMBOL};
	$x = $a->{ACC} if ! defined $x;
	$y = $b->{ACC} if ! defined $y;
	return lc($x) cmp lc($y);
      } @$final_ret];
  }

  return $answer;
}


## Returns a hash of array refs to a arrays of hashes containing data like:
## acc, name, counts, obsolete_p, bucket_term_p, etc.
sub get_counts {

  my $self = shift;

  my @all_results = ();
  my @bp_results = ();
  my @cc_results = ();
  my @mf_results = ();
  my @bucket_results = ();

  ## Create union set of the slim set...
  my @union = ();
  push @union, @{$self->{SLIM_SET}};
  ## ...and bucket terms if necessary.
  if( $self->{BUCKET} ){
    $self->kvetch('BUCKET: ' . $self->{BUCKET});
    $self->{SLIM_GRAPH}->iterate(
				 sub {
				   my $ni = shift;
				   my $t = $ni->term;
				   return if $t->is_relationship_type;
				   my $acc = $t->acc;
				   push @union, $t
				     if $acc !~ /^[a-zA-Z]+\:\d+$/ &&
				       $acc !~ /^all$/;
				   return;
				 }, {no_duplicates=>1});
  }

  ## WARNING:
  ## Generate a hash of the slim terms (including bucket terms if
  ## applicable).
  #%{$self->{SLIM_SET_HASH}} = map {$_->acc => $_} @union;

  ## Iterate over unioned set.
  foreach my $t (@union){

    #my $ni = shift;
    #my $t = $ni->term;
    return if $t->is_relationship_type;

    ## Get equivalent term in GO-full.
    my $acc = $t->acc;
    my $t_full;
    if( $acc ){
      $t_full = $self->{ONTOLOGY_GRAPH}->get_term($acc);
    }else{
      # no equivalent term - the slim id has been
      # retired and not tracked; this should
      # only happen with old slims
      $acc = "NO_ACC";
    }

    my $count_leaf =
      scalar(keys %{$self->{LEAFH}{$acc} || {}}) || 0;
    my $count_all =
      scalar(keys %{$self->{ALLH}{$acc} || {}}) || 0;

    #next if  $count_all == 0;

    my $foo = {};
    $foo->{ACC} = $acc;
    $foo->{NAME} = $t->name; # can't use _full in case a bucket term
    $foo->{COUNT_DIRECT_SLIM} = $count_leaf || 0;
    $foo->{COUNT_ALL_SLIM} = $count_all || 0;
    $foo->{COUNT_SLIM} = $foo->{COUNT_ALL_SLIM};

    ## Again, if root term.
    $foo->{COUNT_SLIM} = $foo->{COUNT_ALL_SLIM};
    $foo->{IS_ROOT_TERM} = 0;
    if (  $self->{ONTOLOGY_ROOTS_HASH}{$acc} ) {
      $foo->{IS_ROOT_TERM} = 1;
      $foo->{COUNT_SLIM} = $foo->{COUNT_DIRECT_SLIM};
    }

    ## Do these number make sense or mean anything?
    #my $num_found = scalar( keys %{$self->{TERMS_ASSOCIATED_WITH_GPS}} );
    my $num_found = scalar( keys %{$self->{GPS}} );

    $foo->{DIRECT_PERCENT} = 0;
    $foo->{DIRECT_PERCENT} =
      sprintf("%.1f", ($foo->{COUNT_SLIM} / $num_found ) * 100.0)
	if $num_found != 0;

    $foo->{ALL_PERCENT} = 0;
    $foo->{ALL_PERCENT} =
      sprintf("%.1f", ($foo->{COUNT_SLIM} / $num_found ) * 100.0)
	if $num_found != 0;

    $foo->{OBSOLETE} = '';
    $foo->{OBSOLETE} = 'OBSOLETE'
      if $t_full && $t_full->is_obsolete;
    ## BUG: These are now worthless
    $foo->{IS_BUCKET_TERM} = 0;
    $foo->{IS_BUCKET_TERM} = 1
      if $acc !~ /^[a-zA-Z]+\:\d+$/ &&
	$acc !~ /^all$/;
    ## BUG:
    $foo->{TYPE} = 'undefined'; # see NAME above
    if( ! $foo->{IS_BUCKET_TERM} ){
      $foo->{TYPE} = $t->get_code_from_namespace;
      $self->kvetch('I_B_T: '. $foo->{ACC}. ', no');
    }else{
      $self->kvetch('I_B_T: ' . $foo->{ACC} . ', ' . $foo->{TYPE} . ', yes');
    }
    #$foo->{TYPE} = $t_full->type || 'undefined'; # see NAME above
    #$foo->{TYPE} = 'undefined'; # see NAME above

    #push @all_results, $foo;
    if( $foo->{TYPE} eq 'C' ){
      push @cc_results, $foo;
    }elsif( $foo->{TYPE} eq 'F' ){
      push @mf_results, $foo;
    }elsif( $foo->{TYPE} eq 'P' ){
      push @bp_results, $foo;
    }elsif( $foo->{TYPE} eq 'undefined' &&
	    $foo->{IS_BUCKET_TERM} ){
      push @bucket_results, $foo;
    }

    ## This is used for terms not caught by the slim.
    $self->{UNIONED_TERM_MAP_RESULTS}{$foo->{ACC}} = 1;
    #print STDERR ")))" . $foo->{ACC} . "\n";
    #print STDERR "." . "\n";
  }

  #print STDERR ";" . "\n";

  ## Sort 'em all out
  #@all_results = sort {
  #  $b->{COUNT_SLIM} <=> $a->{COUNT_SLIM};
  #} @all_results;
  @cc_results = sort {
    $b->{COUNT_SLIM} <=> $a->{COUNT_SLIM};
  } @cc_results;
  @mf_results = sort {
    $b->{COUNT_SLIM} <=> $a->{COUNT_SLIM};
  } @mf_results;
  @bp_results = sort {
    $b->{COUNT_SLIM} <=> $a->{COUNT_SLIM};
  } @bp_results;
  @bucket_results = sort {
    #$b->{COUNT_SLIM} <=> $a->{COUNT_SLIM};
    my $cmp = lc($b->{NAME}) cmp lc($a->{NAME});
    if( $cmp == 1 ){
      $cmp = -1;
    }elsif( $cmp == -1 ){
      $cmp = 1;
    }
    return $cmp;
  } @bucket_results;

  #$self->kvetch('dump bucket: ' . Dumper(\@bucket_results));
  return {
	  BUCKET=>\@bucket_results,
	  BP=>\@bp_results,
	  CC=>\@cc_results,
	  MF=>\@mf_results,
	 };
}


## Returns an array of arrays containing columns for a new gene
## association file. Not a good method. Not sure what a more
## appropriate return system should be.
sub get_columns {

  my $self = shift;
  return $self->{NEW_GA_COLUMNS};
}


##
sub get_ontology_roots_association_hash {

  my $self = shift;
  return $self->{ONTOLOGY_ROOTS_ASSOC_HASH};
}


## WARNING: This must be run after get_count or madness ensues.
sub get_missed {

  my $self = shift;

  ## Construct a hash with all mapped GPs.
  my %mapped_gps = ();
  foreach my $acc (keys %{$self->{UNIONED_TERM_MAP_RESULTS}} ){
    foreach my $gpi (keys %{$self->{GP_MAPPINGS}{$acc}} ){
      #$self->kvetch('acc: '.$acc.' gpi: '.$gpi);
      $mapped_gps{$gpi} = 1;
    }
  }

  ## Go through each of all GPs, and add to missing list if not found
  ## in mapped_gps.
  $self->kvetch('not found mapped: ');
  foreach my $gp (keys %{$self->{GPS}}) {

    ## If not found to be mapped, add it to the hash and array
    ## structures.
    #$self->kvetch('map?: ' . $gp . ', _' . ! defined($mapped_gps{$gp}) . '_');
    if ( ! defined($mapped_gps{$gp}) ){
      $self->{MISSED_GPS}{$gp} = $self->{GPS}{$gp};
    }

  }

  return $self->{MISSED_GPS};
}


## The returning data structure is a pointer to an array of hashes where:
##
##    array[0]{ACC} == <acc>
##    array[0]{LEAVES} == <a ref to an array of the leaf accs>
##    array[0]{ALL} == <a ref to an array of the all accs>
##
sub get_association_mappings {

  my $self = shift;
  my @results = ();

  ## Write slim mapping for all terms in the gene associations.
  #foreach my $acc (sort {$a cmp $b} (keys %{$self->{TERMS_ASSOCIATED_WITH_GPS}} )){
  foreach my $acc (keys %{$self->{TERMS_ASSOCIATED_WITH_GPS}} ){
    $self->kvetch('call map_to_subset');
    my ($leaf_pnodes, $all_pnodes) = $self->map_to_subset($acc);
    #print STDERR "MAPPING: $acc => @$leaf_pnodes // @$all_pnodes\n";
    push @results, { ACC => $acc,
		     LEAVES => $leaf_pnodes,
		     ALL => $all_pnodes };
  }

  return \@results;
}


## The returning data structure is a pointer to an array of hashes where:
##
##    array[0]{ACC} == <acc>
##    array[0]{LEAVES} == <a ref to an array of the leaf accs>
##    array[0]{ALL} == <a ref to an array of the all accs>
##
sub get_ontology_mappings {

  my $self = shift;
  my @results = ();

  my $sg = $self->{ONTOLOGY_GRAPH}; # just to readability (emacs tabbing)
  $sg->iterate(
	       sub {
		 my $ni = shift;
		 my $t = $ni->term;
		 return if $t->is_relationship_type;
		 my $acc = $t->acc;
		 $self->kvetch('call map_to_subset');
		 my ($leaf_pnodes, $all_pnodes) = $self->map_to_subset($acc);
		 push @results, { ACC => $acc,
				  LEAVES => $leaf_pnodes,
				  ALL => $all_pnodes };
		 return;},
	       {no_duplicates=>1});

  ## Write slim mapping for all GO terms.
  #     my $terms = $self->{ONTOLOGY_GRAPH}->get_all_terms;
  #     my $i = 0;
  #     foreach my $t (sort {$a->acc cmp $b->acc} @$terms) {
  #       print STDERR "__on:" . $i . "\n" if $i % 1000 == 0;
  #       $i++;
  #       my $acc = $t->acc;
  #       my ($leaf_pnodes, $all_pnodes) = $self->map_to_subset($acc);
  #       #print STDERR "MAPPING: $acc => @$leaf_pnodes // @$all_pnodes\n";
  #       push @results, { ACC => $acc,
  # 		       LEAVES => $leaf_pnodes,
  # 		       ALL => $all_pnodes };
  #     }

  return \@results;
}


## Returns an array ref to an array of two array refs. The first is an
## array of the slim terms to which the $acc hits first while climbing
## the tree. The second is all ancestors in the slim.
sub map_to_subset {

  my $self = shift;
  my $acc = shift;

  #$self->kvetch('acc: ' . $acc);

  ## Never recompute on the same accession
  #die( $self->{MEMOIZED_RESULTS} );
  my $memo = $self->{MEMOIZED_RESULTS}{$acc};
  return (@{$self->{MEMOIZED_RESULTS}->{$acc}}) if $memo; # return same result

  ## Ain't there, don't do--no such accession in GO.
  my $term = $self->{ONTOLOGY_GRAPH}->get_term($acc);
  return ([],[]) if ! $term;

  # Trace the paths to root of the input acc in the full GO (there may
  # be multiple paths to the root)
  my $paths = $self->{ONTOLOGY_GRAPH}->paths_to_top($acc);

  # keep hash, keyed by slim accession, boolean value -
  #  will have true if the slim term is an ancestor of $acc
  my %ancestorh = ();   # ALL ancestors of $acc in slim
  my %pancestorh = ();  # ancestors of $acc in slim for which there is
                        # a path through another ancestor

  my $number_of_slim_term_hits = 0;
  foreach my $path (@$paths) {
    my $terms = $path->term_list;
    unshift(@$terms, $term); # make path inclusive of base term

    # if there are "slop" terms (eg OTHER nucleotide binding)
    # AND there is an IMPLICIT path through this slop term,
    # then add this to the explicit path
    if( $self->{BUCKET} ){
      my $got_leaf = 0;
      @$terms =
	map {
	  my $slimt = $self->{SLIM_GRAPH_TERM_HASH}{$_};
	  my @R = ($_);
	  if ($slimt && !$got_leaf) {
	    my $crs = $self->{SLIM_GRAPH}->get_child_relationships($_);
	    my @brels = grep {$_->type eq "bucket"} @$crs;
	    if (@brels) {
	      my $bterm = $self->{SLIM_GRAPH}->get_term($brels[0]->acc2);
	      @R = ($bterm, $_);
	    }
	  }
	  if ($slimt) {
	    $got_leaf = 1;
	  }
	  @R;
	} @$terms;
    }

    my $got_leaf = 0;
    # follow path from $acc up to root, checking to
    # see if the intermediate term is in the slim
    foreach my $term (@$terms) {

      my $pacc = $term->acc;

      if ($self->{SLIM_GRAPH_TERM_HASH}{$pacc}) {
	# intermediate term is in the slim
	$ancestorh{$pacc} = 1;
	if ($got_leaf) {
	  $pancestorh{$pacc} = 1;
	}
	$got_leaf = 1;
      }

      ## See if we connect to the slim anywhere.
      if($self->{SLIM_SET_HASH}{$pacc}) {
	$number_of_slim_term_hits++;
      }
    }
  }

  $self->kvetch('number_of_slim_term_hits: ' . $number_of_slim_term_hits);
  if ( $number_of_slim_term_hits == 0 ) {
    $self->{MISSED_TERMS}{$acc} = 1;
  }

  # find unique ancestors, ie ancestors that are not intermediates to
  # another anestor
  my @uancestors = grep {!$pancestorh{$_}} keys %ancestorh;

  ## Memoize.
  $memo = [[@uancestors], [keys %ancestorh]];
  $self->{MEMOIZED_RESULTS}{$acc} = $memo;

  #$self->kvetch(Dumper($memo));

  return @$memo;
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



1;
