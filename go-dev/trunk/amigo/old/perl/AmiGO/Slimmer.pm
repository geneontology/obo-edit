#!/usr/bin/perl -w

##
## Taken from Chris's script.
##


use strict;

package AmiGO::Slimmer;

require Exporter;
my @ISA = qw(Exporter);
my @EXPORT = qw(new success error_message
		get_ontology_mappings get_association_mappings
		get_counts get_columns add_list);
my @EXPORT_OK = qw(map_to_subset);


## Takes an ontology graph, slim graph, and a binary argument for
## whether or not to use buckets.
sub new {

  my $class = shift;
  my $ontology_graph = shift || undef;
  my $slim_graph = shift || undef;
  my $slim_set = shift || undef;
  my $bucket = shift || 0;

  ## TODO: error checking
  #print STDERR '>>>[type]' . $type . "\n";
  #sleep 2;

  my $self = {};

  $self->{ONTOLOGY_GRAPH} = $ontology_graph;
  $self->{SLIM_GRAPH} = $slim_graph;
  $self->{SLIM_GRAPH_TERM_HASH} = {};
  $self->{SLIM_SET} = $slim_set;

  $self->{MEMOIZED_RESULTS} = {};

  $self->{TERMS_ASSOCIATED_WITH_GPS} = {};
  $self->{GPS} = {};
  $self->{NEW_GA_COLUMNS} = [];
  #$self->{NEW_GA_LINES} = [];

  $self->{GP_MAPPINGS} = {}; # gp map to terms

  $self->{BUCKET} = 0;
  $self->{BUCKET} = 1 if $bucket && $bucket == 1;

  ## Start catching errors.
  $self->{SUCCESS} = 1;
  $self->{ERROR_MESSAGE} = 'n/a';

  ## Basic checking
  if( ! $ontology_graph || ! $slim_graph || ! $slim_set ){
    $self->{SUCCESS} = 0;
    $self->{ERROR_MESSAGE} = 'all three necessary arguments were not supplied';
  }else{

    ## Add buckets if we want them.
    $self->{SLIM_GRAPH}->add_buckets if $self->{BUCKET};

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
  my $from_apph_p = shift || undef;

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
	  push @cols, $gp->speciesdb;# 1  DB
	  push @cols, $gp->acc;# 2  DB_Object_ID
	  push @cols, $gp->symbol;# 3  DB_Object_Symbol

	  # 4  NOT (optional)
	  if ( $assoc->is_not ) {
	    push @cols, 'NOT';
	  }else{
	    push @cols, '';
	  }

	  push @cols, $term->acc;# 5  GOid

	  # 6  DB:Reference
	  my $xref_listref = $ev->xref_list;
	  my @minibuf = ();
	  foreach my $xref ( @$xref_listref ) {
	    push @minibuf, $xref->dbname . ':' . $xref->xref_key; }
	  my $str = join '|', @minibuf;
	  push @cols, $str;

	  push @cols, $ev->code;# 7  Evidence

	  # 8  With/From (optional)
	  if(  $ev->seq_acc ){
	    push @cols, $ev->seq_acc;
	  }else{
	    push @cols, '';
	  }

	  ## Get aspect from the main ontology.
	  # 9  Aspect
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

	  # 10  DB_Object_Name (optional)
	  if ( $gp->full_name ) {
	    push @cols, $gp->full_name;
	  }else{
	    push @cols, '';
	  }

	  # 11  Synonym (optional)
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

	  #
	  push @cols, $gp->type;# 12  DB_Object_type
	  push @cols, 'taxon:' . $gp->species->ncbi_taxa_id; #13  Taxon
	  push @cols, $assoc->assocdate; # 14 association.assoc_date
	  push @cols, $assoc->assigned_by; # 15 assigned_by

	  ## I got 'em, right?
	  if( scalar(@cols) != 15 ){

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
	    my $prod = $cols[1];
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
	    }elsif( ! $prod ){
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

	      ## Use better name and map the annotated GO term up to
	      ## the slim term(s).  $acc = $term->acc;
	      my ($leaf_pnodes, $all_pnodes) = $self->map_to_subset($acc);

	      ## Add interesting bits to self: count and mark the gene
	      ## product as belonging to that slim term.
	      $self->{GPS}{$prod} = 1;
	      $self->{TERMS_ASSOCIATED_WITH_GPS}{$acc} = 1;
	      $self->{COUNTED}{$acc . $prod} = 1;
	      foreach my $leaf (@$leaf_pnodes){
		$self->{LEAFH}{$leaf}->{$prod} = 1;
		#print STDERR "LEAF: (" . $leaf . ", " . $prod . ")\n";
	      }
	      foreach my $anc (@$all_pnodes){
		$self->{ALLH}{$anc}->{$prod} = 1;
		#print STDERR "ANC: (" . $anc . ", " . $prod . ")\n";

		## Associate an acc with a list of gps.
		$self->{GP_MAPPINGS}{$anc} = {}
		  if ! $self->{GP_MAPPINGS}{$anc};
		$self->{GP_MAPPINGS}{$anc}{$prod} = 1
		  if ! $self->{GP_MAPPINGS}{$anc}{$prod};
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
    foreach my $prod (keys %{$self->{GP_MAPPINGS}{$acc}} ){
      push @{$answer->{$acc}}, $prod;
    }
  }

  return $answer;
}


## Returns an array ref to an array of hashes containing data like:
## acc, name, counts, obsolete_p, bucket_term_p, etc.
sub get_counts {

  my $self = shift;

  my @results = ();

  ## Create union set of the slim set and bucket terms if necessary.
  my @union = ();
  push @union, @{$self->{SLIM_SET}};
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
			       }, {no_duplicates=>1})
    if $self->{BUCKET};

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

    ## Do these number make sense or mean anything?
    #my $num_found = scalar( keys %{$self->{TERMS_ASSOCIATED_WITH_GPS}} );
    my $num_found = scalar( keys %{$self->{GPS}} );
    $foo->{DIRECT_PERCENT} =
      sprintf("%.1f", ($foo->{COUNT_DIRECT_SLIM} / $num_found ) * 100.0);
    $foo->{ALL_PERCENT} =
      sprintf("%.1f", ($foo->{COUNT_ALL_SLIM} / $num_found ) * 100.0);
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
    $foo->{TYPE} = $t->get_code_from_namespace
      if ! $foo->{IS_BUCKET_TERM};
    #$foo->{TYPE} = $t_full->type || 'undefined'; # see NAME above
    #$foo->{TYPE} = 'undefined'; # see NAME above

    push @results, $foo;
  }

@results = sort {
    $b->{COUNT_ALL_SLIM} <=> $a->{COUNT_ALL_SLIM};
    #$b->{COUNT_DIRECT_SLIM} <=> $a->{COUNT_DIRECT_SLIM};
  } @results;

  return \@results;
}


## Returns an array of arrays containing columns for a new gene
## association file. Not a good method. Not sure what a more
## appropriate return system should be.
sub get_columns {

  my $self = shift;
  return $self->{NEW_GA_COLUMNS};
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

  ## Never recompute on the same accession
  #print STDERR "FOO___" . $acc . "___BAR\n";
  #sleep 2;
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
    }
  }

  # find unique ancestors, ie ancestors that are not intermediates to
  # another anestor
  my @uancestors = grep {!$pancestorh{$_}} keys %ancestorh;
  $memo = [[@uancestors], [keys %ancestorh]];
  #printf STDERR "SLIM($acc) = @{$memo->[0]} // @{$memo->[1]}\n";
  $self->{MEMOIZED_RESULTS}{$acc} = $memo;

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


# sub get_counts {

#   my $self = shift;

#   my @results = ();

#   my $sg = $self->{SLIM_GRAPH}; # just for readability (emacs tabbing)
#   $sg->iterate(
# 	       sub {
# 		 my $ni = shift;
# 		 my $t = $ni->term;
# 		 return if $t->is_relationship_type;

# 		 ## Get equivalent term in GO-full.
# 		 my $acc = $t->acc;
# 		 my $t_full;
# 		 if( $acc ){
# 		   $t_full = $self->{ONTOLOGY_GRAPH}->get_term($acc);
# 		 }else{
# 		   # no equivalent term - the slim id has been
# 		   # retired and not tracked; this should
# 		   # only happen with old slims
# 		   $acc = "NO_ACC";
# 		 }

# 		 my $count_leaf =
# 		   scalar(keys %{$self->{LEAFH}{$acc} || {}}) || 0;
# 		 my $count_all =
# 		   scalar(keys %{$self->{ALLH}{$acc} || {}}) || 0;

# 		 #next if  $count_all == 0;

# 		 my $foo = {};
# 		 $foo->{ACC} = $acc;
# 		 $foo->{NAME} = $t->name; # can't use _full in case a bucket t
# 		 $foo->{COUNT_DIRECT_SLIM} = $count_leaf || 0;
# 		 $foo->{COUNT_ALL_SLIM} = $count_all || 0;
# 		 $foo->{OBSOLETE} = '';
# 		 $foo->{OBSOLETE} = 'OBSOLETE'
# 		   if $t_full && $t_full->is_obsolete;
# 		 $foo->{IS_BUCKET_TERM} = 0;
# 		 $foo->{IS_BUCKET_TERM} = 1
# 		   if $acc !~ /^[a-zA-Z]+\:\d+$/;
# 		 ## BUG:
# 		 #$foo->{TYPE} = $t_full->type || 'undefined'; # see NAME above
# 		 $foo->{TYPE} = 'undefined'; # see NAME above
# 		 #die "TESTING: died: " . $acc

# 		 ## BUG in how the slim graph is made.
# 		 #if( $foo->{IS_BUCKET_TERM} || $self->{SLIM_GRAPH_TERM_HASH}{$acc} ){
# 		 #  die "WTF: " . $self->{SLIM_GRAPH_TERM_HASH}{$acc};
# 		 push @results, $foo;
# 		 #}

# 		 #printf STDERR ("%s %s (%s)\t%d\t%d\t%s\t%s\n",
# 		#		$acc,
# 		#		$t->name,
# 		#		$t2 && $t2->name ? $t2->name : '?',
# 		#		$count_leaf || 0,
# 		#		$count_all || 0,
# 		#		$t2 && $t2->is_obsolete ? 'OBSOLETE' : '',
# 		#		$t->type || '',
# 		#	       );
# 		 return;
# 	       }, {no_duplicates=>1});

#   @results = sort {
#     $b->{COUNT_ALL_SLIM} <=> $a->{COUNT_ALL_SLIM};
#     #$b->{COUNT_DIRECT_SLIM} <=> $a->{COUNT_DIRECT_SLIM};
#   } @results;

#   return \@results;
# }


##
# sub add_ga_line {

#   my $self = shift;
#   my $line = shift || '';

#   chomp($line);

#   ## If legit and not a comment and we've been successful.
#   if( $line && $line !~ /^\!/ && $self->{SUCCESS} ){

#     ## Split into the columns and get the bits we need.
#     my @cols = split('\t', $_);
#     my $prod = $cols[1];
#     #my $type = $cols[2];
#     my $is_not = $cols[3];
#     my $acc = $cols[4];

#     ## ## Chris's juggling? TODO: Why?
#     ## my $term = $self->{ONTOLOGY_GRAPH}->get_term_by_name($type);

#     ## Check for input errors and skip conditions before dropping to
#     ## main.
#     if( ! $acc ){
#       $self->{ERROR_MESSAGE} = 'faulty line, no acc: ' . $line;
#       $self->{SUCCESS} = 0;
#     }elsif( ! $prod ){
#       $self->{ERROR_MESSAGE} = 'faulty line, no product: ' . $line;
#       $self->{SUCCESS} = 0;

#       #}elsif( ! $term ){
#       #  $self->{ERROR_MESSAGE} = 'could not find term in ontology: (' .
#       #	$type . '): ' .
#       #	  $line;
#       #$self->{SUCCESS} = 0;

#     }elsif( $is_not && $is_not =~ /^not$/i ){
#       ## Skip things with a NOT qualifier.
#     }else{

#       ## Use better name and map the annotated GO term up to the slim
#       ## term(s).
#       #$acc = $term->acc;
#       my ($leaf_pnodes, $all_pnodes) = $self->map_to_subset($acc);

#       ## Add interesting bits to self: count and mark the gene product
#       ## as belonging to that slim term.
#       $self->{TERMS_ASSOCIATED_WITH_GPS}{$acc} = 1;
#       $self->{COUNTED}{$acc . $prod} = 1;
#       $self->{LEAFH}{$_}->{$prod} = 1 foreach @$leaf_pnodes;
#       $self->{ALLH}{$_}->{$prod} = 1 foreach @$all_pnodes;

#       ## Use the leaves to bump out the 
#       foreach my $replacement_acc (@$leaf_pnodes) {
# 	$cols[4] = $replacement_acc;
# 	push @{$self->{NEW_GA_LINES}}, join("\t", @cols);
# 	#print STDERR join("\t", @cols), "\n";
#       }
#     }
#   }
# }


1;
