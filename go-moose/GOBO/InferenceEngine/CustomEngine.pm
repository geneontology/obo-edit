package GOBO::InferenceEngine::CustomEngine;
use Moose;
use Data::Dumper;
extends 'GOBO::InferenceEngine';


## test_sub: a subroutine that should return 1 if the inference engine is
## to stop iterating through the graph
has test_sub => (traits => ['Code'], is => 'rw', isa => 'CodeRef', default => sub { sub { return 0 } }, predicate => 'has_sub', handles => { test => 'execute' } );

## graph edge data, stored in the form
## { node_rel_target }{ $node_id }{ $relation_id }{ $target_id } = 1
##
## other rearrangements are also stored with the appropriate key,
## e.g. rel_target_node, target_node_rel, etc.
##
has graph_data_h => (is=>'rw', isa=>'HashRef[HashRef[HashRef[HashRef[Str]]]]');

=head2 get_inferred_outgoing_edges

 input:  hash containing
          node => $term_to_get_statements_for
          from_ix => name of the statement index to get statements from
          save_ix => name of the statement index to save the new statements in
 output: arrayref of inferred statements

If a subroutine is specified in $self->test_sub, it will be run on each edge
discovered; if the subroutine returns 0, the inference engine will continue to
iterate through the graph, collecting edges. If the sub returns 1, the IE will
not look for further edges connected to the edge currently being tested.

=cut

override 'get_inferred_outgoing_edges' => sub {
	my $self = shift;
	my %args = (
		from_ix => $self->from_ix,  ## default
		save_ix => $self->save_ix,  ## default
		@_
	);

	confess( (caller(0))[3]  . ": no node specified! " ) unless $args{node} || $args{node_id};
	if (! $args{node})
	{	$args{node} = $self->graph->get_node( $args{node_id} ) || new GOBO::Node( id=>$args{node_id} );
	}

	## make sure we have the index in question...
	if ($self->graph->exists_statement_ix($args{save_ix}))
	{	## same as get_outgoing_statements_in_ix(ix=>$args{save_ix}, node=>$args{node})
		my $tlinks = $self->graph->get_matching_statements(node => $args{node}, ix=>$args{save_ix});
		if ($tlinks && @$tlinks)
		{	# cached
			return $tlinks;
		}
	}
	else
	{	## add the save index if it doesn't exist
		$self->graph->add_statement_ix($args{save_ix});
	}

	# initialize link set based on input node;
	# we will iteratively extend upwards
	my @links = @{$self->graph->get_matching_statements(node=>$args{node}, ix => $args{from_ix})};

	my %outlink_h = ();
	while (@links) {
		my $link = shift @links;
		next if $outlink_h{$link};
		$outlink_h{$link} = $link;

		## see if the link passes the test
		my $pass = $self->test($link);

		foreach my $srel (@{$self->get_subrelation_closure($link->relation)}) {
			my $newlink = $self->create_link_statement(
				node=>$link->node, relation=>$srel, target=>$link->target);
			$outlink_h{$newlink} = $newlink;
			## see if any of the subrel links pass the test
			$pass = $self->test($newlink) if ! $pass;
		}

		## the test was passed. Move on to the next link!
		next if $pass;

		## see if we have any cached links...
		my $more_links = $self->graph->get_matching_statements(node=>$link->target, ix=>$args{save_ix});
		if ($more_links && @$more_links)
		{	## we had cached links. We don't need to look at any of the child links
			foreach my $xlink (@$more_links)
			{	my $combined = $self->_combine_statements($link, $xlink);
				if ($combined && @$combined)
				{	$outlink_h{$_} = $_ foreach @$combined;
				}
			}
			next;
		}

		## no cached links. Let's find ourselves some links from the graph!
		foreach my $xlink (@{$self->graph->get_matching_statements(node=>$link->target, ix=>$args{from_ix})})
		{	my $combined = $self->_combine_statements($link, $xlink);
			if ($combined && @$combined)
			{	push(@links, @$combined);
			#	foreach (@$combined)
			#	{	push @links, $_ unless $outlink_h{$_};
			#	}
			}
		}

=cut
		# otherwise, check out the parents of this mofo!
		my @x_link_arr;
		my $i;
		## see if we already have any inferred links...
			if ($args{recursive})
			{	@x_link_arr = @{$self->get_inferred_outgoing_statements_recursive( (%args, node => $link->target ) )};
			}
			else
			{
				@x_link_arr = @{$g->get_outgoing_statements(node => $link->target, ix => $args{from_ix})};
			}
		}

		foreach my $xlink (@x_link_arr)
		{	my $combined = $self->_combine_statements($link, $xlink);
			if ($combined && @$combined)
			{	if (! $i )
				{	push(@links, @$combined);
				}
				else
				{	## otherwise, we've already done this node
					$outlink_h{$_} = $_ foreach @$combined;
				}
			}
		}
=cut

	}

	return [] unless values %outlink_h;

	$self->graph->add_statements_to_ix(statements => [values %outlink_h], ix => $args{save_ix});

	return [values %outlink_h];
};


=head2 get_inferred_incoming_statements

Does the same as get_inferred_outgoing_statements, but in the other direction

 input:  hash containing
          target => $term_to_get_statements_for
          from_ix => name of the statement index to get statements from
          save_ix => name of the statement index to save the new statements in
 output: arrayref of inferred statements

=cut

override 'get_inferred_incoming_edges' => sub {
	my $self = shift;
	my %args = (
		from_ix => $self->from_ix,  ## default
		save_ix => $self->save_ix,  ## default
		@_
	);

	confess( (caller(0))[3]  . ": no node specified! " ) unless $args{target} || $args{target_id};
	if (! $args{target})
	{	$args{target} = $self->graph->get_node( $args{target_id} ) || new GOBO::Node( id=>$args{target_id} );
	}


#print STDERR "doing get_inferred_incoming_statements with " . $n->id . "!\n";

	## make sure we have the index in question...
	if ($self->graph->exists_statement_ix($args{save_ix}))
	{	my $tlinks = $self->graph->get_matching_statements(target=>$args{target}, ix => $args{save_ix});
		if (@$tlinks) {
			# cached
			return $tlinks;
		}
	}
	else
	{	## add the save index if it doesn't exist
		$self->graph->add_statement_ix($args{save_ix});
	}

	# initialize link set based on input node;
	# we will iteratively extend upwards
	my @links = @{$self->graph->get_matching_statements(target=>$args{target}, ix=>$args{from_ix})};
#	printf STDERR "looking at $n => @links\n";

	my %inlink_h = ();

	while (@links) {
		my $link = shift @links;
		next if $inlink_h{$link};
		$inlink_h{$link} = $link;

		## see if the link passes the test
		my $pass = $self->test($link);

		foreach my $srel (@{$self->get_subrelation_closure($link->relation)}) {
			my $newlink = $self->create_link_statement(
				node=>$link->node, relation=>$srel, target=>$link->target);
			$inlink_h{$newlink} = $newlink;
			## see if any of the subrel links pass the test
			$pass = $self->test($newlink) if ! $pass;
		}

		## the test was passed. Move on to the next link!
		next if $pass;

		## see if we have any cached links...
		my $more_links = $self->graph->get_matching_statements(target=>$link->node, ix=>$args{save_ix});
		if ($more_links && @$more_links)
		{	## we had cached links. We don't need to look at any of the child links
			foreach my $xlink (@$more_links)
			{	my $combined = $self->_combine_statements($xlink, $link);
				if ($combined && @$combined)
				{	$inlink_h{$_} = $_ foreach @$combined;
				}
			}
			next;
		}

		## no cached links. Let's find ourselves some links from the graph!
		foreach my $xlink (@{$self->graph->get_matching_statements(target => $link->node, ix => $args{from_ix})})
		{	my $combined = $self->_combine_statements($xlink, $link);
			if ($combined && @$combined)
			{	push(@links, @$combined);
			}
		}
	}

	return [] unless values %inlink_h;

	$self->graph->add_statements_to_ix(statements => [values %inlink_h], ix => $args{save_ix});

	return [values %inlink_h];
};





=head2 slim_graph

Concatenates the various sub-functions involved in slimming

 input: graph      => Graph object
        subset_ids => arrayref of terms to slim to
        input_ids  => arrayref of term IDs to slim 'from' (all terms in graph
                      if not specified)
        from_ix    => $name_of_from_ix  ## optional; which statements to use to
                                        ## for slimming; defaults to $self->from_ix
        save_ix    => $name_of_save_ix  ## optional; where the slimmed edges
                                        ## should be saved; defaults to 'slimmed'

        options    => hashref of options
          return_as_graph => 1  ## to return the results as a proper Graph object

 output:
        with option 'return_as_graph' on: a slimmed Graph object in $self->graph
        otherwise, data hash in the form
        {graph}{ node_id }{ relation_id }{ target_id }

=cut


sub slim_graph {
	my $self = shift;

#	print STDERR 'args: ' . Dumper( \@_ ) . "\n\n\n";

	my %args = (
		from_ix => $self->from_ix,
		all_ix => 'subset_transitive_closure',
		save_ix => 'slimmed',
		@_
	);

	warn( (caller(0))[3] . ": warning: save_ix, all_ix and from_ix contain " ) if $args{save_ix} eq $args{all_ix} || $args{all_ix} eq $args{from_ix} || $args{from_ix} eq $args{save_ix};

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless defined $args{subset_ids};


	if ($args{save_ix})
	{	$self->save_ix( $args{save_ix} );
	}
	if ($args{from_ix})
	{	$self->from_ix( $args{from_ix} );
	}

	# no input: use all the terms in the graph as input
	if (! $args{input_ids})
	{	$args{input_ids} = [ map { $_->id } @{$self->graph->terms} ];
	}

	if (! $self->graph->get_statement_ix_by_name($self->from_ix))
	{	## make sure we've copied the asserted ontology links to a new index
		$self->graph->duplicate_statement_ix('ontology_links', $self->from_ix);
	}

	## give the inference engine a subroutine to execute upon each wee little term
	## so it stops looking for parent terms when it reaches a subset node
	my $sub = sub {
		my $x = shift;
#		print STDERR "subset: " . join(", ", @subset_ids) . "\n\n";
		return 1 if grep { $x->target->id eq $_ } @{$args{subset_ids}};
		return 0;
	};
	$self->test_sub( $sub );


	# get the links between the nodes
	$self->__create_edge_matrix( %args, save_ix => 'subset_transitive_closure' );
	# populate the node look up hashes
	$self->__populate_all_edge_matrices;

	$self->__trim_edge_matrix( trim_relations => 1 );

#	$self->dump_edge_matrix('N_R_T');

	if ($args{options} && $args{options}->{return_as_graph})
	{	# make a copy of the old graph, but without statements and nodes
		my $new_graph = new GOBO::Graph;

		## copy the graph metadata and relations
		GOBO::Util::GraphFunctions::copy_attributes(from => $self->graph, to => $new_graph, ignore => [ qw( statement_ix_h term_h ) ]);

		## convert the links in $self->edge_matrix->{N_T_R} into LinkStatements
		$self->__convert_matrix_to_edges(matrix => 'N_R_T', from => $self->graph, to => $new_graph, save_ix => $args{save_ix});

		## replace the graph in $self->graph with the new graph
		$self->graph($new_graph);
	}

}



sub get_closest_and_ancestral {
	my $self = shift;

	my %args = (
		closest_ix => 'closest',
		all_ix => 'all',
		@_
	);

	print STDERR 'get_closest_and_ancestral args: ' . Dumper( \@_ ) . "\n\n\n" if $ENV{VERBOSE};

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless defined $args{subset_ids};

	# no input: use all the terms in the graph as input
	if (! $args{input_ids})
	{	$args{input_ids} = [ map { $_->id } @{$self->graph->terms} ];
	}

	if (! $self->graph->get_statement_ix_by_name($self->from_ix))
	{	warn "statement index " . $self->from_ix . " is not populated!";
		## make sure we've copied the asserted ontology links to a new index
		$self->graph->duplicate_statement_ix('ontology_links', $self->from_ix);
	}

	# make a copy of the old graph, but without statements and nodes
	my $new_graph = new GOBO::Graph;

	## copy the graph metadata and relations
	GOBO::Util::GraphFunctions::copy_attributes(from => $self->graph, to => $new_graph, ignore => [ qw( statement_ix_h ) ]);

	print STDERR "done copy_attributes!\n" if $ENV{VERBOSE};

	# get the links between the nodes
	$self->__create_edge_matrix( %args, save_ix => 'subset_transitive_closure' );

	print STDERR "done __create_edge_matrix\n" if $ENV{VERBOSE};

	## convert the links in $self->edge_matrix->{N_R_T} into LinkStatements
	$self->__convert_matrix_to_edges(matrix => 'N_R_T', from => $self->graph, to => $new_graph, save_ix => $args{all_ix});

	print STDERR "done __convert_matrix_to_edges\n" if $ENV{VERBOSE};

#	# copy the matrix
#	$self->set_edge_matrix( 'N_R_T_copy' => $self->get_edge_matrix('N_R_T') );

	# populate the node look up hashes
	$self->__populate_all_edge_matrices;

	print STDERR "done __populate_all_edge_matrices\n" if $ENV{VERBOSE};

	$self->__trim_edge_matrix( trim_relations => 1 );

	print STDERR "done __trim_edge_matrix!\n" if $ENV{VERBOSE};

	$self->__convert_matrix_to_edges(matrix => 'N_R_T', from => $self->graph, to => $new_graph, save_ix => $args{closest_ix});

	print STDERR "done __convert_matrix_to_edges!\n" if $ENV{VERBOSE};

	$self->graph($new_graph);
}

=head2 get_closest_ancestral_edges_from_matrix

Find the closest ancestral terms of $node that pass a test and return that term
and the relation between it and $node_id


input:  node_id  => $n_id        ## find the closest ancestral node(s) of $n_id
        test     => sub { ... }  ## a subr that will return 1 if true

output: arrayref of hashrefs in the form
        [ { node_id => $n_id, relation_id => $r_id, target_id => $t_id }, ... ]

For a given term, finds the closest node[s]

=cut

sub get_closest_ancestral_edges_from_matrix {
	my $self = shift;
	my %args = ( @_ );
	my $n_id = $args{node_id};
	my $options = $args{options};

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless $n_id && $self->defined_edge_matrix('N_R_T');

	my $matrix = $self->edge_matrix;

	# if there are no links whatsoever involving this term, report and return undef
	if (! $matrix->{N_R_T}{$n_id} )
	{	print STDERR "No links from $n_id\n" if $ENV{VERBOSE};
		return undef;
	}

#	if ( $r_id )
#	{	# if a relation is specified, but there are no relations involving this term
#		# that match, return undef
#		if ( ! values %{$matrix->{N_R_T}{$n_id}{$r_id}} )
#		{	print STDERR "No $relation_id links from $n_id\n" if $options->{verbose};
#			return undef;
#		}
#		# if it is only connected to one node by the relation, it must be the closest!
#		elsif (scalar keys %{$matrix->{N_R_T}{$n_id}{$r_id}} == 1)
#		{	return [ map { { node_id => $n_id, relation_id => $r_id, target_id => $_ } } keys %{$matrix->{N_R_T}{$n_id}{$r_id}} ];
#		}
#	}

	# make sure the look up hashes are populated
	if (! $matrix->{N_T_R} || ! $matrix->{T_N_R} )
	{	$self->__populate_all_edge_matrices;
	}

	# only connected to one node: must be the closest!
	if (scalar keys %{$matrix->{N_T_R}{$n_id}} == 1)
	{	# find out what the target ID is
		my $t_id = (keys %{$matrix->{N_T_R}{$n_id}})[0];
#		# we specified a relation
#		if ($r_id)
#		{	return undef unless $matrix->{N_T_R}{$n_id}{$t_id}{$r_id};
#			return [ { node_id => $n_id, relation_id => $r_id, target_id => $t_id } ];
#		}
#		else
#		{
			return [ map { { node_id => $n_id, relation_id => $_, target_id => $t_id } } keys %{$matrix->{N_T_R}{$n_id}{$t_id}} ];
#		}
	}

	my $closest;
#	foreach my $rel (keys %{$matrix->{N_R_T}{$n_id}})
#	{	next if $r_id && $rel ne $r_id;

		## target_list contains all the terms connected to $n_id
		my @target_list = grep { $self->test($_) } keys %{$matrix->{N_T_R}{$n_id}};

		print STDERR "n_id: $n_id; targets: " . join(", ", sort @target_list) . "\n";

		REL_SLIMDOWN_LOOP:
		while (@target_list)
		{	## look at all the terms that n_id is connected to
			## try to work out where they are in relation to each other
			my $a = pop @target_list;
			my @target2_list;
			while (@target_list)
			{	my $b = pop @target_list;
				if ($matrix->{T_N_R}{$a} && $matrix->{T_N_R}{$a}{$b})
				{	#	we have a link from b [node] to a [target]
					#	b is closer to $n_id than a
					#	forget about a, go on to the next list item
					push @target_list, $b;
					push @target_list, @target2_list if @target2_list;
					next REL_SLIMDOWN_LOOP;
				}
				elsif ($matrix->{T_N_R}{$b} && $matrix->{T_N_R}{$b}{$a})
				{	#	we have a link from a [node] to b [target]
					#	a is closer to $n_id than b
					#	forget about b, look at the next in target_list
					next;
				}
				else
				{	#a and b aren't related
					#	keep b to check
					push @target2_list, $b;
					next;
				}
			}

			## a must be closer to $n_id than any of the terms in @target_list
			## add it to our matrix of closest terms
#			map { $closest->{$n_id}{$_}{$a}++ } keys %{$matrix->{T_N_R}{$n_id}{$a}};
			foreach (keys %{$matrix->{T_N_R}{$n_id}{$a}})
			{	push @$closest, { node_id => $n_id, target_id => $a, relation_id => $_ };
			}
			push @target_list, @target2_list if @target2_list;
		}
#	}

	return $closest || undef;
}


=head2 get_closest_ancestral_nodes_from_matrix

As

=cut

=cut

	handles => {
These methods are implemented in Moose::Meta::Attribute::Native::MethodProvider::Hash.

get($key, $key2, $key3...)
Returns values from the hash.

In list context return a list of values in the hash for the given keys. In scalar context returns the value for the last key specified.

set($key => $value, $key2 => $value2...)
Sets the elements in the hash to the given values.

delete($key, $key2, $key3...)
Removes the elements with the given keys.

keys
Returns the list of keys in the hash.

values
Returns the list of values in the hash.

exists($key)
Returns true if the given key is present in the hash.

defined($key)
Returns true if the value of a given key is defined.

kv
Returns the key/value pairs in the hash as an array of array references.

  for my $pair ( $object->options->pairs ) {
      print "$pair->[0] = $pair->[1]\n";
  }
elements
Returns the key/value pairs in the hash as a flattened list..

clear
Resets the hash to an empty value, like %hash = ().

count
Returns the number of elements in the hash. Also useful for not empty: has_options => 'count'.

is_empty
If the hash is populated, returns false. Otherwise, returns true.

accessor
If passed one argument, returns the value of the specified key. If passed two arguments, sets the value of the specified key.

	},
=cut



#sub get_inferred_outgoing_statements_recursive {
#	my $self = shift;
#	my %args = (@_);
#	return $self->get_inferred_outgoing_statements( @_, recursive => 1 );
#}


#*get_inferred_target_statements = &get_inferred_outgoing_statements;


#sub get_inferred_incoming_statements_recursive {
#	my $self = shift;
#	return $self->get_inferred_incoming_statements( @_, recursive => 1 );
#}


=cut

combining two links

link_a = X -- rel_a --> Y

link_b = Y -- rel_b --> Z

X -- rel_a --> Y -- rel_b --> Z

=>  X -- rel_x --> Z


sub _combine_statements {
	my $self = shift;
	my ($stt_a, $stt_b) = (@_);

	my @rels;
	## see if we already have this combo or not

	if (defined $self->relation_composition_h->{$stt_a->relation->id} && defined $self->relation_composition_h->{$stt_a->relation->id}{$stt_b->relation->id})
	{	@rels = @{$self->relation_composition_h->{$stt_a->relation->id}{$stt_b->relation->id}};
#		print STDERR "retrieving cached results for " . $stt_a->relation->id . "." . $stt_b->relation->id . ": " . join(", ", @rels) . "\n";
	}
	else
	{	my $rel_h;
		foreach my $rel_a (@{$self->get_subrelation_reflexive_closure($stt_a->relation)} )
		{	my @temp;
			#print STDERR "  XLINK: $xlink\n";
			my $rel_b = $stt_b->relation;
			@temp = $self->relation_composition($rel_a, $rel_b);
			# R1 subrelation_of R2, x R1 y => x R2 y
			@temp = map { @{$self->get_subrelation_reflexive_closure($_)} } @temp;
			map { $rel_h->{ $_->id } = $_ } @temp;
		}
		@rels = values %$rel_h;
		$self->relation_composition_h->{$stt_a->relation->id}{$stt_b->relation->id} = [ @rels ];
	}

#	return undef if ! @rels;
	my $statements;
	foreach my $rel (@rels)
	{	my $new_stt = new GOBO::LinkStatement(node => $stt_a->node, relation => $rel, target => $stt_b->target);
		# todo - provenance/evidence of statement
		push(@$statements, $new_stt);
	}
	return $statements;
}

=cut

=head2 get_inferred_graph

Infer all the links in the graph

input:  graph (as part of $self)
output: inferred graph


sub get_inferred_graph {
	my $self = shift;
	my %args = (@_);
	my $g = $self->graph;
#	my $ig = $self->inferred_graph;

	my $direction = shift;

	$g->update_graph;

	return unless $g->has_links;

	if ($direction && $direction eq 'from_leaves')
	{	my $leaves = $g->get_leaves;
		foreach (@$leaves)
		{	$self->get_inferred_outgoing_links_recursive( %args, node=>$_ );
		}
	}
	else
	{	## get the roots of the graph, and then recursively get the incoming links
		my $roots = $g->get_roots;

		foreach (@$roots)
		{	$self->get_inferred_incoming_links_recursive( %args, target=>$_ );
		}
	}

	## check that we covered all the terms...
	my @to_check;
	foreach (@{$g->terms})
	{	## look for it in t_done_h
		if (! $self->t_done_h->{$_->id})
		{	push @to_check, $_;
		}
		elsif (! $self->t_done_h->{$_->id}{all_in} )
		{	push @to_check, $_;
		}
	}

	if (@to_check)
	{	foreach (@to_check)
		{	my @links = ( @{$g->get_incoming_links(%args, target=>$_)} , @{$g->get_outgoing_links(%args, node=>$_)} );
			if (@links)
			{	print STDERR "Didn't get $_ in our checks!\n".join("\n", @links)."\n";
			}
		}
	}

	return $self->graph;
=cut


=cut
	# create the matrix from the graph
	my $results = $self->_create_matrix;
#	my $results = $self->_create_matrix( graph => $g );
	my $t_hash = $results->{term_mapping};
	my $matrix = $results->{matrix};

	# combine the relationships...
	my $rels_combined = $self->_combine_relations;

	return unless $t_hash && $matrix && $rels_combined;

	## now let's multiply up the matrix!
	$matrix = $self->_get_closure_matrix(matrix => $matrix, rel_combination_h => $rels_combined, term_mapping => $t_hash);

	## now convert the matrix into graph
	my %links;
	my $n_terms = scalar @$matrix - 1;
	my @t_list = (1 .. $n_terms);
#	for (my $i = 1; $i <= $n_terms; $i++)
	foreach my $i (@t_list)
	{	next unless $matrix->[$i];
#		for (my $j = 1; $j <= $n_terms; $j++)
		foreach my $j (@t_list)
		{	next unless defined $matrix->[$i][$j] && values %{$matrix->[$i][$j]};
			foreach (keys %{$matrix->[$i][$j]})
			{
				my $link = new GOBO::LinkStatement(
					node => $g->noderef( $t_hash->{by_acc}{$i} ),
					relation => $g->noderef($_),
					target => $g->noderef( $t_hash->{by_acc}{$j} ));
#				print STDERR "adding $link\n";
				$links{$link} = $link;
			}
		}
	}


	$ig->add_links( [ values %links ] );
#	$self->inferred_graph($ig);

	$self->named_graph_h({ all_links => $ig });
	$self->named_matrix_h({ all_links => { matrix => $matrix, term_mapping => $t_hash }});
	return $ig;
=cut
#}


=head2 get_reduced_graph

Remove the redundant links in the graph

input:  $self, graph => $graph or as part of $self
        input => arr of nodes to use as input (all if not stated)
        subset => arr of nodes to map to (all if not stated)
output: a graph with no redundant links (with any luck)

=cut

sub get_reduced_graph {
	my $self = shift;
	my $g = $self->graph;
	my $inferred = $self->get_inferred_graph;


#	my %args = ( graph => $self->graph, inferred => $self->inferred_graph, @_ );
#	my $g = $args{graph};
#	my $inferred = $args{inferred};

	$inferred->update_graph;

	return unless $inferred->has_links;

	$self->graph( $inferred );

	# create the matrix from the graph
	my $results = $self->_create_matrix;
#	my $results = $self->_create_matrix( graph => $inferred );
	my $t_hash = $results->{term_mapping};
	my $matrix = $results->{matrix};

	# combine the relationships...
	my $rels_combined = $self->_combine_relations;

	my $rel_reduction;
	foreach my $rel (@{$inferred->relations})
	{	## get the relation closure.
		foreach (@{$self->get_subrelation_closure($rel)})
		{	next if $_->id eq $rel->id;
			$rel_reduction->{$rel->id}{$_->id}++;
		}
	}

#	print STDERR "rel_reduction_h: " . Dumper($rel_reduction) . "\n\n";

#	foreach my $r1 (keys %$rels_combined)
#	{	foreach my $r2 (keys %{$rels_combined->{$r1}})
#		{	foreach my $r (keys %{$rels_combined->{$r1}{$r2}})
#			{	if ($rel_reduction->{$r})
#				{	delete $rels_combined->{$r1}{$r2}{$_} foreach keys %{$rel_reduction->{$r}};
#				}
#			}
#		}
#	}

	return unless $t_hash && $matrix && $rels_combined;

=cut
	## ok, let's see about subsets and inputs and all that jazz
	foreach my $l qw(subset input)
	{	if ($args{$l})
		{	foreach my $t (@{$args{$l}})
			{	## find the ID in t_hash and get the appropriate matrix row #
				next if ! $t_hash->{by_id}{$t};
				push @{$args{$l . "_acc_list"}}, $t_hash->{by_id}{$t};
			}
		}
	}

	undef $matrix->[$_] foreach @{$args{subset_acc_list}};
	foreach my $i (@$matrix)
	{	next unless defined $i;
		foreach (@{$args{subset_acc_list}})
		{	undef $i->[$_];
		}
	}
=cut
	## now let's multiply up the matrix!
#	my $results_2 = $self->_get_reduction_matrix(%args, matrix => $matrix, rel_combination_h => $rels_combined, term_mapping => $t_hash );
	my $results_2 = $self->_get_reduction_matrix(matrix => $matrix, rel_combination_h => $rels_combined, term_mapping => $t_hash );
	$matrix = $results_2->{matrix};

	print STDERR "NEW matrix:\n";
	$self->__dump_matrix( matrix => $matrix, term_mapping => $t_hash );


	my $rel_combos = $results_2->{rel_combos};

#	print STDERR "rel_combos: " . Dumper($rel_combos) . "\n\n";

	if ($rel_reduction)
	{	## go through the relationship combos and remove any redundancies
		foreach my $c (keys %{$rel_combos->{by_acc}})
		{	foreach my $r (keys %$rel_reduction)
			{	if ($rel_combos->{by_acc}{$c}{$r})
				{	delete $rel_combos->{by_acc}{$c}{$_} foreach keys %{$rel_reduction->{$r}};
				}
			}
		}
	}

#	print STDERR "rel combos: " . Dumper($rel_combos) . "\n\n";

	## now convert the matrix into graph
	my @links;
	my $n_terms = scalar @$matrix - 1;
	my @t_list = (1 .. $n_terms);
	foreach my $i (@t_list)
	{	next unless defined $matrix->[$i];
		foreach my $j (@t_list)
		{	next unless $matrix->[$i][$j];
#			print STDERR "looking at " . $t_hash->{by_acc}{$i} . " --> " . $t_hash->{by_acc}{$j} . ": " . $matrix->[$i][$j] . "\n";
			## get the relations, create links
#			foreach (keys %{$rel_combos->{by_acc}{ $matrix->[$i][$j] }})
			foreach (keys %{$matrix->[$i][$j]})
			{	push @links, new GOBO::LinkStatement(
					node=> $t_hash->{by_acc}{$i},
					relation=> $g->get_relation($_),
					target=> $t_hash->{by_acc}{$j});
			}
		}
	}

	$self->inferred_graph->add_links( \@links );
	return $self->inferred_graph;

}



=head2 _create_matrix

Create a matrix from graph data

input:  $graph
output: a matrix
        a hash containing the mapping of term ids to rows in the matrix


sub _create_matrix {
	my $self = shift;
	my $g = shift || $self->graph;

	$g->update_graph;

#	my %args = ( graph => $self->graph, @_ );
#	my $g = $args{graph};

	die "No graph present... aborting" if ! defined $g;
	die "No links in graph... aborting" unless $g->has_links;

	my $t_hash;
	my $matrix;
	my $acc = 1;
	# add the terms and links to the matrix
	foreach my $l (@{$g->links})
	{	foreach ($l->node, $l->target)
		{	if (! $t_hash->{by_id}{$_->id})
			{	$t_hash->{by_id}{$_->id} = $acc;
				$t_hash->{by_acc}{$acc} = $_->id;
				$acc++;
			}
		}

		foreach my $rel ($l->relation, @{$self->get_subrelation_closure($l->relation)})
		{	$matrix->[ $t_hash->{by_id}{$l->node->id} ][ $t_hash->{by_id}{$l->target->id} ]{ $rel->id } = 1;
		}
	}

	return { matrix => $matrix, term_mapping => $t_hash };
}

=cut

=head2 _combine_relations

Work out what different relation(s) would result from combining two links

input:  graph containing relations OR arrayref of relations
output: a hash of the form
          {rel_1_id}{rel_2_id}{ rel_a => 1, rel_b => 1 }
          where rel_a and rel_b are the result of combining rel_1 and rel_2


sub _combine_relations {
	my $self = shift;
	my %args = ( relations => $self->graph->relations, @_ );
#	my $relations = shift || $self->graph->relations;

	return unless $args{relations} && @{$args{relations}};

	my $rels_combined;
	## work out the results of various combinations of links
	foreach my $r1 (@{$args{relations}})
	{	foreach my $r2 (@{$args{relations}})
		{	foreach my $rc_1 (@{$self->get_subrelation_reflexive_closure($r1)})
			{	my @rels = $self->relation_composition($rc_1, $r2);
				@rels = map { @{$self->get_subrelation_reflexive_closure($_)} } @rels;
				foreach my $rel (@rels)
				{	$rels_combined->{ $r1->id }{ $r2->id }{$rel->id}++;
				}
			}
		}
	}
	return $rels_combined;
}
=cut

=head2 _get_closure_matrix

Get every possible combination of things in the matrix

input:  matrix,
output:


sub _get_closure_matrix {
	my $self = shift;
	my %args = (@_);

	$self->__create_edge_matrix unless $self->edge_matrix->{N_T_R};
	$self->__populate_all_edge_matrices unless $self->edge_matrix->{T_N_R};

	my $matrix = $self->edge_matrix;

	my $rel_combo_h = $self->relation_composition_h;
=cut


=cut

transitive closure algorithm:

foreach k (matrix_terms)
	foreach i (matrix_terms)
		foreach j (matrix_terms)
			if



	foreach my $k (keys %{$matrix->{T_N_R}})
	{	## needs to have incoming terms too
		next unless $matrix->{N_T_R}{$k};
		## looking for i->k
		foreach my $i (keys %{$matrix->{T_N_R}{$k}})
		{	## looking for k->j and i->j
			next if $i eq $k;
			foreach my $j (keys %{$matrix->{N_T_R}{$k}})
			{	next if $j eq $i || $j eq $k;

				## we have the rels between $i and $j in keys %{$matrix->{N_T_R}{$i}{$j}

				my $r1_combo = join(".", sort keys %{$matrix->{N_T_R}{$i}{$k}});
				my $r2_combo = join(".", sort keys %{$matrix->{N_T_R}{$k}{$j}});

				if ($rel_combo_h->{$r1_combo} && $rel_combo_h->{$r1_combo}{$r2_combo})
				{	## we've seen this combination before
					map { $matrix->{N_T_R}{$i}{$j}{$_}++ } @{$rel_combo_h->{$r1_combo}{$r2_combo}};
				}
				else
				{	## this is a new combination...
					my $temp_h = {};
					## for every link of relation $r1 between i and k
					foreach my $r1 (keys %{$matrix->{N_T_R}{$i}{$k}})
					{	## for every link of relation $r2 between k and j
						foreach my $r2 (keys %{$matrix->{N_T_R}{$k}{$j}})
						{	## see if $r1 . $r2 results in a relation or not

							#map { $temp_h->{$_}++ } @{$rel_combo_h->{$r1}{$r2}};
						}
					}
					foreach (keys %$temp_h)
					{

					}
					$rel_combo_h->{$r1_combo}{$r2_combo} = [ keys %$temp_h ];
				}
			}
		}
	}



	foreach my $k (keys %{$matrix->{N_T_R}})
	{	foreach my $i (@t_list)
		{	next if ! defined $matrix->[$i];
			next if $i == $k;
			foreach my $j (@t_list)
			{	# not interested in self-self links
				next if $k == $j || $i == $j;
				## there may already be links between $i and $j -- this doesn't matter
				if (defined $matrix->[$i][$k] && defined $matrix->[$k][$j])
				{	## combine the links and save
					my $r1_combo = join(".", sort keys %{$matrix->[$i][$k]});
					my $r2_combo = join(".", sort keys %{$matrix->[$k][$j]});

					if ($rels_combined->{$r1_combo} && $rels_combined->{$r1_combo}{$r2_combo})
					{	## we've seen this combination before
						$matrix->[$i][$j]{$_}++ foreach keys %{$rels_combined->{$r1_combo}{$r2_combo}};
						next;
					}

					## this is a new combination...
					## for every link of relation $r1 between i and k
					foreach my $r1 (keys %{$matrix->[$i][$k]})
					{	next unless $rels_combined->{$r1};
						## for every link of relation $r2 between k and j
						foreach my $r2 (keys %{$matrix->[$k][$j]})
						{	## see if $r1 . $r2 results in a relation or not; if it does,
							## add this relation to i -> j
							if (defined $rels_combined->{$r1}{$r2})
							{	map
								{ $matrix->[$i][$j]{$_}++; } keys %{$rels_combined->{$r1}{$r2}};
							}
						}
					}
					if (defined $matrix->[$i][$j])
					{	map { $rels_combined->{$r1_combo}{$r2_combo}{$_}++ } keys %{$matrix->[$i][$j]};
					}
					else
					{	$rels_combined->{$r1_combo}{$r2_combo} = {};
					}
				}
			}
		}
	}

	return $matrix;
}
=cut

=head2 _get_full_matrix_rel_independent

Get every possible combination of things in the matrix

input:  matrix,
output:


sub _get_full_matrix_rel_independent {
	my $self = shift;
	my %args = (@_);
	my $matrix = $args{matrix};
	my $new_matrix;

	foreach my $l qw(input_acc_list subset_acc_list)
	{	$args{$l} = [ 1 .. scalar @$matrix ] if ! $args{$l};
	}

	die "Missing required argument! Dying" unless $matrix && @{$args{input_acc_list}} && @{$args{subset_acc_list}};

	foreach my $k (@{$args{subset_acc_list}})
	{	foreach my $i (@{$args{input_acc_list}})
		{	next if $k eq $i;
			foreach my $j (@{$args{subset_acc_list}})
			{	# not interested in self-self links
				next if $k eq $j || $i eq $j;
				## add to the matrix if we have either i->j or i->k->j
				$new_matrix->[$i][$j]++ if (defined $matrix->[$i][$j] || (defined $matrix->[$i][$k] && defined $matrix->[$k][$j]));
				$matrix->[$i][$j]++ if (defined $matrix->[$i][$j] || (defined $matrix->[$i][$k] && defined $matrix->[$k][$j]));
			}
		}
	}

	return $new_matrix;
}

=cut


=head2 _get_reduction_matrix

Reduce the matrix down if possible

input:  a closure matrix
output: a reduced matrix, we hope


sub _get_reduction_matrix {
	my $self = shift;
	my %args = (@_);
	my $matrix = $args{matrix};
	my $rels_combined = $args{rel_combination_h};
	my $rel_combos;
	my $acc = 1;

	print STDERR "rel combination h: " . Dumper($rels_combined) . "\n\n";

	my $all = scalar @$matrix - 1;

	my $new_matrix;
	for (my $k = 1; $k <= $all; $k++)
	{	for (my $i = 1; $i <= $all; $i++)
		{	next if $k eq $i;
			for (my $j = 1; $j <= $all; $j++)

			{	# not interested in self-self links
				next if $k eq $j || $i eq $j;
				if (defined $matrix->[$i][$j] && defined $matrix->[$i][$k] && defined $matrix->[$k][$j])
				{
					print STDERR "i->j: " . join(", ", sort keys %{$matrix->[$i][$j]}) . "\ni -> k: " . join(", ", sort keys %{$matrix->[$i][$k]}) . "; k->j: " . join(", ", sort keys %{$matrix->[$k][$j]}) . "\n";

					## find out what links we would generate over i->k->j
					## we can remove these links
					my $r1_combo = join(".", sort keys %{$matrix->[$i][$k]});
					my $r2_combo = join(".", sort keys %{$matrix->[$k][$j]});

					if ($rels_combined->{$r1_combo} && $rels_combined->{$r1_combo}{$r2_combo})
					{	## we've seen this combination before

						print STDERR "rels_combined: " . join(", ", keys %{$rels_combined->{$r1_combo}{$r2_combo}} ). "\n";

						delete $matrix->[$i][$j]{$_} foreach keys %{$rels_combined->{$r1_combo}{$r2_combo}};
					}

					else
					{
						my $temp_h = {};
						## this is a new combination...
						## for every link of relation $r1 between i and k
						foreach my $r1 (keys %{$matrix->[$i][$k]})
						{	next unless $rels_combined->{$r1};
							## for every link of relation $r2 between k and j
							foreach my $r2 (keys %{$matrix->[$k][$j]})
							{	## see if $r1 . $r2 results in a relation or not;
								## if it does, remove this relation from i -> j
								if (defined $rels_combined->{$r1}{$r2})
								{	map
									{ delete $matrix->[$i][$j]{$_};
										$temp_h->{$_}++;
									} keys %{$rels_combined->{$r1}{$r2}};
								}
							}
						}
						$rels_combined->{$r1_combo}{$r2_combo} = $temp_h;
					}

					if (keys %{$matrix->[$i][$j]})
					{	print STDERR "encoding i->j: " . ( join(", ", keys %{$matrix->[$i][$j]} ) ) . "\n";
						my $combo = join(".", sort keys %{$matrix->[$i][$j]});
						if ($rel_combos->{by_combo}{$combo})
						{	## seen this combo before
							$new_matrix->[$i][$j] = $rel_combos->{by_combo}{$combo};
						}
						else
						{	$rel_combos->{by_combo}{$combo} = $acc;
							map { $rel_combos->{by_acc}{$acc}{$_} = 1 } keys %{$matrix->[$i][$j]};
							$new_matrix->[$i][$j] = $acc;
							$acc++;
						}
					}
					else
					{	undef $matrix->[$i][$j];
					}
					print STDERR "i->j now: " . ( join(", ", keys %{$matrix->[$i][$j] || {} } ) ) . "\n";
				}
			}
		}
	}

	print STDERR "new matrix: " . Dumper($new_matrix) . "\n\n";

	return { matrix => $matrix, rel_combos => $rel_combos, new_matrix => $new_matrix };
}
=cut


=head2 _get_reduction_matrix_rel_independent

Reduce the matrix down if possible

input:  matrix
output: a reduced matrix


sub _get_reduction_matrix_rel_independent {
	my $self = shift;
	my %args = (@_);
	my $matrix = $args{matrix};
	die "Missing required arguments! Dying" unless $matrix && @{$args{input_acc_list}} && @{$args{subset_acc_list}};

	foreach my $l qw(input_acc_list subset_acc_list)
	{	$args{$l} = [ 1 .. scalar @$matrix ] if ! $args{$l};
	}

	my $acc = scalar @$matrix;
	foreach my $k (@{$args{subset_acc_list}})
	{	foreach my $i (@{$args{input_acc_list}})
		{	next if $k eq $i;
			foreach my $j (@{$args{subset_acc_list}})
			{	# not interested in self-self links
				next if $k eq $j || $i eq $j;
				## remove i->j from the matrix if we have i->k->j
				if (defined $matrix->[$i][$j] && defined $matrix->[$i][$k] && defined $matrix->[$k][$j])
				{	## remove $matrix->[$i][$j]
					undef $matrix->[$i][$j];
				}
			}
		}
	}

	return $matrix;
}

=cut




1;