package GOBO::InferenceEngine;
use Moose;
use Moose::Util::TypeConstraints;
use GOBO::Graph;
use Data::Dumper;
#use GOBO::Statement;
#use GOBO::Annotation;
#use GOBO::Node;
#use GOBO::TermNode;
#use GOBO::RelationNode;

use Storable;

has graph => (is=>'rw', isa=> 'GOBO::Graph');
has inferred_graph => (is=>'rw', isa=> 'GOBO::Graph', default=>sub{new GOBO::Graph});
has from_ix => (is=>'rw', isa=>'Str', default=>'statements');
has save_ix => (is=>'rw', isa=>'Str', default=>'saved_statements');
## has use_cache => (is=>'rw', isa=>'Bool', default=>sub{'1'});


### CACHED relationship data
## structure: subrelation_closure_h->{$rel->id} = [ $rel_a, $rel_b, $rel_c, ... ]
has subrelation_closure_h => (is=>'rw', isa=>'HashRef[ArrayRef[GOBO::RelationNode]]', default=>sub{{}});

## structure: subrelation_reflexive_closure_h->{$rel->id} = [ $rel_a, $rel_b, $rel_c, ... ]
has subrelation_reflexive_closure_h => (is=>'rw', isa=>'HashRef[ArrayRef[GOBO::RelationNode]]', default=>sub{{}});

## relation composition of $r1 and $r2
## structure: relation_composition_h->{ $r1->id }{ $r2->id } = [ $rel_a ]
has relation_composition_h => (is=>'rw', isa=>'HashRef[HashRef[ArrayRef[GOBO::RelationNode]]]', default=>sub{{}});

## combined_rel_h
## the relations that result from combining two statements
## A -- $r1 --> B  and B -- $r2 --> C
## key 1: $r1; may be multiple rel IDs, sorted and joined with "."
## key 2: $r2; may be multiple rel IDs, sorted and joined with "."
## value is an array containing all possible relations between A and C

has combined_rel_h => (is=>'rw', isa=>'HashRef[HashRef[ArrayRef[GOBO::RelationNode]]]');


=head2 generate_simple_combined_rel_h

Create a cached hash with all possible combinations of relations in the graph

=cut

sub generate_simple_combined_rel_h {
	my $self = shift;
	my $rels = $self->graph->relations;
	my $rel_combo_h = $self->combined_rel_h;

#	print STDERR "relations: " . join(", ", @$rels) . "\n";
#	print STDERR "rel_combo_h: ";
#	foreach my $rel_a (sort keys %$rel_combo_h)
#	{	foreach my $rel_b (sort keys %{$rel_combo_h->{$rel_a}})
#		{	print STDERR "$rel_a x $rel_b => " . join(", ", @{$rel_combo_h->{$rel_a}{$rel_b}}) . "\n";
#		}
#	}

	foreach my $rel_a (@$rels)
	{	foreach my $rel_b (@$rels)
		{	next if $rel_combo_h->{$rel_a->id} && $rel_combo_h->{$rel_a->id}{$rel_b->id};
			my $rel_h;
			foreach my $r1 (@{$self->get_subrelation_reflexive_closure($rel_a)} )
			{	my @temp = $self->relation_composition($r1, $rel_b);
				if (! @temp)
				{	$rel_combo_h->{$rel_a->id}{$rel_b->id} = [];
					next;
				}
				# R1 subrelation_of R2, x R1 y => x R2 y
				 map { map { $rel_h->{ $_->id } = $_ } @{$self->get_subrelation_reflexive_closure($_)} } @temp;
			}
			$rel_combo_h->{$rel_a->id}{$rel_b->id} = [ values %$rel_h ];
		}
	}

#	print STDERR "rel_combo_h NOW: ";
#	foreach my $rel_a (sort keys %$rel_combo_h)
#	{	foreach my $rel_b (sort keys %{$rel_combo_h->{$rel_a}})
#		{	print STDERR "$rel_a x $rel_b => " . join(", ", @{$rel_combo_h->{$rel_a}{$rel_b}}) . "\n";
#		}
#	}
	$self->combined_rel_h( $rel_combo_h );
	return $rel_combo_h;
}


=head2 backward_chain

Find the inferred ancestry of a term using the backward chaining reasoner

input:  node => GOBO::Node
output: arrayref of ancestral GOBO::Nodes

=cut

sub backward_chain {
	my $self = shift;
	my %args = (
		from_ix => $self->from_ix,  ## default
		save_ix => $self->save_ix,  ## default
		@_
	);
	my $g = $self->graph;
	warn "No node specified for backward_chain!" && return undef if ! $args{node};

	# initialize link set based on input node;
	# we will iteratively extend upwards
	my @links = @{$g->get_matching_statements(%args, ix=>$args{from_ix})};
	my %outlink_h = ();
#	my %link_closure_h = ();
	while (@links) {
		my $link = shift @links;
		next if $outlink_h{$link};
		$outlink_h{$link} = 1;
		my $extlinks = $self->extend_link(%args, link => $link);
		if (@$extlinks) {
			push(@links,@$extlinks);
			#push(@outlinks,@$extlinks);
#			map {$link_closure_h{$_}=1} @$extlinks;
		}
	}
	return [keys %outlink_h];
}

=head2 get_inferred_outgoing_edges (was get_inferred_outgoing_links)

given a graph node $node, get inferred and asserted edges where $node is the
subject (child) in the edge.

if relation is specified, also filters results on relation

backward-chaining

input: hash of the form
         node => GOBO::Node              ## get ontology links for this node
         OR node_id => $node_id
         relation => GOBO::RelationNode  ## filter by relation, optional
         or relation_id => $relation_id
         from_ix => name of the statement index to get statements from
          (defaults to 'ontology_links' if not set)
         save_ix => name of the statement index to save the new statements in
          (defaults to 'inferred_ontology_links' if not set)

output: arrayref of inferred statements

=cut

sub get_inferred_outgoing_edges {
	my $self = shift;
	my %args = (
		from_ix => $self->from_ix, ## default
		save_ix => $self->save_ix, ## default || 'inferred_outgoing_edges',
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
	my @links = @{$self->graph->get_matching_statements(node => $args{node}, ix=>$args{from_ix})};
#	print STDERR "node $args{node} links:\n" . join("\n", @links)."\n\n";

	my %outlink_h = ();
	while (@links) {
#		print STDERR "\nloop: \@links is\n" . join("\n", @links) . "\n";
		my $link = shift @links;
#		print STDERR "looking at $link... ";
#		print STDERR "seen it before!\n" &&
		next if $outlink_h{$link};
		$outlink_h{$link} = $link;
#		print STDERR "new to me!\n";

#		my $extlinks = $self->extend_link(%args, link => $link);
#		if (@$extlinks) {
#			push(@links,@$extlinks);
#		}

		foreach my $srel (@{$self->get_subrelation_closure($link->relation)}) {
			my $newlink = $self->create_link_statement(
				node=>$link->node, relation=>$srel, target=>$link->target);
#			print STDERR "adding $newlink to link array\n";
			$outlink_h{$newlink} = $newlink;
#			push(@links,$newlink);
		}

		my $more_links;
		$more_links = $self->graph->get_matching_statements(node=>$link->target, ix=>$args{save_ix}); # if $self->use_cache;
		if ($more_links && @$more_links)
		{	#print STDERR "more links:\n". join("\n", @$more_links) . "\n\n";
			## we had cached links. We don't need to look at any of the child links
			foreach my $xlink (@$more_links)
			{	my $combined = $self->_combine_statements($link, $xlink);
				if ($combined && @$combined)
				{	$outlink_h{$_} = $_ foreach @$combined;
				}
			}
			next;
		}
		foreach my $xlink (@{$self->graph->get_matching_statements(node=>$link->target, ix=>$args{from_ix})})
		{	my $combined = $self->_combine_statements($link, $xlink);
			if ($combined && @$combined)
			{	#print STDERR "new links from combine_statements:\n".join("\n", @$combined)."\n\n";
				foreach (@$combined)
				{	push @links, $_ unless $outlink_h{$_};
				}
			}
		}
	}

#	print STDERR "Finished loop!\n";
	return [] unless values %outlink_h;

	$self->graph->add_statements_to_ix( statements=>[values %outlink_h], ix => $args{save_ix} );
#	$ig->add_ontology_links(statements=>[values %outlink_h]);
	return [values %outlink_h];
}

*get_inferred_target_links = \&get_inferred_outgoing_edges;
*get_inferred_outgoing_statements = \&get_inferred_outgoing_edges;

sub get_inferred_outgoing_ontology_links {
	my $self = shift;
	return $self->get_inferred_outgoing_edges( @_, from_ix => 'ontology_links' );
}


=head2 get_inferred_outgoing_nodes (subject GOBO::Node, relation GOBO::RelationNode OPTIONAL)

given a subject (child), get inferred target (parent) nodes

if relation is specified, also filters results on relation

backward-chaining

=cut

sub get_inferred_outgoing_nodes {
	my $self = shift;
	my %tn = ();
	foreach my $link (@{ $self->get_inferred_outgoing_edges(@_) }) {
		$tn{$link->target->id} = $link->target;
	}
	return [values %tn];
}

*get_inferred_target_nodes = \&get_inferred_outgoing_nodes;
*get_inferred_target_statements = \&get_inferred_outgoing_nodes;


=head2 get_inferred_incoming_edges

Does the same as get_inferred_outgoing_edges, but in the other direction; i.e.
gets edges where the target (parent) node is $target

input: hash of the form
         target => GOBO::Node            ## get graph edges for this node
         OR target_id => $target_id
         relation => GOBO::RelationNode  ## filter by relation, optional
         or relation_id => $relation_id
         from_ix => name of the statement index to get statements from
          (defaults to 'ontology_links' if not set)
         save_ix => name of the statement index to save the new statements in
          (defaults to 'inferred_ontology_links' if not set)

output: arrayref of inferred statements

=cut

sub get_inferred_incoming_edges {
	my $self = shift;
	my %args = (
		from_ix => $self->from_ix,  ## default
		save_ix => $self->save_ix,  ## default || 'inferred_incoming_edges'
		@_
	);
	my $g = $self->graph;

	confess( (caller(0))[3]  . ": no node specified! " ) unless $args{target} || $args{target_id};
	if (! $args{target})
	{	$args{target} = $self->graph->get_node($args{target_id}) || new GOBO::Node(id=>$args{target_id})
	}

	#print STDERR "doing get_inferred_incoming_statements with " . $n->id . "!\n";

	my $tlinks = $g->get_matching_statements(target=>$args{target}, ix => $args{save_ix});
	if ($tlinks && @$tlinks) {
		# cached
		return $tlinks;
	}

	# initialize link set based on input node;
	# we will iteratively extend upwards
	my @links = @{$g->get_matching_statements(target=>$args{target}, ix=>$args{from_ix})};
#	printf STDERR "looking at $n => @links\n";

	my %inlink_h = ();

	while (@links) {
		my $link = shift @links;
		next if $inlink_h{$link};
		$inlink_h{$link} = $link;

		foreach my $srel (@{$self->get_subrelation_closure($link->relation)}) {
			my $newlink = new GOBO::LinkStatement(node=>$link->node,
												relation=>$srel,
												target=>$link->target);
			push(@links,$newlink);
		}

		my $more_links = $g->get_matching_statements(target=>$link->node, ix=>$args{save_ix});
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

		foreach my $xlink (@{$g->get_matching_statements(target => $link->node, ix => $args{from_ix})})
		{	my $combined = $self->_combine_statements($xlink, $link);
			if ($combined && @$combined)
			{	push(@links, @$combined);
			}
		}
	}

	if ($args{relation})
	{	## filter out relations of rel type $args{relation}
		for ( my ($k, $v) = each %inlink_h )
		{	delete $inlink_h{$k} if $v->relation->id ne $args{relation}->id;
		}
	}

	$g->add_statements(statements => [values %inlink_h], ix => $args{save_ix});

	return [values %inlink_h];
}


=head2 get_subrelation_closure

 input:  GOBO::RelationNode
 output: arrayref of GOBO::RelationNodes that are "is a" parents of the input

Retrieve the "is a" parents of a relation
Sets the subrelation reflexive closure when working out the subrelation closure
Results are cached by the inference engine

=cut

sub get_subrelation_closure {
	my $self = shift;
	my $rel = shift;

#	print STDERR "get_subrelation_closure rel: $rel\n";
#	return $self->get_inferred_outgoing_nodes(from_ix => 'statements', save_ix => 'statements', node => $rel, relation => 'is_a' );

## NEW!!
	my $cl_h = $self->subrelation_closure_h;
	## see if the result exists already
	if ($cl_h && $cl_h->{ $rel->id })
	{	return $cl_h->{ $rel->id };
	}
	else
	{	my $results = $self->get_inferred_outgoing_nodes(from_ix => 'statements', node => $rel, relation_id => 'is_a' );
		## save the subrelation closure
		$self->subrelation_closure_h({ %$cl_h, $rel->id => $results });
		## the subrelation reflexive closure is the same, but includes $rel
		$self->subrelation_reflexive_closure_h({ %{$self->subrelation_reflexive_closure_h}, $rel->id => [$rel, @$results] });
		return $results;
	}
}

=head2 get_subrelation_reflexive_closure

 input:  GOBO::RelationNode
 output: arrayref of GOBO::RelationNodes comprising the input node and the "is a"
         parents of the input

Retrieve the "is a" parents of a relation plus the relation itself
If the result is not available, calls $self->get_subrelation_closure($rel),
which automatically adds the reflexive subrelation closure.

=cut

sub get_subrelation_reflexive_closure {
	my $self = shift;
	my $rel = shift;

	## see if the result exists already
	my $r_cl_h = $self->subrelation_reflexive_closure_h;
	if ($r_cl_h && $r_cl_h->{ $rel->id })
	{	return $r_cl_h->{ $rel->id };
	}
	else
	{	$self->get_subrelation_closure($rel);
		return $self->subrelation_reflexive_closure_h->{$rel->id};
	}
}

=head2 generate_subrelation_closure_h
=head2 generate_subrelation_reflexive_closure_h

Work out all the subrelation closures and subrelation reflexive closures for
the graph.

=cut

sub generate_subrelation_closure_h {
	my $self = shift;
	foreach (@{$self->graph->relations})
	{	$self->get_subrelation_closure($_);
	}
	return $self->subrelation_closure_h;
}

sub generate_subrelation_reflexive_closure_h {
	my $self = shift;
	$self->generate_subrelation_closure_h;
	return $self->subrelation_reflexive_closure_h;
}


=head2 extend_link

Given a link A, find links such that the target of link A is the same as
the node of link B, i.e.

node 1 ---- A ---- node 2 ---- B ---- node 3

extend_link will return all the edges between node 1 and node 3


 input:  hash with link => $link
 output: arrayref of links

=cut

sub extend_link {
	my $self = shift;
	my %args = (
		from_ix => $self->from_ix, ## default
		@_
	);


	my @newlinks = ();
	foreach my $xlink (@{$self->graph->get_matching_statements(ix=>$args{from_ix}, node=>$args{link}->target)})
	{	my $combined = $self->_combine_statements($args{link}, $xlink);
		if ($combined)
		{	push @newlinks, @$combined;
		}

#		foreach my $rel_1 (@{$self->get_subrelation_reflexive_closure( $args{link}->relation )} )
#		{	#printf STDERR "  XLINK: $xlink\n";
#			my $rel_2 = $xlink->relation;
#			my @rels = $self->relation_composition($rel_1, $rel_2);
#			print STDERR "result: " . join("\n", @rels) . "\n\n";

			# R1 subrelation_of R2, x R1 y => x R2 y
#			@rels = map { @{$self->get_subrelation_reflexive_closure( $_ )} } @rels;
#			foreach my $rel (@rels) {
#				my $newlink = new GOBO::LinkStatement(node=>$args{link}->node,
#													  relation=>$rel,
#													  target=>$xlink->target);
#				# todo - provenance/evidence of link
#				push(@newlinks, $newlink);
#			}
#		}
	}
	return \@newlinks;
}



=cut

combining two links

link_a = X -- rel_a --> Y

link_b = Y -- rel_b --> Z

X -- rel_a --> Y -- rel_b --> Z

=>  X -- rel_x --> Z

=cut

sub _combine_statements {
	my $self = shift;
	my ($stt_a, $stt_b) = (@_);

#	print STDERR "Combine statements: $stt_a * $stt_b\n";

	my @rels;
	## see if we already have this combo or not
	if (defined $self->relation_composition_h->{$stt_a->relation->id} && defined $self->relation_composition_h->{$stt_a->relation->id}{$stt_b->relation->id})
	{	## we've seen this combo already
		@rels = @{$self->relation_composition_h->{$stt_a->relation->id}{$stt_b->relation->id}};
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

	return undef if ! @rels;

	my @statements;
	foreach my $rel (@rels)
	{	my $new_stt = $self->create_link_statement(node => $stt_a->node, relation => $rel, target => $stt_b->target);
		# todo - provenance/evidence of statement
		push(@statements, $new_stt);
	}
	return [@statements] if @statements;
	return undef;
}



=head2 get_nonredundant_set (nodes ArrayRef[GOBO::Node], OPTIONAL set2 ArrayRef[GOBO::Node])

TODO: allow specification of relations

returns all nodes n in set1 such that there is no n' in (set1 U set2)
such that no relationship nRn' can be inferred

=cut

sub get_nonredundant_set {
	my $self = shift;
	my $nodes = shift;
	my $set2 = shift || [];
	#print STDERR "Finding NR set for @$nodes\n";
	my %nh = map { ($_ => $_) } @$nodes;
	foreach my $node (@$nodes) {
		my $targets = $self->get_inferred_outgoing_nodes(node=>$node);
		foreach (@$targets) {
			delete $nh{$_->id};
		}
	}
	foreach my $node (@$set2) {
		my $targets = $self->get_inferred_outgoing_nodes(node=>$node);
		delete $nh{$node};
		foreach (@$targets) {
			delete $nh{$_->id};
		}
	}
	# TODO
	return [values %nh];
}

=head2 relation_composition

 Arguments: GOBO::RelationNode r1 GOBO::RelationNode r2
 Returns:   Array[GOBO::RelationNode]

Given two relations r1 and r2, returns the list of relations that hold
true between x and z where x r1 y and y r2 z holds

Formal definition:

  (R1 o R2 -> R3) implies ( x R1 y, y R2 z -> x R3 z)

Examples:

  part_of o part_of -> part_of (if part_of is declared transitive)
  regulates o part_of -> regulates (if regulates is declared transitive_over part_of)

See also:

http://geneontology.org/GO.ontology-ext.relations.shtml

http://wiki.geneontology.org/index.php/Relation_composition

=cut

sub relation_composition {
	my $self = shift;
	my $r1 = shift;
	my $r2 = shift;

#	print STDERR "relation composition: r1: $r1; r2: $r2\n";

	my $rel_comp_h = $self->relation_composition_h;
	if ($rel_comp_h && $rel_comp_h->{$r1->id} && $rel_comp_h->{$r1->id}{$r2->id})
	{	return @{$rel_comp_h->{$r1->id}{$r2->id}};
	}

	if ($r1->equals($r2) && $r1->transitive) {
		$rel_comp_h->{$r1->id}{$r2->id} = [ $r1 ];
#		print STDERR "$r1 * $r2 => $r1\n" if $ENV{VERBOSE};
		return ($r1);
	}
	if ($r1->is_subsumption && $r2->propagates_over_is_a) {
		$rel_comp_h->{$r1->id}{$r2->id} = [ $r2 ];
#		print STDERR "$r1 * $r2 => $r2\n" if $ENV{VERBOSE};
		return ($r2);
	}
	if ($r2->is_subsumption && $r1->propagates_over_is_a) {
		$rel_comp_h->{$r1->id}{$r2->id} = [ $r1 ];
#		print STDERR "$r1 * $r2 => $r1\n" if $ENV{VERBOSE};
		return ($r1);
	}
	if ($r1->transitive_over && $r1->transitive_over->equals($r2)) {
		$rel_comp_h->{$r1->id}{$r2->id} = [ $r1 ];
#		print STDERR "$r1 * $r2 => $r1\n" if $ENV{VERBOSE};
		return ($r1);
	}

#	print STDERR "could not work out what to do with $r1 and $r2!\n";
	## see if there are any chains involving this relation...
	my $chains = [];
	foreach (@{$self->graph->relations})
	{	if ($_->holds_over_chain)
		{	my $c_list = $_->holds_over_chain_list;
			foreach my $l (@$c_list)
			{	if (scalar @$l == 2 && $l->[0]->equals($r1) && $l->[1]->equals($r2))
				{	push @$chains, $_;
				}
			}
		}
	}


#	print STDERR "could not work out what to do with $r1 and $r2!\n";
	# TODO: arbitrary chains
	$rel_comp_h->{$r1->id}{$r2->id} = $chains;
	return @$chains;
}


=head2 generate_relation_composition_h

Create a cached relation composition hash with all possible relations in the graph

=cut

sub generate_relation_composition_h {
	my $self = shift;
	my $rels = $self->graph->relations;
	my $hash;
	foreach my $a (@$rels)
	{	foreach my $b (@$rels)
		{	$self->relation_composition($a, $b);
		}
	}
	return $self->relation_composition_h;
}

sub print_relation_composition {
	my $self = shift;
	my $r1 = shift;
	my $r2 = shift;
	my $r_composition_h = $self->generate_relation_composition_h;
	if ($r1)
	{	if ($r2)
		{	print STDERR "$r1 . $r2 => " . join(", ", map { $_->id } @{$r_composition_h->{$r1}{$r2}}) . "\n";
		}
		else
		{
			foreach my $r2 (keys %{$r_composition_h->{$r1}})
			{	print STDERR "$r1 . $r2 => " . join(", ", map { $_->id } @{$r_composition_h->{$r1}{$r2}}) . "\n";
			}
		}
	}
	else
	{
		foreach my $r1 (keys %$r_composition_h)
		{	foreach my $r2 (keys %{$r_composition_h->{$r1}})
			{	print STDERR "$r1 . $r2 => " . join(", ", map { $_->id } @{$r_composition_h->{$r1}{$r2}}) . "\n";
			}
		}
	}
	print STDERR "\n";
}


sub forward_chain {
	my $self = shift;
	my $g = $self->graph;
	my $ig = new GOBO::Graph;
	$ig->copy_from($g);
#	$self->inferred_graph($ig);
	$self->calculate_deductive_closure;

}



sub calculate_deductive_closure {
	my $self = shift;
	my $g = $self->graph;

	my $saturated = 0;
	while (!$saturated) {

	}
}

=head2 subsumed_by

c1 subsumed_by c2 if any only if every member of c1 is a member of c2

The following rules are used in the decision procedure:

=head3 Relation Composition

=head3 Intersections

See GOBO::ClassExpression::Intersection

if c2 = a ∩ b AND c1 is subsumed by a AND c1 is subsumed by b THEN c1 is subsumed by c2

=head3 Unions

See GOBO::ClassExpression::Union

if c2 = a ∪ b AND (c1 is subsumed by a OR c1 is subsumed by b) THEN c1 is subsumed by c2

=head3 Relational Expressions

See GOBO::ClassExpression::RelationalExpression

if c2 = <r y> AND c1 r y THEN c1 is subsumed by c2

=cut

sub subsumed_by {
	my $self = shift;
	my $child = shift; # GOBO::Class
	my $parent = shift; # GOBO::Class
	my $subsumes = 0;

	# reflexivity of subsumption relation
	if ($child->equals($parent)) {
		return 1;
	}

#	print STDERR "input: child: " . ref($child) . " $child; parent: " . ref($parent) . " $parent\n";

	# check is_a closure
	if (grep {$_->id eq $parent->id} @{$self->get_inferred_outgoing_nodes(ix=>'statements', node => $child, relation_id => 'is_a')}) {
#		print STDERR "Parent ID is in inferred outgoing nodes\n";
		return 1;
	}

	# parent is not itself a class expression, but may have an equivalence relation to a class expression
#	if ($parent->isa('GOBO::TermNode')) {
	if ($self->graph->get_node($parent) && $self->graph->get_node($parent)->isa('GOBO::TermNode')) {
		# TODO: equiv test for roles?
		if ( $parent->logical_definition) {
#			print STDERR "parent has a logical definition\n";
			return $self->subsumed_by($child,$parent->logical_definition);
		}
		if ( $parent->union_definition) {
#			print STDERR "parent has a union definition\n";
			return $self->subsumed_by($child,$parent->union_definition);
		}
	}

	### CHECK THIS!! ###
	# class subsumption over boolean expressions
	if ($parent->isa('GOBO::ClassExpression')) {
		if ($parent->isa('GOBO::ClassExpression::RelationalExpression')) {
			if (grep {$_->id eq $parent->target} @{$self->get_inferred_outgoing_edges(ix=>'statements', node => $child, relation => $parent->relation)}) {
				return 1;
			}
		}
		elsif ($parent->isa('GOBO::ClassExpression::BooleanExpression')) {
			my $args = $parent->arguments;
			if ($parent->isa('GOBO::ClassExpression::Intersection')) {
				$subsumes = 1;
				foreach my $arg (@$args) {
					if (!$self->subsumed_by($child, $arg)) {
						$subsumes = 0;
						last;
					}
				}
			}
			elsif ($parent->isa('GOBO::ClassExpression::Union')) {
				foreach my $arg (@$args) {
					if ($self->subsumed_by($child, $arg)) {
						$subsumes = 1;
						last;
					}
				}
			}
			else {
				$self->throw("cannot infer with $parent");
			}
		}
		else {

		}
	}
	return $subsumes;
}

# TODO
#sub disjoint_from_violations {
#	 my $self = shift;
#
#}


=head2 create_link_statement

Create a link object, given a set of arguments

 input:  node => GOBO::Node
         relation => GOBO::Node
         target => GOBO::X (object of some sort)
         type => 'GOBO::Annotation' / 'GOBO::LinkStatement' / 'GOBO::Statement' (etc.)
 output: a link object of the appropriate type

if type is not specified, the following rules will be followed:

- if no target is given, a Statement object will be created
- if the target is a GOBO::Gene, an Annotation object will be created
- otherwise, a LinkStatement will be created

=cut

sub create_link_statement {
	my $self = shift;
	my %args = (@_);

	foreach my $x qw(node relation target)
	{	if ($args{$x."_id"} && ! $args{$x})
		{	$args{$x} = $self->graph->get_node( $args{$x."_id"} );
		}
	}

#	print STDERR "node ref: " . ref($args{node}) . "\n";

	if (! $args{type})
	{	if (! $args{target})
		{	$args{type} = 'GOBO::Statement';
		}
		elsif ($args{node}->isa('GOBO::Gene'))
		{	$args{type} = 'GOBO::Annotation';
		}
		else
		{	$args{type} = 'GOBO::LinkStatement';
		}
	}
	my $link = $args{type}->new( %args );

	return $link;
}


has 'edge_matrix' => ( is=>'rw', isa=>'HashRef[HashRef[HashRef[HashRef[Str]]]]', traits=>['Hash'], default=>sub{{}}, handles => {
'get_edge_matrix' => 'get', 'set_edge_matrix' => 'set', 'delete_edge_matrix' => 'delete', 'clear_edge_matrix' => 'clear', 'edge_matrix_keys' => 'keys', 'exists_edge_matrix' => 'exists', 'defined_edge_matrix' => 'defined', 'edge_matrix_values' => 'values', 'edge_matrix_kv' => 'kv', 'edge_matrix_elements' => 'elements', 'count_edge_matrix' => 'count', 'edge_matrix_is_empty' => 'is_empty' },
 );


=head2 __create_edge_matrix

Create a matrix containing all the links, asserted and implied, between the
terms in $input and those in $subset

 input:  hash in the form
         input_ids   => arrayref of input node_ids; optional; uses all graph nodes
                        if not specified
         subset_ids  => arrayref of subset nodes; optional; uses all graph nodes
                        if not specified
         from_ix     => index of asserted links
         save_ix     => index which will contain asserted and inferred links
         options     => hash of options, including options->{verbose}

 output: populates $self->edge_matrix with a hash in the following form
              {N_R_T}{ node_id }{ relation_id }{ target_id } = 1

=cut

sub __create_edge_matrix {
	my $self = shift;
	my %args = (@_);

	my $graph = $self->graph;
	my $options = $args{options} || {};

	confess( (caller(0))[3] . ": missing required arguments" ) unless $args{input_ids} && scalar @{$args{input_ids}} > 0;

#	print STDERR "arguments: " . Dumper(\%args) . "\n" if $ENV{VERBOSE};

	# get rid of any existing data
	my $edges;

#print STDERR "subset: " . join(", ", sort @{$args{subset_ids}}) . "\n";
#print STDERR "input: " . join(", ", sort @{$args{input_ids}}) . "\n";
#print STDERR "links: " . join("\n", @{$self->graph->get_all_statements_in_ix(ix=>$self->from_ix)})."\n";

	if ($args{subset_ids} && @{$args{subset_ids}})
	{	# get all the links between the input nodes and those in the subset
		foreach my $id (@{$args{input_ids}})
		{	#print STDERR "Looking at $id\n";# edges:\n" . join("\n", @{$self->get_inferred_outgoing_edges(node_id=>$id)}) . "\n";
			foreach my $l (@{ $self->get_inferred_outgoing_edges(%args, node_id=>$id) })
			{
				# skip it unless the target is a root or in the subset
				next if ! grep { $l->target->id eq $_ } @{$args{subset_ids}};
				## add to a list of inferred entries
				$edges->{N_R_T}{$id}{$l->relation->id}{$l->target->id}++;
#				push @{$edges->{N_R_T}{$id}{$_->relation->id}{$_->target->id}}, $l;
			}
		}
	}
	else
	{	# get all the links involving the input nodes
		foreach my $id (@{$args{input_ids}})
		{	foreach (@{ $self->get_inferred_outgoing_edges(%args, node_id=>$id) })
			{	## add to a list of inferred entries
				$edges->{N_R_T}{$id}{$_->relation->id}{$_->target->id}++;
#				push @{$edges->{N_R_T}{$id}{$_->relation->id}{$_->target->id}}, $_;
			}
		}
	}

#	print STDERR "edges: ";
#	$self->dump_edge_matrix(key => 'N_R_T', matrix => $edges);
#	print STDERR "\n\n";

	$self->edge_matrix($edges);
}


=head2 __generate_matrix_from_statements

 input:  statements => [ Stt, Stt, ... ]  ## arrayref of statements
         matrix     => $name              ## name of the new matrix

 output: $self->edge_matrix($name) populated

=cut

sub __generate_matrix_from_statements {
	my $self = shift;
	my %args = (
		order => [ 'node', 'relation', 'target' ],
		@_
	);

	$self->delete_edge_matrix($args{matrix});

	confess( (caller(0))[3] . ": missing required arguments" ) unless $args{statements} && scalar @{$args{statements}} > 0;

	my $matrix;
	foreach my $e (@{$args{statements}})
	{	my @keys;
		foreach (@{$args{order}})
		{	if ($e->can($_) && defined $e->$_->id)
			{	push @keys, $e->$_->id;
			}
			else
			{	push @keys, "";
			}
		}
		$matrix->{ $keys[0] }{ $keys[1] }{ $keys[2] }++;
	}

	$self->set_edge_matrix( $args{matrix} => $matrix );
}

=head2 __populate_all_edge_matrices

 input:  $self with some edges in $self->edge_matrix in the form
             {N_R_T}{ node_id }{ relation_id }{ target_id } = Int
 output: rearrangements of the data with first key specifying the order:
         $self->edge_matrix populated with the following hashes:
             {N_T_R}
             {T_N_R}
             {N_R_T}
             {T_R_N}
             {R_N_T}
             {R_T_N}

=cut

sub __populate_all_edge_matrices {
	my $self = shift;

	confess( (caller(0))[3] . ": no edges found in edge matrix!" ) unless $self->defined_edge_matrix('N_R_T');

	$self->delete_edge_matrix( qw(N_T_R T_N_R T_R_N R_N_T R_T_N) );

	my $matrix = $self->edge_matrix;
	foreach my $n (keys %{$matrix->{N_R_T}})
	{	foreach my $r (keys %{$matrix->{N_R_T}{$n}})
		{	foreach my $t (keys %{$matrix->{N_R_T}{$n}{$r}})
			{	$matrix->{N_T_R}{$n}{$t}{$r} = #1;
				$matrix->{T_N_R}{$t}{$n}{$r} = #1;
				$matrix->{T_R_N}{$t}{$r}{$n} = #1;
				$matrix->{R_N_T}{$r}{$n}{$t} = #1;
				$matrix->{R_T_N}{$r}{$t}{$n} = #1;
				$matrix->{N_R_T}{$n}{$r}{$t};
			}
		}
	}
	$self->edge_matrix( $matrix );
}


=head2 __trim_edge_matrix

For each term, trims away any redundant edges

 input:  a populated edge matrix in $self->edge_matrix->{N_T_R}
         trim_relations => 1  ## optional; remove redundant relationships
 output: new slimmed down matrix stored in $self->edge_matrix->{N_T_R}, with
         edges specified as
              { node_id }{ relation_id }{ target_id }

=cut

sub __trim_edge_matrix {
	my $self = shift;
	my %args = (@_) if @_;

	$self->__populate_all_edge_matrices unless $self->defined_edge_matrix('T_N_R');

	## delete the matrices we're not going to use
	$self->delete_edge_matrix( qw(N_R_T T_R_N R_N_T R_T_N) );

	my $trimmed; ## for storing the trimmed matrix
	my $rel_combo_h = $self->generate_simple_combined_rel_h; ## relation combos

=cut Algorithm:

foreach k in edge->targets
	foreach i in edges with target k
		foreach j in edges with node k
			IF exists edge(s) with node i and target j
			## we are looking at i -> j and i -> k -> j
				delete i -> j if i -> k -> j results in the same relationship

=cut

#	$self->dump_edge_matrix(key => 'N_R_T');

	my $matrix = $self->edge_matrix;
	foreach my $k (keys %{$matrix->{T_N_R}})
	{	## needs to have incoming terms too
		next unless $matrix->{N_T_R}{$k};
		## looking for i->k
		foreach my $i (keys %{$matrix->{T_N_R}{$k}})
		{	## looking for k->j and i->j
			foreach my $j (keys %{$matrix->{N_T_R}{$k}})
			{	next if $j eq $i;
				next if ! $matrix->{N_T_R}{$i} && ! $matrix->{N_T_R}{$i}{$j};

	## we have the rels between $i and $j in keys %{$matrix->{N_T_R}{$i}{$j}
	## remove all i->j relations where i->k->j == i->j
				my $r1_combo = join(".", sort keys %{$matrix->{N_T_R}{$i}{$k}});
				my $r2_combo = join(".", sort keys %{$matrix->{N_T_R}{$k}{$j}});

				if ($r1_combo eq "" || $r2_combo eq "")
				{	warn "Shouldn't be proceeding here!\n$i - $r1_combo -> $k\n" . Dumper($matrix->{N_T_R}{$i}{$k}) . "$k - $r2_combo -> $j\n" . Dumper($matrix->{N_T_R}{$k}{$j}) . "\n\n";

				}

				## check if we've seen this combination before
				if (! $rel_combo_h->{$r1_combo} || ! $rel_combo_h->{$r1_combo}{$r2_combo})
				{	my $temp_h;
					## for every link of relation $r1 between i and k
					foreach my $r1 (keys %{$matrix->{N_T_R}{$i}{$k}})
					{	## for every link of relation $r2 between k and j
						foreach my $r2 (keys %{$matrix->{N_T_R}{$k}{$j}})
						{	## see if $r1 . $r2 results in a relation or not; if it does,
							## if it does, this relation can be removed from i -> j
							map { $temp_h->{$_}++ } @{$rel_combo_h->{$r1}{$r2}};
						}
					}
					$rel_combo_h->{$r1_combo}{$r2_combo} = [ keys %$temp_h ];
				}

				## delete these relationships...
				foreach (@{$rel_combo_h->{$r1_combo}{$r2_combo}})
				{	delete $matrix->{N_T_R}{$i}{$j}{$_};
					delete $matrix->{T_N_R}{$j}{$i}{$_};
				}

				## clean up
				if (! keys %{$matrix->{N_T_R}{$i}{$j}})
				{	delete $matrix->{N_T_R}{$i}{$j};
					if (! keys %{$matrix->{N_T_R}{$i}})
					{	delete $matrix->{N_T_R}{$i};
					}
				}
				if (! keys %{$matrix->{T_N_R}{$j}{$i}})
				{	delete $matrix->{T_N_R}{$j}{$i};
					if (! keys %{$matrix->{T_N_R}{$j}})
					{	delete $matrix->{T_N_R}{$j};
					}
				}
			}
		}
	}

	foreach my $n (keys %{$matrix->{N_T_R}})
	{	foreach my $t (keys %{$matrix->{N_T_R}{$n}})
		{	foreach my $r (keys %{$matrix->{N_T_R}{$n}{$t}})
			{	$trimmed->{$n}{$r}{$t} = $matrix->{N_T_R}{$n}{$t}{$r};
			}
		}
	}

	$self->edge_matrix( { N_R_T => $trimmed, trimmed => $trimmed } );
	return unless %args && $args{trim_relations};
	## we also want to trim away any redundant relations
	$self->__populate_all_edge_matrices;
	$self->__remove_redundant_relationships;

=cut old method
	# for each node with a link to a 'target' (closer to root) node
	foreach my $id (keys %{$matrix->{N_T_R}})
	{	# only connected to one node: must be the closest!
		if (scalar keys %{$matrix->{N_T_R}{$id}} == 1)
		{	$trimmed->{$id} = $matrix->{N_R_T}{$id};
			next;
		}
		foreach my $rel (keys %{$matrix->{N_R_T}{$id}})
		{	# only one node connected by $rel
			if (scalar keys %{$matrix->{N_R_T}{$id}{$rel}} == 1)
			{	$trimmed->{$id}{$rel} = $matrix->{N_R_T}{$id}{$rel};
				next;
			}

			#	list_by_rel contains all the nodes between it and the root(s) of $id
			my @list_by_rel = keys %{$matrix->{N_R_T}{$id}{$rel}};

			REL_SLIMDOWN_LOOP:
			while (@list_by_rel)
			{	my $a = pop @list_by_rel;
				my @list2_by_rel = ();
				while (@list_by_rel)
				{	my $b = pop @list_by_rel;
					if ($matrix->{T_N_R}{$a}{$b})
					{	#	b is node, a is target
						#	forget about a, go on to the next list item
						push @list_by_rel, $b;
						push @list_by_rel, @list2_by_rel if @list2_by_rel;
						next REL_SLIMDOWN_LOOP;
					}
					elsif ($matrix->{N_T_R}{$a}{$b})
					{	#	a is node, b is target
						#	forget about b, look at the next in the list
						next;
					}
					else
					{	#	a and b aren't related
						#	keep b
						push @list2_by_rel, $b;
						next;
					}
				}
				#	if a is still around, it must be a descendent of
				#	all the nodes we've looked at, so it can go on our
				#	descendent list
				$trimmed->{$id}{$rel}{$a} = $matrix->{N_R_T}{$id}{$rel}{$a};

				#	if we have a list2_by_rel, transfer it back to @list_by_rel
				push @list_by_rel, @list2_by_rel if @list2_by_rel;
			}
		}
	}
=cut


}


=head2 __convert_matrix_to_edges

 input:  matrix  => $matrix_key    ## the key for a populated edge matrix
         save_ix => $save_ix       ## save edges to this statement_ix
         from    => $graph         ## graph to use as the source for nodes
         to      => $graph         ## graph to add links to

 output: edges in $matrix added as links to $save_ix

Converts all the edges in $matrix into LinkStatements
- if save_ix is not specified, adds the edges to the default indexes
- if from and/or to are not specified, uses $self->graph

=cut

sub __convert_matrix_to_edges {
	my $self = shift;
	my %args = (
		save_ix => 'statements',
		from => $self->graph,
		to => $self->graph,
		@_
	);
	if (! defined $args{from} || ! defined $args{to})
	{	confess "from or to graph not defined!";
	}
	if (! $args{matrix} || ! $self->defined_edge_matrix($args{matrix}))
	{	warn "Edge matrix not specified or no values defined";
		return;
	}
	
	my $matrix = $self->get_edge_matrix($args{matrix});
	
	$self->dump_edge_matrix(matrix => $matrix);
	
	my $to_add;
	my $h;
	## add any nodes required and create link statements
	foreach my $n (keys %$matrix)
	{	if (! $args{to}->get_node($n))
		{	$args{to}->add_node($args{from}->get_node($n));
		}
		
		foreach my $r (keys %{$matrix->{$n}})
		{	if (! $args{to}->get_node($r))
			{	$args{to}->add_relation($args{from}->get_node($r));
			}
			foreach my $t (keys %{$matrix->{$n}{$r}})
			{	if (! $args{to}->get_node($t))
				{	$args{to}->add_node($args{from}->get_node($t));
				}
				my $l = $self->create_link_statement( node => $args{to}->get_node($n), relation => $args{to}->relation_noderef($r), target => $args{to}->get_node($t) );
				push @$to_add, $l;
			}
		}
	}

	$args{to}->add_statements_to_ix( statements => $to_add, ix => $args{save_ix});

}

=head2 __remove_redundant_relationships

 input:  a populated edge matrix
 output: edge_matrix->{N_R_T} with redundant rels carefully removed

if we have relationships between relations -- e.g. positively_regulates is_a
regulates -- and two (or more) related relations are found between the same
two nodes, the less specific relationships are removed.

e.g.

A positively_regulates B
A regulates B

==> A regulates B will be removed

=cut

sub __remove_redundant_relationships {
	my $self = shift;
	my %args = (@_);

	confess( (caller(0))[3] . ": missing edge matrix! Dying" ) unless defined $self->edge_matrix && $self->defined_edge_matrix('N_R_T');

	$self->__populate_all_edge_matrices unless $self->defined_edge_matrix('R_N_T');

	# make sure we have the relation relationships
	my $cl_h = $self->generate_subrelation_closure_h;

	my $matrix = $self->edge_matrix;
	## start with the relations that have the largest number of 'is_a' parents
	foreach my $r (sort { scalar @{$cl_h->{$b}} cmp scalar @{$cl_h->{$a}} } keys %$cl_h)
	{	next if scalar @{$cl_h->{$r}} == 0;
		## see if we have this relation in the graph...
		next if ! $matrix->{R_N_T}{$r};
		## find these relations in the graph
		foreach my $n (keys %{$matrix->{R_N_T}{$r}})
		{	foreach my $t (keys %{$matrix->{R_N_T}{$r}{$n}})
			{	## delete this combo if it exists for any of the relations in @$red
				foreach (@{$cl_h->{$r}})
				{	if ($matrix->{R_N_T}{$_->id}
					&& $matrix->{R_N_T}{$_->id}{$n}
					&& $matrix->{R_N_T}{$_->id}{$n}{$t})
					{	delete $matrix->{N_R_T}{$n}{$_->id}{$t};

						if (! keys %{$matrix->{N_R_T}{$n}{$_->id}})
						{	delete $matrix->{N_R_T}{$n}{$_->id};
							if (! keys %{$matrix->{N_R_T}{$n}})
							{	delete $matrix->{N_R_T}{$n};
							}
						}
					}
				}
			}
		}
	}

	$self->set_edge_matrix( N_R_T => $matrix->{N_R_T} );

	# remove the (now out of date) matrices
	$self->delete_edge_matrix( qw(N_T_R R_N_T R_T_N T_N_R T_R_N) );

}



=head2 dump_edge_matrix

Prettier printing of an edge matrix

input:  $key     ## the ID of the matrix to get
         OR
        $matrix  ## a matrix to print
output: matrix printed out on STDERR

=cut

sub dump_edge_matrix {
	my $self = shift;
	my %args = (@_);

	my $matrix;
	if (! $args{matrix} || ! $args{key})
	{	warn "Please supply a key or a matrix for printing!" && return;
	}
	$matrix = $args{matrix} || $self->edge_matrix;
	if ($args{key})
	{	warn "key $args{key} could not be found in the matrix!" if ! $matrix->{$args{key}} && return;
		$matrix = $matrix->{$args{key}};
	}

	warn "No edge matrix found!" && return if ! $matrix;

	if ($args{key})
	{	print STDERR "Matrix $args{key}\n";
	}
	else
	{	print STDERR "Matrix\n";
	}
	foreach my $n (sort keys %$matrix)
	{	foreach my $r (sort keys %{$matrix->{$n}})
		{	foreach my $t (sort keys %{$matrix->{$n}{$r}})
			{	print STDERR "  $n $r $t\n";
			}
		}
	}
}


=head1 NAME

GOBO::InferenceEngine

=head1 SYNOPSIS

NOT FULLY IMPLEMENTED

=head1 DESCRIPTION

An GOBO::Graph is a collection of GOBO::Statements. These statements can
be either 'asserted' or 'inferred'. Inferred statements are created by
an Inference Engine. An InferenceEngine object provides two accessors,
'graph' for the source graph and 'inferred_graph' for the set of
statements derived from the source graph after applying rules that
take into account properties of the relations in the statements.

The notion of transitive closure in a graph can be expressed in terms
of the deductive closure of a graph of links in which each
GOBO::RelationNode has the property 'transitive'. The notion of
ancestry in a graph can be thought of as the inferred links where the
relations are transitive.

=head2 Rules

Rules are horn rules with sets of statements in the antecedent and
typically a single statement in the consequent.

In the notation below, subject and target nodes are indicated with
lower case variables (x, y, z, ...) and relations (GOBO::RelationNode)
are indicated with upper case (R, R1, R2, ...). Each statement is
written in this order:

  $s->node $->relation $s->target

=head3 Transitivity

  x R y, y R z => x R z (where $R->transitive)

=head3 Propagation over and under is_a

  x R y, y is_a z => x R z (where $R->propagates_over_is_a)
  x is_a y, y R z => x R z (where $R->propagates_over_is_a)

=head3 Link composition

  x R1 y, y R2 z => x R z (where R=R1.R2)

The above two rules are degenerate cases of this one.

The notion R = R1.R2 is used to specify these compositions. See $r->holds_over_chain_list

=head3 Reflexivity

 x ? ? => x R x (where $R->reflexive)

ie where x exists, x stands in relation R to itself

=head3 Symmetry

 x R y => y R x (where $R->symmetric)

Note that the type level adjacenct_to relation is not transitive

=head3 Inverses

 x R1 y => y R2 x (where $R1->inverse_of_list contains $R2 or vice versa)

Note that the type level part_of relation is not the inverse of has_part

=head3 Inference over GOBO::ClassExpressions

class expressions define sets of entities. We can infer the existing
of subset/subsumption relationships between these sets.

TODO

=head2 Inference strategies

=head3 Backward chaining

Starting for a given node, find all inferred Statements related to
that node by applying rules. Ancestors statements: all statements in
which this node plays the role of subject. Descendants statements: all
statements in which this node plays the role of target.

Can be combined with memoization, in which case subsequent queries can
retrieve cached results.

=head3 Forward chaining

Starting with a graph of asserted statements, iteratively keep
applying rules and adding resulting statements to the inferred graph,
until no new statements are added.

=head1 STATUS

PRE-ALPHA!!!

=cut

1;
