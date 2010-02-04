=head1 NAME

GOBO::Graph

=head1 SYNOPSIS

=head1 DESCRIPTION

A collection of inter-related GOBO::Node objects. With a simple
ontology these are typically GOBO::TermNode objects, although other
graphs e.g. instance graphs are possible.

This module deliberately omits any kind of graph traversal
functionality. This is done by an GOBO::InferenceEngine.

=head2 DETAILS

A GOBO::Graph consists of two collections: a node collection and a
link collection. Both types of collection are handled behind the
scenes using indexes (in future these can be transparently mapped to
databases).

A graph keeps a reference of all nodes declared or referenced. We draw
a distinction here: a graph can reference a node that is not declared
in that graph. For example, consider an obo file with two stanzas:

 id: x
 is_a: y

 id: y
 is_a: z

Here there are only two nodes declared (x and y) but there are a total
of three references.

The noderef method can be used to access the full list of nodes that
are either declared or referenced. This is useful to avoid
instantiating multiple copies of the same object.

Methods such as terms, relations and instances return only those nodes
declared to be in the graph

=head1 SEE ALSO

GOBO::Node

GOBO::LinkStatement

=cut

package GOBO::Graph;
use Moose;
with 'GOBO::Attributed';
use strict;
use GOBO::Annotation;
use GOBO::ClassExpression::Union;
use GOBO::ClassExpression;
use GOBO::Formula;
use GOBO::Indexes::NodeIndex;
use GOBO::Indexes::StatementIndexHelper;
use GOBO::Indexes::StatementRefIndex;
use GOBO::Indexes::StatementObjectIndex;
use GOBO::InstanceNode;
use GOBO::LinkStatement;
use GOBO::LiteralStatement;
use GOBO::Node;
use GOBO::RelationNode;
use GOBO::Statement;
use GOBO::Subset;
use GOBO::Synonym;
use GOBO::TermNode;

use Data::Dumper;

use overload ('""' => 'as_string');

has 'relation_h' => (is => 'rw', isa => 'HashRef[GOBO::RelationNode]', default=>sub{{}});
has 'term_h' => (is => 'rw', isa => 'HashRef[GOBO::TermNode]', default=>sub{{}});
has 'instance_h' => (is => 'rw', isa => 'HashRef[GOBO::InstanceNode]', default=>sub{{}});
#has 'node_index' => (is => 'rw', isa => 'HashRef[GOBO::Node]', default=>sub{{}});
has 'node_index' => (is => 'ro', isa => 'GOBO::Indexes::NodeIndex',
				default=>sub{ new GOBO::Indexes::NodeIndex() },
				handles => {
					'nodes' => 'nodes',
					'update_node_label_index' => 'update_label_index',
				} );
has 'subset_index' => (is => 'rw', isa => 'HashRef[GOBO::Subset]', default=>sub{{}});
has 'formulae' => (is => 'rw', isa => 'ArrayRef[GOBO::Formula]', default=>sub{[]});

has 'statement_ix_h' => (
	is=>'rw',
	isa=>'GOBO::Indexes::StatementIndexHelper',
	handles => #{
	[ qw(
		get_statement_ix
		add_statement_ix
		remove_statement_ix
		exists_statement_ix
		duplicate_statement_ix

		get_statement_ix_h
		set_statement_ix_h
		clear_statement_ix_h

		get_statement_ix_h_names
		get_statement_ix_h_values

		get_statement_ix_by_name

		get_matching_statements

		statements
		edges
		ontology_links
		annotations
		get_all_statements_in_ix

		add_statements
		add_edges
		add_ontology_links
		add_annotations
		add_statements_to_ix

		remove_statements
		remove_edges
		remove_ontology_links
		remove_annotations
		remove_statements_from_ix

		add_statement
		add_edge
		add_ontology_link
		add_annotation
		add_statement_to_ix

		remove_statement
		remove_edge
		remove_ontology_link
		remove_annotation
		remove_statement_from_ix

		clear_all_statements
		clear_all_edges
		clear_all_ontology_links
		clear_all_annotations
		clear_all_statements_in_ix

		statements_by_node_id
		statements_by_target_id
		statement_node_index
		statement_target_index

		edges_by_node_id
		edges_by_target_id
		edge_node_index
		edge_target_index

		ontology_links_by_node_id
		ontology_links_by_target_id
		ontology_link_node_index
		ontology_link_target_index

		annotations_by_node_id
		annotations_by_target_id
		annotation_node_index
		annotation_target_index

		statements_in_ix_by_node_id
		statements_in_ix_by_target_id
		statement_ix_node_index
		statement_ix_target_index

		get_outgoing_statements
		get_outgoing_edges
		get_outgoing_ontology_links
		get_outgoing_annotations
		get_outgoing_statements_in_ix

		get_incoming_statements
		get_incoming_edges
		get_incoming_ontology_links
		get_incoming_annotations
		get_incoming_statements_in_ix

		remove_statements_from_ix_by_id

		)],
	default=>sub{ new GOBO::Indexes::StatementIndexHelper(); },
);


## Add an 'is_a' node to the graph by default
sub BUILD {
	my $self = shift;
	my $r = new GOBO::RelationNode(id => 'is_a', label => 'is a');
	$self->add_relation( $r );
}


sub referenced_nodes {
	my $self = shift;
	return $self->node_index->nodes;
}


## after adding  links / annotations, need to update the graph
sub update_graph {
	my $self = shift;
	my @statements = (@_);

	if (! @statements)
	{	@statements = @{$self->statements};
	}

	foreach my $s (@statements)
	{	## check we have the nodes in the Graph
		foreach ('node', 'relation', 'target')
		{	next unless $s->can($_);
			my $n = $s->$_;
			# skip if there's no such node or we already have the node
			next unless $n && ! $self->get_node($n);

			if ($n->isa("GOBO::TermNode"))
			{	$self->add_term($n) if ! $self->get_term($n);
			}
			elsif ($n->isa("GOBO::RelationNode"))
			{	$self->add_relation($n) if ! $self->get_relation($n);
			}
			elsif ($n->isa("GOBO::InstanceNode"))
			{	$self->add_instance($n) if ! $self->get_instance($n);
			}
			elsif ($n->isa("GOBO::Node"))
			{	$self->add_node($n) if ! $self->get_node($n);
			}
			else
			{	warn "Don't understand " . ref($n) . "; adding as plain node";
				$self->noderef($n);
			}
		}
	}
}



sub has_terms {
	my $self = shift;
	return 1 if scalar @{$self->terms};
	return undef;
}

sub has_relations {
	my $self = shift;
	return 1 if scalar @{$self->relations};
	return undef;
}

sub has_instances {
	my $self = shift;
	return 1 if scalar @{$self->instances};
	return undef;
}

sub has_subsets {
	my $self = shift;
	return 1 if scalar @{$self->declared_subsets};
	return undef;
}

*has_declared_subsets = \&has_subsets;

sub has_formulae {
	my $self = shift;
	return 1 if scalar @{$self->formulae};
	return undef;
}

sub has_statements {
	my $self = shift;
	return 1 if scalar @{$self->statements};
	return undef;
}

sub has_edges {
	my $self = shift;
	return 1 if scalar @{$self->edges};
	return undef;
}

sub has_ontology_links {
	my $self = shift;
	return 1 if scalar @{$self->ontology_links};
	return undef;
}

sub has_annotations {
	my $self = shift;
	return 1 if scalar @{$self->annotations};
	return undef;
}

sub has_nodes {
	my $self = shift;
	return 1 if scalar @{$self->nodes};
	return undef;
}



=head2 declared_subsets

 - returns ArrayRef[GOBO::Subset]

returns the subsets declared in this graph.

See also: GOBO::TermNode->subsets() - this returns the subsets a term belongs to

=cut

# @Override
sub declared_subsets {
	my $self = shift;
	if (@_) {
		my $ssl = shift;
		$self->subset_index->{$_->id} = $_ foreach @$ssl;
	}
	return [values %{$self->subset_index()}];
}


=head2 get_term

 input:  id Str
 output: GOBO::TermNode, if term is declared in this graph

=cut

sub get_term {
	my $self = shift;
	my $id = shift;
	return $self->term_h->{$id};
}

=head2 get_relation

 input:  id Str
 output: GOBO::RelationNode, if relation is declared in this graph

=cut

sub get_relation {
	my $self = shift;
	my $id = shift;
	return $self->relation_h->{$id};
}

=head2 get_instance

 input:  id Str
 output: GOBO::InstanceNode, if instance is declared in this graph

=cut

sub get_instance {
	my $self = shift;
	my $id = shift;
	return $self->instance_h->{$id};
}


=head2 terms

 output: ArrayRef[GOBO::TermNode], where each member is a term belonging to this graph

=cut

sub terms {
	my $self = shift;
	#$self->node_index->nodes_by_metaclass('term');
	return [values %{$self->term_h}];
}

=head2 relations

 output: ArrayRef[GOBO::RelationNode], where each member is a relation belonging to this graph

=cut

sub relations {
	my $self = shift;
	#$self->node_index->nodes_by_metaclass('relation');
	return [values %{$self->relation_h}];
}

=head2 instances

 output: ArrayRef[GOBO::InstanceNode], where each member is an instance belonging to this graph

=cut

sub instances {
	my $self = shift;
	#$self->node_index->nodes_by_metaclass('instance');
	return [values %{$self->instance_h}];
}

=head2 add_term

 - Arguments: Str or GOBO::Node
 output: GOBO::TermNode
 - Side effects: adds the object to the list of terms referenced in this graph. Forces the class to be GOBO::TermNode

=cut

sub add_term {
	my $self = shift;
	my $n = $self->term_noderef(@_);
	$self->term_h->{$n->id} = $n;
	return $n;
}

=head2 add_relation

 - Arguments: Str or GOBO::Node
 output: GOBO::RelationNode
 - Side effects: adds the object to the list of relations referenced in this graph. Forces the class to be GOBO::RelationNode

=cut

sub add_relation {
	my $self = shift;
	my $n = $self->relation_noderef(@_);
	$self->relation_h->{$n->id} = $n;
	return $n;
}

=head2 add_instance

 - Arguments: Str or GOBO::Node
 output: GOBO::InstanceNode

adds the object to the list of instances referenced in this
graph. Forces the class to be GOBO::InstanceNode

=cut

sub add_instance {
	my $self = shift;
	my $n = $self->instance_noderef(@_);
	$self->instance_h->{$n->id} = $n;
	return $n;
}

=head2 remove_node

 - Arguments: node GOBO::Node, cascade Bool[OPT]

unlinks the node from this graph

If cascade is 0 or undef, any statements involving this node will remain as dangling links.

If cascade is set, then statements involving this node will also be deleted

=cut

sub remove_node {
	my $self = shift;
	my $n = shift;
	my $cascade = shift;

	if (! ref $n)
	{	$n = $self->get_node($n);
	}
	return unless $n;
	my $id = $n->id;

	if ($self->term_h->{$id}) {
		delete $self->term_h->{$id};
	}
	if ($self->instance_h->{$id}) {
		delete $self->instance_h->{$id};
	}
	if ($self->relation_h->{$id}) {
		delete $self->relation_h->{$id};
	}
	if ($cascade) {
#		$self->remove_link($_) foreach @{$self->get_outgoing_links($n)};
#		$self->remove_link($_) foreach @{$self->get_incoming_links($n)};
		foreach my $x qw( node relation target )
		{	#print STDERR "Running remove statements for $x = $id\n";
			my @stt = @{$self->get_matching_statements($x=>$n)};
			$self->remove_statements( @stt ) if @stt;
		}
	}

	return $self->node_index->remove_node($n);
}


sub add_formula { my $self = shift; push(@{$self->formulae},@_) }


# given a node ID or a node object, returns the corresponding
# node in the graph. If no such node exists, one will be created.
sub noderef {
	my $self = shift;
	my $id = shift; # Str or GOBO::Node
	my $ix = $self->node_index;

	my $n_obj;
	if (ref($id)) {
		# $id is actually a GOBO::Node
		$n_obj = $id;
		$id = $id->id;
	}
	else {
		if ($id =~ /\s/) {
			confess("attempted to noderef '$id' -- no whitespace allowed in ID.");
		}
	}

	if ($ix->node_by_id($id)) {	  # already in the index
		$n_obj = $ix->node_by_id($id);
	}
	else {
		if (! $n_obj) {
			$n_obj = new GOBO::Node(id=>$id);
		}
		$ix->add_node( $n_obj );
	}
	return $n_obj;
}

## add a node to the graph, checking what type it is first

sub add_node {
	my $self = shift;
	my $id = shift; # Str or GOBO::Node
	my $ix = $self->node_index;

	my $n_obj;
	if (ref($id)) {
		if ($id->isa('GOBO::TermNode'))
		{	return $self->add_term($id);
		}
		elsif ($id->isa('GOBO::RelationNode'))
		{	return $self->add_relation($id);
		}
		elsif ($id->isa('GOBO::InstanceNode'))
		{	return $self->add_instance($id);
		}
	}
	return $self->noderef($id);
}


# given a node ID or a node object, returns the corresponding
# node in the graph. Returns undef if none exists
sub get_node {
	my $self = shift;
	my $id = shift; # Str or GOBO::Node
	confess("no ID supplied!") if ! $id;
	my $ix = $self->node_index;
	if (ref($id)) {
		# $id is actually a GOBO::Node
		$id = $id->id;
	}
	else {
		if ($id =~ /\s/) {
			confess("attempted to noderef '$id' -- no whitespace allowed in ID.");
		}
	}

	if ($ix->node_by_id($id)) {	  # already in the index
		return $ix->node_by_id($id);
	}
	else {
		return undef;
	}
}



# given a node ID or a node object, returns the corresponding
# node in the graph. If no such node exists, one will be created.
# Forces the resulting object to be a TermNode.
sub term_noderef {
	my $self = shift;
	my $n = $self->noderef(@_);
	if (!$n->isa('GOBO::TermNode')) {
		bless $n, 'GOBO::TermNode';
	}
	return $n;
}

# given a node ID or a node object, returns the corresponding
# node in the graph. If no such node exists, one will be created.
# Forces the resulting object to be a RelationNode.
sub relation_noderef {
	my $self = shift;
	my $n = $self->noderef(@_);
	if (!$n->isa('GOBO::RelationNode')) {
		bless $n, 'GOBO::RelationNode';
	}
	return $n;
}

# given a node ID or a node object, returns the corresponding
# node in the graph. If no such node exists, one will be created.
# Forces the resulting object to be an InstanceNode.
sub instance_noderef {
	my $self = shift;
	my $n = $self->noderef(@_);
	if (!$n->isa('GOBO::InstanceNode')) {
		bless $n, 'GOBO::InstanceNode';
	}
	return $n;
}

# given a node ID or a node object, returns the corresponding
# node in the graph. If no such node exists, one will be created.
# Forces the resulting object to be a Subset.
sub subset_noderef {
	my $self = shift;
	my $ssid = shift;
	my $n = $self->subset_index->{$ssid};
	if (!$n) {
		# TODO: fail?
#		 warn "creating subset $ssid";
		$n = new GOBO::Subset(id=>$ssid);
		$self->subset_index->{$ssid} = $n;
	}
	if (!$n->isa('GOBO::Subset')) {
		bless $n, 'GOBO::Subset';
	}
	return $n;
}

sub parse_idexprs {
	my $self = shift;
	my @nodes = @{$self->node_index->nodes};
	my %done = ();
	while (my $n = shift @nodes) {
		next if $done{$n->id};
		if ($n->id =~ /\^/) {
			my $ce = new GOBO::ClassExpression->parse_idexpr($self,$n->id);
			#printf STDERR "$n => $ce\n";
			if (!$n->can('logical_definition')) {
				bless $n, 'GOBO::Term';
			}
			$n->logical_definition($ce);
			foreach my $arg (@{$ce->arguments}) {
				push(@nodes,$n);
				printf STDERR "n=$n\n";
			}
		}
		$done{$n->id} = 1;
	}
}

# logical definitions can be directly attached to TermNodes, or they can be
# present in the graph as intersection links
# TBD : move to utility class?
use GOBO::ClassExpression::RelationalExpression;
use GOBO::ClassExpression::Intersection;
use GOBO::ClassExpression::Union;
sub convert_intersection_links_to_logical_definitions {
	my $self = shift;
	my @xplinks = ();
	my @nlinks = ();
	my %xpnodeh = ();
	foreach (@{$self->ontology_links}) {
		if($_->is_intersection) {
			push(@xplinks, $_);
			push(@{$xpnodeh{$_->node->id}}, $_);
		}
		else {
			push(@nlinks, $_);
		}
	}
	if (@xplinks) {
		$self->clear_all_ontology_links;
		$self->add_ontology_links(\@nlinks);
		foreach my $nid (keys %xpnodeh) {
			my $n = $self->noderef($nid);
			my @exprs =
				map {
					if ($_->relation->is_subsumption) {
						$_->target;
					}
					else {
						new GOBO::ClassExpression::RelationalExpression(relation=>$_->relation, target=>$_->target);
					}
			} @{$xpnodeh{$nid}};
			if (@exprs < 2) {
				$self->throw("invalid intersection links for $nid. Need at least 2, you have @exprs");
			}
			$n->logical_definition(new GOBO::ClassExpression::Intersection(arguments=>\@exprs));
		}
	}
	return;
}

sub as_string {
	my $self = shift;
	return
		join('',
			 (map { "$_\n" } @{$self->ontology_links}),
			 (map { "$_\n" } @{$self->annotations}),
		);
}


=head1 Methods involving statement indexes

Statements are kept in StatementIndex objects, of which a graph may have several.
These indexes are stored in a hash, $graph->statement_ix_h, with the index name
as the key and the StatementIndex object as the value.

By default, graphs have four indexes:

 - statements      ## all statements in the graph
 - edges           ## all LinkStatements with a node, relation, and target
 - ontology_links  ## LinkStatements between TermNodes in a graph
 - annotations     ## Annotation objects


=head2 get_statement_ix

Get a statement index from the graph

 input:  $name_of_statement_ix
 output: the index


=head2 get_statement_ix_by_name

Get a statement index, creating it if necessary

 input:  $index_name, create_if_does_not_exist (OPTIONAL)
 output: the appropriate statement index
         OR undef if the index does not exist and 'create_if_does_not_exist' is undef/0

e.g. $self->get_statement_ix_by_name('selected_statements', 1)
would get all the statements in the index 'selected_statements', and if the
index did not exist, it would be created.

=head2 add_statement_ix

Add a statement index to the graph

 input:  hash in the form
         $new_index_name => GOBO::Indexes::StatementIndex object
 output: the StatementIndex


=head2 remove_statement_ix

Remove a statement index from the graph

 input:  $index_name
 output: none


=head2 exists_statement_ix

Returns true if $index_name exists in statement_ix_h

 input:  $index_name
 output: 1 or undef


=head2 set_statement_ix_h

Set the statement_ix_h using a hash of $index_name => Index object pairs

 input:  hash in the form
         $index_name => Index, $index_name_2 => Index_2, ...
 output: none

=head2 clear_statement_ix_h

Remove everything from the entire statement_ix_h


=head2 get_statement_ix_h

Get the statement_ix_h as an array
NB: returns an array, not an array ref!

 input:  self
 output: $index_name, Index, $index_name_2, Index_2, ...


=head2 get_statement_ix_h_names

Retrieve the names of the indexes in statement_ix_h
NB: returns an array, not an array ref!

 input:  self
 output: array of index names


=head2 get_statement_ix_h_values

Retrieve the indexes themselves from statement_ix_h
NB: returns an array, not an array ref!

 input:  self
 output: array of indexes


=cut



=head2 sync_statement_indexes

Go through the statements in the statement indexes and make sure they're
categorised correctly and that the nodes are those in the graph's node index.

 input:  self
 output: self, but with the indexes beautifully synchronised!

=cut

sub sync_statement_indexes {
	my $self = shift;
	print STDERR "Starting sync_statement_indexes\n";
	my $all = $self->get_statement_ix_by_name('statements')->get_all_statements;
	my $add_h;
	foreach my $s (@$all)
	{	foreach my $c qw(node relation target)
		{	if ($s->can($c) && $s->$c->id)
			{	my $c_obj = $self->get_node($s->$c->id);
				$s->$c( $c_obj ) if defined $c_obj;
			}
		}
		map { push @{$add_h->{$_}}, $s } @{$self->statement_ix_h->_get_statement_type( $s )};
	}
	if ($add_h)
	{	map {
			$self->statement_ix_h->clear_all_statements_in_ix($_);
			$self->add_statements_to_ix( ix => $_, statements => $add_h->{$_} )
		} keys %$add_h;
	}
}



=head2 get_outgoing_edges

given a subject (node / child), get the target (parent) from a LinkStatement

 input:  $node     ## GOBO::Node
         $relation ## GOBO::RelationNode (optional)
 output: arrayref of Statements where the node is $node

if relation is specified, also filters results on relation

=head2 get_outgoing_ontology_links

Shortcut for get_outgoing_edges

=head2 get_outgoing_annotations

Retrieve annotations by the gene product in the annotation statement.

 input:  $gene_product
 output: arrayref of Annotations where the node is $gene_product

Also aliased as 'get_annotations_by_subject' and 'get_annotations_by_gene_product'


=head2 get_incoming_edges

given a subject (node / child), get the target (parent) from a LinkStatement

 input:  $node     ## GOBO::Node
         $relation ## GOBO::RelationNode (optional)
 output: arrayref of Statements where the node is $node

if relation is specified, also filters results on relation

=head2 get_outgoing_ontology_links

Shortcut for get_outgoing_edges

=head2 get_outgoing_annotations

Shortcut for get_outgoing_edges

=cut



*get_edges_by_node = \&get_outgoing_edges;
*get_target_edges = \&get_outgoing_edges; # @Deprecated
*get_ontology_links_by_node = \&get_outgoing_ontology_links;
*get_target_ontology_links = \&get_outgoing_ontology_links; # @Deprecated
*get_annotations_by_subject = \&get_outgoing_annotations;
*get_annotations_by_gene_product = \&get_outgoing_annotations;
*get_edges_by_target = \&get_incoming_edges;
*get_ontology_links_by_target = \&get_incoming_ontology_links;
*get_annotations_by_target = \&get_incoming_annotations;
*get_annotations_by_term = \&get_incoming_annotations;


=head2 get_annotated_terms

Retrieve all terms that are attached to an annotation

 input:   
 output:  ArrayRef[GOBO::TermNode]

=head2 get_annotated_terms_in_ix

Retrieve all terms in index $ix_name that have an annotation attached

 input:   $ix_name  ## index to use
 output:  ArrayRef[GOBO::TermNode]

=cut

sub get_annotated_terms {
	my $self = shift;
	my @term_ids = @{$self->get_statement_ix_by_name('annotations', 1)->statement_target_index};
	return [ map { $self->get_term($_) } @term_ids ];
}

sub get_annotated_terms_in_ix {
	my $self = shift;
	my $ix = shift;
	return [] if ! $self->exists_statement_ix($ix);
	my %term_h = ();
	foreach my $t (@{$self->get_statement_ix_by_name($ix)->statement_target_index})
	{	foreach (@{$self->statements_in_ix_by_target_id($ix, $t)})
		{	if ($_->isa('GOBO::Annotation'))
			{	$term_h{ $_->target->id } = $_->target;
				last;
			}
		}
	}
	return [ values %term_h ];
}


=head2 get_orphan_terms

 input:   optional argument to update the graph before proceeding
 output:  ArrayRef[GOBO::TermNode]

 returns terms that aren't part of any ontology link statements

=head2 get_orphan_terms_in_ix

 input:   $ix_name   ## index to look for terms in
          optional argument to update the graph before proceeding
 output:  ArrayRef[GOBO::TermNode]

 returns terms that aren't part of any statements in $ix_name

=cut

sub get_orphan_terms {
	my $self = shift;
	return $self->get_orphan_terms_in_ix('ontology_links', @_);
}

sub get_orphan_terms_in_ix {
	my $self = shift;
	my $ix = shift;
	my $include_update = shift;

	$self->update_graph if $include_update;

	# no orphans if there are no terms in the graph!
	return [] if ! $self->has_terms;

	## return all terms if the statement index does not exist
	warn "statement index $ix does not exist!" && return $self->terms unless $self->exists_statement_ix($ix);

	my $node_h;
	map { $node_h->{$_}++ } (@{$self->statement_ix_node_index($ix)}, @{$self->statement_ix_target_index($ix)});
	return [ grep { ! $node_h->{ $_->id } && ! $_->obsolete } @{$self->terms} ];
}


=head2 get_is_a_roots

 input:  none
 output: ArrayRef[GOBO::TermNode]

returns terms that lack an is_a parent

=cut

sub get_is_a_roots {
	my $self = shift;
	return $self->get_roots('is_a');
}

=head2 get_roots

 input:  relation Str or OBO::RelationNode [OPTIONAL]
 output: ArrayRef[GOBO::TermNode]

returns terms that lack a parent by the given relation. If no relation
specified, then returns terms that lack a parent by any relation

Note: this only looks at terms with no outgoing (parent) links. There is no
check for any incoming links.

=cut

sub get_roots {
	my $self = shift;
#	my $rel = shift;
	my @roots = ();
	foreach my $term (@{$self->terms || []}) {
		next if $term->obsolete;
		if (!@{$self->get_outgoing_ontology_links($term, @_)}) {
			push(@roots,$term);
		}
	}
	return \@roots;
}

=head2 get_connected_roots

 input:  [none]
 output: ArrayRef[GOBO::TermNode]

Find nodes connected to the graph which have no outgoing ontology links (i.e.
they are 'parent' terms but have no 'parents' themselves)

=head2 get_connected_roots_in_ix

 input:  $ix_name  ## the name of the statement index in which to look for roots
 output: ArrayRef[GOBO::TermNode]

Find nodes in a certain index that are connected to the graph but have no
outgoing links (i.e. they are 'parent' terms but have no 'parents' themselves)

=cut

sub get_connected_roots {
	my $self = shift;
	return $self->get_connected_roots_in_ix('ontology_links');
}


sub get_connected_roots_in_ix {
	my $self = shift;
	my $ix = shift;
	my $node_h;

	return [] unless $self->exists_statement_ix($ix);

	map { $node_h->{$_}++ } @{ $self->statement_ix_target_index($ix) };
	foreach (@{$self->statement_ix_node_index($ix)})
	{	delete $node_h->{$_};
	}

	foreach (keys %$node_h)
	{	my $t = $self->get_term($_);
		if ($t->obsolete)
		{	delete $node_h->{$_} && next;
		}
		$node_h->{$_} = $t;
	}

	return [ values %$node_h ];
}


=head2 get_leaves

Find nodes connected to the graph which have no incoming ontology links

 input:  [none]
 output: ArrayRef[GOBO::TermNode]
 
=head2 get_leaves_in_ix

Find nodes in a statement index that are connected to the graph but have no
incoming links

 input:  $ix_name  ## the name of the statement index in which to look for roots
 output: ArrayRef[GOBO::TermNode]
 
=cut

sub get_leaves {
	my $self = shift;
	return $self->get_leaves_in_ix('ontology_links');
}


sub get_leaves_in_ix {
	my $self = shift;
	my $ix = shift;
	return [] unless $self->exists_statement_ix($ix);
	my $node_h;
	map { $node_h->{$_}++ } @{ $self->statement_ix_node_index($ix) };
	foreach (@{$self->statement_ix_target_index($ix)})
	{	delete $node_h->{$_};
	}
	foreach (keys %$node_h)
	{	$node_h->{$_} = $self->get_term($_);
	}
	return [ grep { defined $_ } values %$node_h ];
}



1;

