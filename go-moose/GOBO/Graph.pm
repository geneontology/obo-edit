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
use GOBO::Indexes::StatementIndex;
use GOBO::InstanceNode;
use GOBO::LinkStatement;
use GOBO::LiteralStatement;
#use GOBO::Node;
use GOBO::RelationNode;
use GOBO::Statement;
use GOBO::Subset;
use GOBO::Synonym;
use GOBO::TermNode;

use Data::Dumper;

use overload ('""' => 'as_string');

has 'relation_h' => (is => 'rw', isa => 'HashRef[GOBO::TermNode]', default=>sub{{}});
has 'term_h' => (is => 'rw', isa => 'HashRef[GOBO::TermNode]', default=>sub{{}});
has 'instance_h' => (is => 'rw', isa => 'HashRef[GOBO::InstanceNode]', default=>sub{{}});
has 'link_ix' => (is => 'rw', isa => 'GOBO::Indexes::StatementIndex',
				default=>sub{ new GOBO::Indexes::StatementIndex() },
				handles => { links => 'statements', add_link => 'add_statements', add_links => 'add_statements', remove_links => 'remove_statements', remove_link => 'remove_statements', remove_all_links => 'clear_all', link_node_index => 'statement_node_index', link_target_index => 'statement_target_index' } );
has 'annotation_ix' => (is => 'rw', isa => 'GOBO::Indexes::StatementIndex',
				default=>sub{ new GOBO::Indexes::StatementIndex() },
				handles => { annotations => 'statements', add_annotations => 'add_statements', remove_annotations => 'remove_statements', add_annotation => 'add_statements', remove_annotation => 'remove_statements',  remove_all_annotations => 'clear_all', annotated_entities => 'referenced_nodes', } );
#has 'node_index' => (is => 'rw', isa => 'HashRef[GOBO::Node]', default=>sub{{}});
has 'node_index' => (is => 'rw', isa => 'GOBO::Indexes::NodeIndex',
				default=>sub{ new GOBO::Indexes::NodeIndex() },
				handles => [ 'nodes' ] );
has 'subset_index' => (is => 'rw', isa => 'HashRef[GOBO::Subset]', default=>sub{{}});
has 'formulae' => (is => 'rw', isa => 'ArrayRef[GOBO::Formula]', default=>sub{[]});


around 'links' => sub {
	my $method = shift;
	my $self = shift;
	my $results = $self->$method;
	return [ sort { $a->node->id cmp $b->node->id || $a->target->id cmp $b->target->id } @$results ];
};

#sub nodes {
#	 my $self = shift;
#	 return $self->node_index->nodes;
#}

sub referenced_nodes {
	my $self = shift;
	return $self->node_index->nodes;
}


## after adding  links / annotations, need to update the graph
sub update_graph {
	my $self = shift;
	my @added = (@_);

	if (! @added)
	{	@added = (@{$self->links}, @{$self->annotations});
	}

	foreach my $s (@added)
	{	## check we have the nodes in the Graph
		foreach ('node', 'relation', 'target')
		{	my $n = $s->$_;
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

sub has_links {
	my $self = shift;
	return 1 if scalar @{$self->links};
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

 - Argument: id Str
 - Returns: GOBO::TermNode, if term is declared in this graph

=cut

sub get_term {
	my $self = shift;
	my $id = shift;
	return $self->term_h->{$id};
}

=head2 get_relation

 - Argument: id Str
 - Returns: GOBO::RelationNode, if relation is declared in this graph

=cut

sub get_relation {
	my $self = shift;
	my $id = shift;
	return $self->relation_h->{$id};
}

=head2 get_instance

 - Argument: id Str
 - Returns: GOBO::InstanceNode, if instance is declared in this graph

=cut

sub get_instance {
	my $self = shift;
	my $id = shift;
	return $self->instance_h->{$id};
}


=head2 terms

 - Returns: ArrayRef[GOBO::TermNode], where each member is a term belonging to this graph

=cut

sub terms {
	my $self = shift;
	#$self->node_index->nodes_by_metaclass('term');
	return [values %{$self->term_h}];
}

=head2 relations

 - Returns: ArrayRef[GOBO::RelationNode], where each member is a relation belonging to this graph

=cut

sub relations {
	my $self = shift;
	#$self->node_index->nodes_by_metaclass('relation');
	return [values %{$self->relation_h}];
}

=head2 instances

 - Returns: ArrayRef[GOBO::InstanceNode], where each member is an instance belonging to this graph

=cut

sub instances {
	my $self = shift;
	#$self->node_index->nodes_by_metaclass('instance');
	return [values %{$self->instance_h}];
}

=head2 add_term

 - Arguments: Str or GOBO::Node
 - Returns: GOBO::TermNode
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
 - Returns: GOBO::RelationNode
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
 - Returns: GOBO::InstanceNode

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

If cascade is 0 or undef, any links to or from this node will remain as dangling links.

If cascade is set, then links to and from this node will also be deleted

=cut

sub remove_node {
	my $self = shift;
	my $n = shift;
	my $cascade = shift;

	my $id = ref($n) ? $n->id : $n;

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
		$self->remove_link($_) foreach @{$self->get_outgoing_links($n)};
		$self->remove_link($_) foreach @{$self->get_incoming_links($n)};
	}

	return $self->node_index->remove_node($n);
}


sub add_formula { my $self = shift; push(@{$self->formulae},@_) }


=head2 get_outgoing_links (subject GOBO::Node, relation GOBO::RelationNode OPTIONAL)

given a subject (child), get target (parent) links

if relation is specified, also filters results on relation

=cut

sub get_outgoing_links {
	my $self = shift;
	my $n = shift;
	my $rel = shift;
	my @sl = @{$self->link_ix->statements_by_node_id(ref($n) ? $n->id : $n) || []};
	return [] if ! @sl;
	# if x = a AND r(b), then x r b
	if (ref($n) && $n->isa('GOBO::ClassExpression::Intersection')) {
		foreach (@{$n->arguments}) {
			if ($_->isa('GOBO::ClassExpression::RelationalExpression')) {
				push(@sl, new GOBO::LinkStatement(node=>$n,relation=>$_->relation,target=>$_->target));
			}
			else {
				push(@sl, new GOBO::LinkStatement(node=>$n,relation=>'is_a',target=>$_));
			}
		}
	}
	if ($rel) {
		# TODO: use indexes to make this faster
		my $rid = ref($rel) ? $rel->id : $rel;
		@sl = grep {$_->relation->id eq $rid} @sl;
	}
	return \@sl;
}

*get_links_by_node = \&get_outgoing_links;

# @Deprecated
*get_target_links = \&get_outgoing_links;


=head2 get_incoming_links (subject GOBO::Node, relation GOBO::RelationNode OPTIONAL)

given a target (parent), get subject (child) links

if relation is specified, also filters results on relation

=cut

sub get_incoming_links {
	my $self = shift;
	my $n = shift;
	my $rel = shift;
	my @sl = @{$self->link_ix->statements_by_target_id(ref($n) ? $n->id : $n) || []};
	return [] if ! @sl;
	if ($rel) {
		# TODO: use indexes to make this faster
		my $rid = ref($rel) ? $rel->id : $rel;
		@sl = grep {$_->relation->id eq $rid} @sl;
	}
	return \@sl;
}

*get_links_by_target = \&get_incoming_links;


=head2 get_annotations_by_subject (subject GOBO::Node, relation GOBO::RelationNode OPTIONAL)

given a subject (gene product), get target (term) links

if relation is specified, also filters results on relation

=cut

sub get_annotations_by_subject {
	my $self = shift;
	my $n = shift;
	my $rel = shift;
	my @sl = @{$self->annotation_ix->statements_by_node_id(ref($n) ? $n->id : $n) || []};
	# if x = a AND r(b), then x r b
	if (ref($n) && $n->isa('GOBO::ClassExpression::Intersection')) {
		foreach (@{$n->arguments}) {
			if ($_->isa('GOBO::ClassExpression::RelationalExpression')) {
				push(@sl, new GOBO::LinkStatement(node=>$n,relation=>$_->relation,target=>$_->target));
			}
			else {
				push(@sl, new GOBO::LinkStatement(node=>$n,relation=>'is_a',target=>$_));
			}
		}
	}
	if ($rel) {
		# TODO: use indexes to make this faster
		my $rid = ref($rel) ? $rel->id : $rel;
		@sl = grep {$_->relation->id eq $rid} @sl;
	}
	return \@sl;
}

=head2 get_annotations_by_target (target GOBO::Node, relation GOBO::RelationNode OPTIONAL)

given a target (term), get annotated subjects (gene products)

if relation is specified, also filters results on relation

=cut

sub get_annotations_by_target {
	my $self = shift;
	my $n = shift;
	my $rel = shift;
	my @sl = @{$self->annotation_ix->statements_by_target_id(ref($n) ? $n->id : $n) || []};
	if ($rel) {
		# TODO: use indexes to make this faster
		my $rid = ref($rel) ? $rel->id : $rel;
		@sl = grep {$_->relation->id eq $rid} @sl;
	}
	return \@sl;
}
*get_annotations_by_term = \&get_annotations_by_target;


=head2 get_annotated_terms

Retrieve all terms that are attached to an annotation

=cut

sub get_annotated_terms {
	my $self = shift;
	my @term_ids = @{$self->annotation_ix->statement_target_index};
	return [ map { $self->get_term($_) } @term_ids ];
}


=head2 get_orphan_terms

 - Argument: optional argument to update the graph before proceeding
 - Returns:  ArrayRef[GOBO::TermNode]

 returns terms that aren't part of any link statements

=cut

sub get_orphan_terms {
	my $self = shift;
	my $include_update = shift;

	$self->update_graph if $include_update;

	# no orphans if there are no terms in the graph!
	return [] if ! $self->has_terms;

	# if no links, return all the terms
	return $self->terms if ! $self->has_links;

#	my @terms = map { $_->id } grep { $_->isa('GOBO::TermNode') } @{$self->nodes};
	my $referenced;
	map { $referenced->{$_}++ } (@{$self->link_node_index}, @{$self->link_target_index});
	return [ grep { ! $referenced->{ $_->id } } @{$self->terms} ];
}


=head2 get_is_a_roots

 - Argument: none
 - Returns: ArrayRef[GOBO::TermNode]

returns terms that lack an is_a parent

=cut

sub get_is_a_roots {
	my $self = shift;
	return $self->get_roots('is_a');
}

=head2 get_roots

 - Argument: relation Str or OBO::RelationNode [OPTIONAL]
 - Returns: ArrayRef[GOBO::TermNode]

returns terms that lack a parent by the given relation. If no relation
specified, then returns terms that lack a parent by any relation

=cut

sub get_roots {
	my $self = shift;
#	 my $rel = shift;
	my @roots = ();
	foreach my $term (@{$self->terms || []}) {
		next if $term->obsolete;
		if (!@{$self->get_outgoing_links($term, @_)}) {
			push(@roots,$term);
		}
	}
	return \@roots;
}



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
	foreach (@{$self->links}) {
		if($_->is_intersection) {
			push(@xplinks, $_);
			push(@{$xpnodeh{$_->node->id}}, $_);
		}
		else {
			push(@nlinks, $_);
		}
	}
	if (@xplinks) {
		$self->links(\@nlinks);
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
			 (map { "$_\n" } @{$self->links}),
			 (map { "$_\n" } @{$self->annotations}),
		);
}

1;

