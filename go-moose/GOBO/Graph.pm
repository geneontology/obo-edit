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
our $VERSION='0.01-pre';
use strict;
use GOBO::Statement;
use GOBO::Annotation;
use GOBO::Node;
use GOBO::Subset;
use GOBO::TermNode;
use GOBO::RelationNode;
use GOBO::Indexes::StatementIndex;
use GOBO::Indexes::NodeIndex;
use overload ('""' => 'as_string');

has 'relation_h' => (is => 'rw', isa => 'HashRef[GOBO::TermNode]', default=>sub{{}});
has 'term_h' => (is => 'rw', isa => 'HashRef[GOBO::TermNode]', default=>sub{{}});
has 'instance_h' => (is => 'rw', isa => 'HashRef[GOBO::InstanceNode]', default=>sub{{}});
has 'link_ix' => (is => 'rw', isa => 'GOBO::Indexes::StatementIndex', 
                  default=>sub{ new GOBO::Indexes::StatementIndex() });
has 'annotation_ix' => (is => 'rw', isa => 'GOBO::Indexes::StatementIndex', 
                  default=>sub{ new GOBO::Indexes::StatementIndex() });
#has 'node_index' => (is => 'rw', isa => 'HashRef[GOBO::Node]', default=>sub{{}});
has 'node_index' => (is => 'rw', isa => 'GOBO::Indexes::NodeIndex', 
                  default=>sub{ new GOBO::Indexes::NodeIndex() });

has 'subset_index' => (is => 'rw', isa => 'HashRef[GOBO::Subset]', default=>sub{{}});


sub terms {
    my $self = shift;
    #$self->node_index->nodes_by_metaclass('term');
    return [values %{$self->term_h}];
}

sub relations {
    my $self = shift;
    #$self->node_index->nodes_by_metaclass('relation');
    return [values %{$self->relation_h}];
}

sub instances {
    my $self = shift;
    #$self->node_index->nodes_by_metaclass('instance');
    return [values %{$self->instance_h}];
}

sub add_term {
    my $self = shift;
    my $n = $self->term_noderef(@_);
    $self->term_h->{$n->id} = $n;
    return $n;
}


sub add_relation {
    my $self = shift;
    my $n = $self->relation_noderef(@_);
    $self->relation_h->{$n->id} = $n;
    return $n;
}

sub add_instance {
    my $self = shift;
    my $n = $self->instance_noderef(@_);
    $self->instance_h->{$n->id} = $n;
    return $n;
}

sub nodes {
    my $self = shift;
    return $self->node_index->nodes;
}

sub links { shift->link_ix->statements(@_) }
sub add_link { shift->link_ix->add_statement(@_) }
sub add_links { shift->link_ix->add_statements(@_) }
sub remove_link { shift->link_ix->remove_statements([@_]) }

sub annotations { shift->annotation_ix->statements(@_) }
sub add_annotation { shift->annotation_ix->add_statement(@_) }
sub add_annotations { shift->annotation_ix->add_statements(@_) }
sub remove_annotation { shift->annotation_ix->remove_statements([@_]) }
sub annotated_entities { shift->annotation_ix->referenced_nodes }

=head2 get_target_links (subject GOBO::Node, relation GOBO::RelationNode OPTIONAL)

given a subject (child), get target (parent) links

if relation is specified, also filters results on relation

=cut

sub get_target_links {
    my $self = shift;
    my $n = shift;
    my $rel = shift;
    my @sl = @{$self->link_ix->statements_by_node_id(ref($n) ? $n->id : $n) || []};
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


sub noderef {
    my $self = shift;
    my $id = shift;
    if (ref($id)) {
        # $id is actually a GOBO::Node
        $id = $id->id;
    }
    my $ix = $self->node_index;
    my $n = $ix->node_by_id($id);
    if (!$n) {
        $n = new GOBO::Node(id=>$id);
        $ix->add_node( $n );
    }
    else {
    }
    return $n;
}

sub term_noderef {
    my $self = shift;
    my $n = $self->noderef(@_);
    if (!$n->isa('GOBO::TermNode')) {
        bless $n, 'GOBO::TermNode';
    }
    return $n;
}

sub relation_noderef {
    my $self = shift;
    my $n = $self->noderef(@_);
    if (!$n->isa('GOBO::RelationNode')) {
        bless $n, 'GOBO::RelationNode';
    }
    return $n;
}

sub instance_noderef {
    my $self = shift;
    my $n = $self->noderef(@_);
    if (!$n->isa('GOBO::InstanceNode')) {
        bless $n, 'GOBO::InstanceNode';
    }
    return $n;
}

sub subset_noderef {
    my $self = shift;
    my $ssid = shift;
    my $n = $self->subset_index->{$ssid};
    if (!$n) {
        # TODO: fail?
        warn "creating subset $ssid";
        $n = new GOBO::Subset(id=>$ssid);
        $self->subset_index->{$ssid} = $n;
    }
    if (!$n->isa('GOBO::Subset')) {
        bless $n, 'GOBO::Subset';
    }
    return $n;
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

