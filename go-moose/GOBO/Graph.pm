=head1 NAME

GOBO::Graph

=head1 SYNOPSIS

=head1 DESCRIPTION

A collection of inter-related GOBO::Node objects. With a simple
ontology these are typically GOBO::TermNode objects, although other
graphs e.g. instance graphs are possible.

This module deliberately omits any kind of graph traversal
functionality. This is done by an GOBO::InferenceEngine.

=head1 SEE ALSO

GOBO::Node

GOBO::LinkStatement

=cut

package GOBO::Graph;
use Moose;
our $VERSION='0.01-pre';
use strict;
use GOBO::Statement;
use GOBO::Annotation;
use GOBO::Node;
use GOBO::TermNode;
use GOBO::RelationNode;
use GOBO::Indexes::StatementIndex;
use overload ('""' => 'as_string');

has 'relations' => (is => 'rw', isa => 'ArrayRef[GOBO::TermNode]', default=>sub{[]});
has 'terms' => (is => 'rw', isa => 'ArrayRef[GOBO::TermNode]', default=>sub{[]});
has 'link_ix' => (is => 'rw', isa => 'GOBO::Indexes::StatementIndex', 
                  default=>sub{ new GOBO::Indexes::StatementIndex() });
has 'annotation_ix' => (is => 'rw', isa => 'GOBO::Indexes::StatementIndex', 
                  default=>sub{ new GOBO::Indexes::StatementIndex() });
has 'node_index' => (is => 'rw', isa => 'HashRef[GOBO::Node]', default=>sub{{}});

sub add_term {
    my $self = shift;
    push(@{$self->terms},@_);
    return;
}

sub add_relation {
    my $self = shift;
    push(@{$self->relations},@_);
    return;
}

sub nodes {
    my $self = shift;
    return [values %{$self->node_index}];
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
    #my $fac = shift || sub {new GOBO::Node(id=>shift)};
    my $id = shift;
    my $ix = $self->node_index;
    if (!$ix->{$id}) {
        #print STDERR "Adding node: $id\n";
        $ix->{$id} = new GOBO::Node(id=>$id);
        #$ix->{$id} = $fac->($id);
    }
    else {
        #print STDERR "Already have referenced node: $id\n";
    }
    return $ix->{$id};
}

sub term_noderef {
    my $self = shift;
    my $n = $self->noderef(@_);
    if (!$n->isa('GOBO::TermNode')) {
        #$n = new GOBO::ClassNode(%$n); # TODO - re-bless?
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

