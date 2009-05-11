=head1 NAME

OBO::Graph

=head1 SYNOPSIS

=head1 DESCRIPTION

A collection of inter-related OBO::Node objects. With a simple
ontology these are typically OBO::TermNode objects, although other
graphs e.g. instance graphs are possible.

This module deliberately omits any kind of graph traversal
functionality. This is done by an OBO::InferenceEngine.

=head1 SEE ALSO

OBO::Node

OBO::LinkStatement

=cut

package OBO::Graph;
use Moose;
our $VERSION='0.01-pre';
use strict;
use OBO::Statement;
use OBO::Annotation;
use OBO::Node;
use OBO::TermNode;
use OBO::RelationNode;
use OBO::Indexes::StatementIndex;
use overload ('""' => 'as_string');

has 'relations' => (is => 'rw', isa => 'ArrayRef[OBO::TermNode]', default=>sub{[]});
has 'terms' => (is => 'rw', isa => 'ArrayRef[OBO::TermNode]', default=>sub{[]});
#has 'links' => (is => 'rw', isa => 'ArrayRef[OBO::LinkStatement]', default=>sub{[]});
has 'link_ix' => (is => 'rw', isa => 'OBO::Indexes::StatementIndex', 
                  default=>sub{ new OBO::Indexes::StatementIndex() });
#has 'annotations' => (is => 'rw', isa => 'ArrayRef[OBO::Annotation]', default=>sub{[]});
has 'annotation_ix' => (is => 'rw', isa => 'OBO::Indexes::StatementIndex', 
                  default=>sub{ new OBO::Indexes::StatementIndex() });
has 'node_index' => (is => 'rw', isa => 'HashRef[OBO::Node]', default=>sub{{}});

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

sub links { shift->link_ix->statements(@_) }
sub add_link { shift->link_ix->add_statement(@_) }
sub add_links { shift->link_ix->add_statements(@_) }
sub remove_link { shift->link_ix->remove_statements([@_]) }

sub annotations { shift->annotation_ix->statements(@_) }
sub add_annotation { shift->annotation_ix->add_statement(@_) }
sub add_annotations { shift->annotation_ix->add_statements(@_) }
sub remove_annotation { shift->annotation_ix->remove_statements([@_]) }

=head2 get_target_links (subject OBO::Node, relation OBO::RelationNode OPTIONAL)

given a subject (child), get target (parent) links

if relation is specified, also filters results on relation

=cut

sub get_target_links {
    my $self = shift;
    my $n = shift;
    my $rel = shift;
    my $sl = $self->link_ix->statements_by_node_id(ref($n) ? $n->id : $n);
    if ($rel) {
        my $rid = ref($rel) ? $rel->id : $rel; 
        return [grep {$_->relation->id eq $rid} @$sl];
    }
    return $sl;
}

sub noderef {
    my $self = shift;
    #my $fac = shift || sub {new OBO::Node(id=>shift)};
    my $id = shift;
    my $ix = $self->node_index;
    if (!$ix->{$id}) {
        #print STDERR "Adding node: $id\n";
        $ix->{$id} = new OBO::Node(id=>$id);
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
    if (!$n->isa('OBO::TermNode')) {
        #$n = new OBO::ClassNode(%$n); # TODO - re-bless?
        bless $n, 'OBO::TermNode';
    }
    return $n;
}

sub relation_noderef {
    my $self = shift;
    my $n = $self->noderef(@_);
    if (!$n->isa('OBO::RelationNode')) {
        bless $n, 'OBO::RelationNode';
    }
    return $n;
}


# logical definitions can be directly attached to TermNodes, or they can be
# present in the graph as intersection links
# TBD : move to utility class?
use OBO::ClassExpression::RelationalExpression;
use OBO::ClassExpression::Intersection;
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
                        new OBO::ClassExpression::RelationalExpression(relation=>$_->relation, target=>$_->target);
                    }
            } @{$xpnodeh{$nid}};
            if (@exprs < 2) {
                $self->throw("invalid intersection links for $nid. Need at least 2, you have @exprs");
            }
            $n->logical_definition(new OBO::ClassExpression::Intersection(arguments=>\@exprs));
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

