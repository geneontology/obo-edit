package OBO::Graph;
use Moose;
use strict;
use OBO::Statement;
use OBO::Annotation;
use OBO::Node;
use OBO::TermNode;
use OBO::RelationNode;
use overload ('""' => 'as_string');

has 'relations' => (is => 'rw', isa => 'ArrayRef[OBO::TermNode]', default=>sub{[]});
has 'terms' => (is => 'rw', isa => 'ArrayRef[OBO::TermNode]', default=>sub{[]});
has 'links' => (is => 'rw', isa => 'ArrayRef[OBO::LinkStatement]', default=>sub{[]});
has 'annotations' => (is => 'rw', isa => 'ArrayRef[OBO::Annotation]', default=>sub{[]});
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

sub add_links {
    my $self = shift;
    my $links = shift;
    push(@{$self->links}, @$links);
    return;
}

sub noderef {
    my $self = shift;
    #my $fac = shift || sub {new OBO::Node(id=>shift)};
    my $id = shift;
    my $ix = $self->node_index;
    if (!$ix->{$id}) {
        $ix->{$id} = new OBO::Node(id=>$id);
        #$ix->{$id} = $fac->($id);
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

sub get_target_links {
    my $self = shift;
    my $n = shift;
    my $nid = $n->id;
    # TODO: use an index
    my @links =
        grep { $_->node->id eq $nid } @{$self->links};
    return \@links;
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


=head1 NAME

OBO::Graph

=head1 SYNOPSIS

=head1 DESCRIPTION

A collection of inter-relation OBO::Node objects. With a simple
ontology these are typically OBO::TermNode objects, although other
graphs e.g. instance graphs are possible.

This module deliberately omits any kind of graph traversal
functionality. This is done by an OBO::InferenceEngine.

=head1 SEE ALSO

OBO::Node

OBO::LinkStatement

=cut
