package OBO::Indexes::NodeIndex;
use Moose;
use Carp;
use strict;
use OBO::Node;
use OBO::Node;
use OBO::Node;
use OBO::RelationNode;

has ixN => (is => 'rw', isa => 'HashRef[OBO::Node]', default=>sub{{}});
has ixLabel => (is => 'rw', isa => 'HashRef[ArrayRef[OBO::Node]]', default=>sub{{}});

sub create_node {
    my $self = shift;
    my $n = OBO::Node->new(@_); # TODO - other types
    $self->add_node($s);
    return $s;
}

# TODO - use Set::Object? List::MoreUtils?
sub add_node {
    my $self = shift;
    $self->add_nodes([@_]);
}

# TODO - check for duplicates?
sub add_nodes {
    my $self = shift;
    my $nl = shift;
    foreach my $n (@$nl) {
        my $nid = $n->id;
        $self->ixN->{$n->id} = $n;
        push(@{$self->ixLabel->{$n->label}}, $n);
    }
    return;
}

sub remove_nodes {
    my $self = shift;
    my $nl = shift;
    foreach my $n (@$nl) {
        my $nid = $s->node->id;
        delete $self->ixN->{$nid};
        my $arr = $self->ixT->{$n->label};
        @$arr = grep {!$n->equals($_)} @$arr;
    }
    return;
}

sub nodes {
    my $self = shift;
    if (@_) {
        # SET
        $self->clear_all;
        $self->add_nodes([@_]);
    }
    # GET
    return [map { @$_ } values %{$self->ixN}];
}

sub node_by_id {
    my $self = shift;
    my $x = shift;
    confess("requires argument") unless $x;
    return $self->ixN->{$x};
}

sub nodes_by_label {
    my $self = shift;
    my $x = shift;
    return $self->ixLabel->{$x} || [];
}


1;


=head1 NAME

OBO::Indexes::NodeIndex

=head1 SYNOPSIS

do not use this method directly

=head1 DESCRIPTION

Stores a collection of OBO::Node objects, optimized for fast
access. In general you should not need to use this directly - use
OBO::Graph instead, which includes different indexes for links,
annotations etc

=head2 TODO

Currently there are 2 indexes, by node ID (subject) and by primary
label, but in future this may be extended. General search may also be
added.

Eventually it should support any combination of indexing

=head2 Binding to a database

This index is in-memory. It can be extended to be bound to a database
(e.g. the GO Database) or to a Lucene index by overriding the methods

=cut
