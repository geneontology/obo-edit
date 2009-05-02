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

sub as_string {
    my $self = shift;
    return
        join('',
             (map { "$_\n" } @{$self->links}),
             (map { "$_\n" } @{$self->annotations}),
        );
}

1;


