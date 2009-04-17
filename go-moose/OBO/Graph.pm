package OBO::Graph;
use Moose;
use strict;
extends 'OBO::Statement';
use OBO::Statement;
use OBO::Annotation;
use OBO::Node;
use OBO::TermNode;
use OBO::RelationNode;
use overload ('""' => 'as_string');

has 'statements' => (is => 'rw', isa => 'ArrayRef[OBO::Statement]', default=>sub{[]});
has 'links' => (is => 'rw', isa => 'ArrayRef[OBO::LinkStatement]', default=>sub{[]});
has 'annotations' => (is => 'rw', isa => 'ArrayRef[OBO::Annotation]', default=>sub{[]});
has 'node_index' => (is => 'rw', isa => 'HashRef[OBO::Node]', default=>sub{{}});

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


sub as_string {
    my $self = shift;
    foreach (@{$self->statements}) {
        print "S: $_\n";
    }
    foreach (@{$self->annotations}) {
        print "S: $_\n";
    }
}

1;


