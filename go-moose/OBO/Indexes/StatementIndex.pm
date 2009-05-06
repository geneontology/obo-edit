package OBO::Indexes::StatementIndex;
use Moose;
use Carp;
use strict;
use OBO::Statement;
use OBO::Node;
use OBO::RelationNode;

has ixN => (is => 'rw', isa => 'HashRef[ArrayRef[OBO::LinkStatement]]', default=>sub{{}});
has ixT => (is => 'rw', isa => 'HashRef[ArrayRef[OBO::LinkStatement]]', default=>sub{{}});

# TODO - use Set::Object

sub add_statement {
    my $self = shift;
    $self->add_statements([@_]);
}

# TODO - check for duplicates?
sub add_statements {
    my $self = shift;
    my $sl = shift;
    foreach my $s (@$sl) {
        my $nid = $s->node->id;
        push(@{$self->ixN->{$nid}}, $s);
        if ($s->isa("OBO::LinkStatement")) {
            my $tid = $s->target->id;
            push(@{$self->ixT->{$tid}}, $s);
        }
    }
    return;
}

sub remove_statements {
    my $self = shift;
    my $sl = shift;
    foreach my $s (@$sl) {
        my $nid = $s->node->id;
        # TODO - Set::Object?
        my $arr = $self->ixN->{$nid};
        @$arr = grep {!$s->equals($_)} @$arr;
        if ($s->isa("OBO::LinkStatement")) {
            my $tid = $s->target->id;
            my $arr = $self->ixT->{$tid};
            @$arr = grep {!$s->equals($_)} @$arr;
        }
    }
    return;
}

sub statements {
    my $self = shift;
    if (@_) {
        # SET
        $self->clear_all;
        $self->add_statements([@_]);
    }
    # GET
    return [map { @$_ } values %{$self->ixN}];
}

sub statements_by_node_id {
    my $self = shift;
    my $x = shift;
    confess("requires argument") unless $x;
    return $self->ixN->{$x} || [];
}

sub statements_by_target_id {
    my $self = shift;
    my $x = shift;
    return $self->ixT->{$x} || [];
}

1;


=head1 NAME

OBO::Indexes::StatementIndex

=head1 SYNOPSIS

do not use this method directly

=head1 DESCRIPTION

Stores a collection of OBO::Statement objects, optimized for fast
access. In general you should not need to use this directly - use
OBO::Graph instead

=head1 BINDING TO A DATABASE

This index is in-memory. It can be extended to be bound to a database
(e.g. the GO Database)

=cut
