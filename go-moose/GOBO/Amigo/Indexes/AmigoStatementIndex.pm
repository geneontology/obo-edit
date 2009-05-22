package GOBO::Amigo::Indexes::AmigoStatementIndex;
use Moose;
extends 'GOBO::Indexes::StatementIndex';
with 'GOBO::Amigo::Indexes::AmigoWrapper';
use Carp;
use strict;
use GOBO::Statement;
use GOBO::Node;
use GOBO::RelationNode;
use AmiGO::Model::Graph;

sub add_statements {
    my $self = shift;
    my $sl = shift;

    # read only?

    return;
}

sub remove_statements {
    my $self = shift;
    my $sl = shift;

    # read only?

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
    
    # TODO
}

sub statements_by_node_id {
    my $self = shift;
    my $x = shift;
    # CALL DBIx::Class USING AMIGO HERE

    $schema = $self->schema;
    my $rrs = $schema->resultset('Term2Term')->search({ 'term2.acc' => $x });
    my @sl = map { $self->convert($_) } @$rrs;
    return \@sl;
}

sub statements_by_target_id {
    my $self = shift;
    my $x = shift;
    # CALL DBIx::Class USING AMIGO HERE

    $schema = $self->schema;
    my $rrs = $schema->resultset('Term2Term')->search({ 'term1.acc' => $x });
    my @sl = map { $self->convert($_) } @$rrs;
    return \@sl;
}

sub convert {
    my $self = shift;
    my $rs = shift;
    # TODO: use a factory to create GOBO::Statement objs
    return new 
      GOBO::Statement(node=>$rs->subject->acc,
                     relation=>$rs->relation->acc,
                     target=>$rs->object->acc);
}

1;


=head1 NAME

GOBO::Amigo::Indexes::StatementIndex

=head1 SYNOPSIS

do not use this method directly

=head1 DESCRIPTION

Overrides GOBO::Indexes::StatementIndex (as used in GOBO::Graph) to
provide direct DB connectivity to the AmiGO/GO Database. Uses the
Amigo DBIx::Class layer

=cut
