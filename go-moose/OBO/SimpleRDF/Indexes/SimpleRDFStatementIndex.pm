package OBO::SimpleRDF::Indexes::SimpleRDFStatementIndex;
use Moose;
extends 'OBO::Indexes::StatementIndex';
with 'OBO::SimpleRDF::Indexes::SimpleRDFWrapper';
use Carp;
use strict;
use OBO::Statement;
use OBO::Node;
use OBO::RelationNode;
use AmiGO::Model::Graph;

sub add_statements {
    my $self = shift;
    my $sl = shift;

    my $model = $self->get_model;
    foreach my $s (@$sl) {
        my $rs = $self->to_rdf($s);
        $model->add_statement($rs);
    }

    return;
}

sub remove_statements {
    my $self = shift;
    my $sl = shift;

    my $model = $self->model;
    foreach my $s (@$sl) {
        $model->remove_statement($self->to_rdf($s));
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
    my $qs = new OBO::LinkStatement;
    my $rs = $self->to_rdf($qs);
    return $self->matching_statements($rs);
    
}

sub statements_by_node_id {
    my $self = shift;
    my $x = shift;
    my $st = $self->to_rdf(node=>$x);
    return $self->matching_statements($st);
}

sub statements_by_target_id {
    my $self = shift;
    my $x = shift;
    my $st = $self->to_rdf(target=>$x);
    return $self->matching_statements($st);
}

sub matching_statements {
    my $self = shift;
    my $s = shift;
    my @sarr = $self->model->find_statements($s);
    return [map { $self->from_rdf($_) } @sarr];
}


1;


=head1 NAME

OBO::SimpleRDF::Indexes::StatementIndex

=head1 SYNOPSIS

do not use this method directly

=head1 DESCRIPTION

Overrides OBO::Indexes::StatementIndex (as used in OBO::Graph) to
provide direct DB connectivity to the AmiGO/GO Database. Uses the
SimpleRDF DBIx::Class layer

=cut
