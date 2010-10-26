package GOBO::ClassExpression::Complement;
use Moose;
use Moose::Util::TypeConstraints;
use GOBO::Types;
extends 'GOBO::ClassExpression::BooleanExpression';

has '+arguments' => (isa => 'GOBO::Types::SingleNodeArray', coerce=>1);

sub operator { 'NOT ' }
sub operator_symbol { '-' }

override 'id' => sub {
	my $self = shift;

	if ($self->arguments->[0]->isa("GOBO::ClassExpression"))
	{	return $self->operator_symbol . "(" . $self->arguments->[0]->id . ")";
	}
	return $self->operator_symbol . $self->arguments->[0]->id;

};

use overload ('""' => 'as_string');

override 'as_string' => sub {
	my $self = shift;
	if ($self->arguments->[0]->isa("GOBO::ClassExpression"))
	{	return $self->operator . "(" . $self->arguments->[0]->as_string . ")";
	}
	return $self->operator . $self->arguments->[0]->as_string;
};


=head1 NAME

GOBO::ClassExpression::Complement

=head1 SYNOPSIS

=head1 DESCRIPTION

An GOBO::ClassExpression::BooleanExpression in which the set operator is one of complement.

=head2 OWL Mapping

equivalent to complementOf

=cut

__PACKAGE__->meta->make_immutable;

1;
