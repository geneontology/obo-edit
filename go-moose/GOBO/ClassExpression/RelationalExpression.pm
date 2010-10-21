package GOBO::ClassExpression::RelationalExpression;
use Moose;
use Moose::Util::TypeConstraints;
use GOBO::Types;
extends 'GOBO::ClassExpression';
use GOBO::Statement;
use GOBO::RelationNode;
use GOBO::Node;

##delegation+constructors don't play well
##has 'statement' => (is=>'ro', isa=>'GOBO::Statement',handles=>['relation','target']);

has relation => (is=>'ro', isa=>'GOBO::RelationNode', coerce=>1);
has target => (is=>'ro', isa=>'GOBO::Node', coerce=>1);
has cardinality => (is=>'ro', isa=>'Int');
has max_cardinality => (is=>'ro', isa=>'Int');
has min_cardinality => (is=>'ro', isa=>'Int');

use overload ('""' => 'as_string');

sub id {
	my $self = shift;
	return sprintf('%s(%s)', $self->relation->id, $self->target->id);
}

sub as_string {
    my $self = shift;
    return sprintf('%s(%s)',$self->relation,$self->target);
}

sub get_expr_type {
	my $self = shift;
	my %hash = (@_);
	if ($self->target)
	{	if ($self->target->isa('GOBO::ClassExpression'))
		{	my $results = $self->target->get_expr_type(%hash);
			foreach (keys %$results)
			{	$hash{$_} += $results->{$_};
			}
		}
		else
		{	my $got;
			foreach my $type qw( GOBO::TermNode GOBO::Gene GOBO::RelationNode GOBO::InstanceNode)
			{	if ($self->target->isa($type))
				{	$hash{$type}++;
					$got++ && last;
				}
			}
			$hash{ ref $_ }++ if ! $got;
		}
	}
	return \%hash;
}



1;

=head1 NAME

GOBO::ClassExpression::RelationalExpression

=head1 SYNOPSIS

=head1 DESCRIPTION

An GOBO::ClassExpression in which the members are constructed by
applying a relation. For example, "the set of all things that are
part_of an oocyte". In this expression, the relation is part_of and
the target is oocyte.

=head2 Syntax

 REL(TARGET)

For example:

  part_of(CL:0001234)

=head2 OWL Translation

Same as a Class Restriction in OWL

=cut
