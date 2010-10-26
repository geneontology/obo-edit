package GOBO::ClassExpression::BooleanExpression;
use Moose;
use Moose::Util::TypeConstraints;
use GOBO::Types;

extends 'GOBO::ClassExpression';

has 'arguments' => (is=>'rw', isa=>'GOBO::Types::NodeArray', default=>sub{[]}, coerce=>1);

sub operator { undef }
sub operator_symbol { undef }

sub id {
	my $self = shift;
	return join($self->operator_symbol,
	map {
		if ($_->isa('GOBO::ClassExpression::BooleanExpression')) {
			"(" . $_->id . ")"
		}
		else {
			$_->id
		}
	} @{$self->arguments});
}

use overload ('""' => 'as_string');

sub as_string {
	my $self = shift;
	return join($self->operator,
		map {
			if (!defined($_)) {
				'';
			}
			elsif ($_->isa('GOBO::ClassExpression::BooleanExpression')) {
				"($_)"
			}
			else {
				$_
			}
		} @{$self->arguments});
}

sub add_argument {
	my $self = shift;
	my $c = shift;
	push(@{$self->arguments},$c);
}

# @Override
sub normalize {
    my $self = shift;

    foreach (@{$self->arguments}) {
        if ($_->can('normalize')) {
            $_->normalize;
        }
    }

    #  A and (B and C) ==> A and B and C
    #  A or (B or C) ==> A or B or C

    my @new_args = ();
    foreach (@{$self->arguments}) {
        if ($_->isa('GOBO::ClassExpression::BooleanExpression')) {
            if ($_->operator eq $self->operator) {
                push(@new_args, @{$_->arguments});
                next;
            }
        }
        push(@new_args, $_);
    }
    $self->arguments(\@new_args);

=cut
    #  A or (B and C) ==> (A and B) or (A and C)
    #  A and (B or C) ==> (A or B) and (A or C)
    if (scalar @{$self->arguments} == 2 && ( $self->arguments[0]->isa('GOBO::ClassExpression::BooleanExpression') || $self->arguments[1]->isa('GOBO::ClassExpression::BooleanExpression') ) && ( ! $self->arguments[0]->isa('GOBO::ClassExpression::BooleanExpression') && $self->arguments[1]->isa('GOBO::ClassExpression::BooleanExpression') ))
    {	my ($t1, $t2);
    	if ($self->arguments[0]->isa('GOBO::ClassExpression::Union') && $self->arguments[1]->isa('GOBO::Node'))
    	{

    	}
    	elsif ($self->arguments[0]->isa('GOBO::ClassExpression::Intersection'))
    	{

    	}

    	if ($self->isa('GOBO::ClassExpression::Union'))
    		{

    		}
    		elsif ($self->isa('GOBO::ClassExpression::Intersection'))
    		{

    		}
    }
=cut
    return;
}

sub get_expr_type {
	my $self = shift;
	my %hash = (@_);
	use Data::Dumper;
	ARG_LOOP:
	foreach (@{$self->arguments})
	{	foreach my $type qw( GOBO::TermNode GOBO::Gene GOBO::RelationNode GOBO::InstanceNode)
		{	if ($_->isa($type))
			{	$hash{$type}++ && next ARG_LOOP;
			}
		}
		if ($_->isa('GOBO::ClassExpression'))
		{	my $results = $_->get_expr_type(%hash);
			foreach (keys %$results)
			{	$hash{$_} += $results->{$_};
			}
		}
		else
		{	$hash{ ref $_ }++;
		}
	}
	return \%hash;
}


=head1 NAME

GOBO::ClassExpression::BooleanExpression

=head1 SYNOPSIS

=head1 DESCRIPTION

An GOBO::ClassExpression in which the members are constructed via a
boolean operation. These are AND, OR, NOT - or in set terms, GOBO::ClassExpression::Intersection, GOBO::ClassExpression::Union
or GOBO::ClassExpression::Complement)

=cut

__PACKAGE__->meta->make_immutable;

1;
