package GOBO::Statement;
use Moose;
use Moose::Util::TypeConstraints;
use GOBO::Types;
with 'GOBO::Attributed';
with 'GOBO::Identified';

#use GOBO::Node;
use GOBO::RelationNode;

has 'node' => ( is=>'rw', isa=>'GOBO::Node', coerce=>1 );
has 'relation' => ( is=>'rw', isa=>'GOBO::RelationNode', coerce=>1 );
has 'inferred' => ( is=>'rw', isa=>'Bool');
has 'negated' => ( is=>'rw', isa=>'Bool'); # TODO: use this or NegatedStatement?
has 'is_intersection' => ( is=>'rw', isa=>'Bool');
has 'is_union' => ( is=>'rw', isa=>'Bool');
has 'sub_statements' => ( is=>'rw', isa=>'ArrayRef[GOBO::Statement]');

use overload ('""' => 'as_string');
sub as_string {
    my $self = shift;
    return sprintf("(%s --[%s]-->%s)",$self->node || '?',$self->relation || '?', $self->can('target') ? $self->target : '?' );
}


sub matches {
    my $self = shift;
    my %h = @_;
#    my $h_str = join(".", sort keys %h);
#    my $s_str = join(".", sort keys %$self);
#    return 0 unless $h_str eq $s_str;

    foreach my $k (keys %h) {
        my $v = $self->$k;
        return 0 unless $v->id eq $h{$k};
    }

    return 1;
}

## Look at the constituents of a statement and work out what type of statement
## it should be.

sub get_statement_type {
	my $self = shift;

	if (! $self->can('target') || ! defined $self->target)
	{	return 'GOBO::Statement';
	}
	if ($self->isa('GOBO::Annotation'))
	{	return 'GOBO::Annotation';
	}

	my $args;
	foreach my $n qw(node target)
	{	$args->{$n} = $self->$n if defined $self->$n;
	}
	return get_statement_type_from_args(%$args);
}


##
sub get_statement_type_from_args {
	my %args = (@_);
	my $type;
	foreach my $n qw(node target)
	{	next unless $args{$n};
		if ($args{$n}->isa('GOBO::ClassExpression'))
		{	my %hash;
			my $results = $args{$n}->get_expr_type(%hash);
			$type->{$n} = join("__", sort keys %$results);
		#	print STDERR "type $n has expressions " . $type->{$n} . "\n\n";
		}
		else
		{	foreach my $x qw( GOBO::TermNode GOBO::AnnotationSubject GOBO::Gene GOBO::RelationNode GOBO::InstanceNode)
			{	if ($args{$n}->isa($x))
				{	$type->{$n} = $x;
					last;
				}
			}
			$type->{$n} = ref $args{$n} if ! $type->{$n};
		}
	}

#	print STDERR "type: " . Dumper($type) . "\n";

	if ($type->{target} eq 'GOBO::TermNode' && ($type->{node} eq 'GOBO::Gene' || $type->{node} eq 'GOBO::AnnotationSubject') )
	{	return 'GOBO::Annotation';
	}
	elsif ($type->{node} eq 'GOBO::TermNode' && $type->{target} eq 'GOBO::TermNode')
	{	return 'GOBO::OntologyLinkStatement';
	}
	elsif (defined $type->{node} && defined $type->{target})
	{	return 'GOBO::LinkStatement';
	}
#	print STDERR "GOBO::Statement\n";
		return 'GOBO::Statement';
}


=head2 create

Create a Statement object, given a set of arguments

 input:  node => GOBO::Node
         relation => GOBO::Node
         target => GOBO::X (object of some sort)
         type => 'GOBO::Annotation' / 'GOBO::LinkStatement' / 'GOBO::Statement' (etc.)
 output: a link object of the appropriate type

if type is not specified, the following rules will be followed:

- if no target is given, a Statement object will be created
- if the target is a GOBO::Gene, an Annotation object will be created
- otherwise, a LinkStatement will be created

=cut

sub create {
	my $proto = shift;
	my %args = (@_);

	if (! $args{type})
	{	$args{type} = get_statement_type_from_args(%args);
		if ($args{type} eq 'GOBO::OntologyLinkStatement')
		{	$args{type} = 'GOBO::LinkStatement';
		}
	}

	## otherwise, use the type we were given
	my $link = $args{type}->new( %args );
	return $link;
}


=head1 NAME

GOBO::Statement

=head1 SYNOPSIS

  printf '%s --[%s]--> %s', $s->node, $s->relation, $->target;

=head1 DESCRIPTION

A type of GOBO::Statement that connects an GOBO::Node object to another
entity via a GOBO::RelationNode object. This can be thought of as a
sentence or statement about a node.

In RDF and Chado terminology, the node can be thought of as the
"subject", and the target the "object". The terms "subject" and
"object" are avoided due to being overloaded.

The two subtypes are GOBO::LinkStatement (edges) or
GOBO::LiteralStatement (tag-values). For most bio-ontologies, the
Statements will be LinkStatements.

Statements have the roles GOBO::Attributed and GOBO::Identified. This
means they can have metadata attached. For example, who made the
statement and when.


=cut

1;
