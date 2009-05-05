package OBO::ClassExpression::RelationalExpression;
use Moose;
use strict;
extends 'OBO::ClassExpression::BooleanExpression';
use OBO::Statement;
use OBO::RelationNode;
use OBO::Node;

##delegation+constructors don't play well
##has 'statement' => (is=>'ro', isa=>'OBO::Statement',handles=>['relation','target']);

has relation => (is=>'ro', isa=>'OBO::RelationNode', coerce=>1);
has target => (is=>'ro', isa=>'OBO::Node', coerce=>1);

use overload ('""' => 'as_string');
sub as_string {
    my $self = shift;
    return sprintf('%s(%s)',$self->relation,$self->target);
}

1; 

=head1 NAME

OBO::ClassExpression::RelationalExpression

=head1 SYNOPSIS

=head1 DESCRIPTION

An OBO::ClassExpression in which the members are constructed by
applying a relation. For example, "the set of all things that are
part_of an oocyte". In this expression, the relation is part_of and
the target is oocyte.

=head2 OWL Translation

Same as a Class Restriction in OWL

=cut
