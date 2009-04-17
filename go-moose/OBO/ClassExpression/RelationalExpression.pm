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

=head2 Documentation

Same as Class Restriction in OWL

=cut
