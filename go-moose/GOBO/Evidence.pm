package GOBO::Evidence;
use Moose;
use Moose::Util::TypeConstraints;
use GOBO::Types;
extends 'GOBO::Node';

has ev_type => (is=>'rw', isa=>'GOBO::ClassNode', coerce=>1);
has supporting_entities => (is=>'rw', isa=>'ArrayRef[GOBO::Node]');

sub with_str {
    return join('|',@{shift->supporting_entities || []});
}

sub as_string {
    my $self = shift;
    return $self->ev_type . '-' . $self->with_str;
}

sub is_IEA {
    return shift->ev_type->id eq 'IEA';
}

__PACKAGE__->meta->make_immutable;

1;
