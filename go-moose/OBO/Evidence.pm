package OBO::Evidence;
use Moose;
use strict;
extends 'OBO::Node';

has type => (is=>'rw', isa=>'OBO::ClassNode', coerce=>1);
has supporting_entities => (is=>'rw', isa=>'ArrayRef[OBO::Node]', coerece=>1);

sub with_str {
    return join('|',@{shift->supporting_entities || []});
}

sub as_string {
    my $self = shift;
    return $self->type . '-' . $self->with_str;
}

1;
