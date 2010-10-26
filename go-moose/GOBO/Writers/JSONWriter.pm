package GOBO::Writers::JSONWriter;
use Moose;
use strict;
extends 'GOBO::Writers::PerlHashWriter';
use GOBO::Node;
use GOBO::LinkStatement;
use JSON::PP;

sub write {
    my $self = shift;
    $self->SUPER::write();
    my $json = encode_json $self->hashref;
    print $json;
}

__PACKAGE__->meta->make_immutable;

1;
