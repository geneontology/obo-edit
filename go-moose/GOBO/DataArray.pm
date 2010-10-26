package GOBO::DataArray;
use Moose;
use strict;
extends 'GOBO::Gene';
## data is in the form { id => Str, data => [ arr ] }
has data_arr => ( is=>'rw', isa=>'ArrayRef[HashRef]', predicate => 'has_data_arr' );

sub get_annotation_data {
	my $self = shift;
	return [] unless $self->has_data_arr;
	return [ map { $_->{data} } @{$self->data_arr} ];
}

__PACKAGE__->meta->make_immutable;

1;
