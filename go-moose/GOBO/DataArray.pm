package GOBO::DataArray;
use Moose;
use strict;
extends 'GOBO::Gene';
## data is in the form { id => Str, data => [ arr ] }
has data_arr => ( is=>'rw', isa=>'ArrayRef[HashRef]' );

1;
