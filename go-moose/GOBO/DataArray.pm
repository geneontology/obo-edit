package GOBO::DataArray;
use Moose;
use strict;
extends 'GOBO::Node';
# extends 'GOBO::TermNode';
has data => ( is=>'rw', isa=>'ArrayRef[Str]' );
# has target => ( is=>'rw', isa=>'Str' );
1;
