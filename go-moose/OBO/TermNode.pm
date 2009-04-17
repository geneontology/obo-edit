package OBO::TermNode;
use Moose;
use strict;
extends 'OBO::ClassNode';

has definition => (is=>'rw', isa=>'Str');
has definition_xrefs => (is=>'rw', isa=>'ArrayRef[Str]');


1;
