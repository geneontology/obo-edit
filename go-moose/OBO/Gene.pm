package OBO::Gene;
use Moose;
use strict;
extends 'OBO::Node';
use OBO::Node;

has type => (is=>'rw', isa=>'OBO::Node', coerce=>1);
has taxon => (is=>'rw', isa=>'OBO::Node', coerce=>1);

1;
