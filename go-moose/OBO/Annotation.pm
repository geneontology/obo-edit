package OBO::Annotation;
use Moose;
use strict;
extends 'OBO::LinkStatement';
use OBO::Evidence;

has evidence => ( is=>'rw', isa=>'OBO::Evidence');
has specific_node => ( is=>'rw', isa=>'OBO::Node');

# alias
sub gene {
    shift->node(@_);
}

1;
