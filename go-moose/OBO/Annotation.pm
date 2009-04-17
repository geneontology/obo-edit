package OBO::Annotation;
use Moose;
use strict;
extends 'OBO::LinkStatement';
use OBO::Evidence;

has evidence => ( is=>'rw', isa=>'OBO::Evidence');

# alias
sub gene {
    shift->node(@_);
}

1;
