package OBO::LinkStatement;
use Moose;
use strict;
extends 'OBO::Statement';
use OBO::Node;

has 'target' => ( is=>'ro', isa=>'OBO::Node', coerce=>1 );

1;
