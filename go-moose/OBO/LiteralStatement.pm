package OBO::LiteralStatement;
use Moose;
use strict;
extends 'OBO::Statement';
use OBO::Node;

has 'target' => ( is=>'ro', isa=>'Value');

# TODO -- use this or use frame-style? both

1;
