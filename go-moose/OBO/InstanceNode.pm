package OBO::InstanceNode;
use Moose;
use strict;
extends 'OBO::Node';

has types => (is=>'rw', isa=>'ArrayRef[OBO::ClassNode]');

1;
