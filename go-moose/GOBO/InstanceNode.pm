package GOBO::InstanceNode;
use Moose;
use strict;
extends 'GOBO::Node';

has types => (is=>'rw', isa=>'ArrayRef[GOBO::ClassNode]');

1;
