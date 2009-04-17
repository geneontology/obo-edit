package OBO::Definable;
use Moose::Role;

has definition => (is=>'rw', isa=>'Str');
has definition_xrefs => (is=>'rw', isa=>'ArrayRef[Str]');

1;

