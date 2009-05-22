=head1 NAME

OBO::Synonym

=head1 SYNOPSIS

=head1 DESCRIPTION

An alternate label for an OBO::Labeled object

=cut

package OBO::Synonym;
use Moose;
use strict;
use OBO::Node;
with 'OBO::Attributed';

use Moose::Util::TypeConstraints;

coerce 'OBO::Synonym'
      => from 'Str'
      => via { new OBO::Synonym(label=>$_) };

has label => (is=>'rw',isa=>'Str');
has scope => (is=>'rw',isa=>'Str');
has type => (is=>'rw',isa=>'OBO::Node', coerce=>1);
has lang => (is=>'rw',isa=>'Str');

1;
