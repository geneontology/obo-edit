package GOBO::NegatedStatement;
use Moose;
use Moose::Util::TypeConstraints;
use GOBO::Types;

has 'statement' => (is=>'ro', isa=>'GOBO::Statement',handles=>qr/.*/);
__PACKAGE__->meta->make_immutable;

1;
