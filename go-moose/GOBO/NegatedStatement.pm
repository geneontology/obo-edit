package OBO::NegatedStatement;
use Moose;
use strict;
use OBO::Statement;
use Moose::Util::TypeConstraints;

coerce 'OBO::NegatedStatement'
    => from 'OBO::Statement'
    => via { new OBO::NegatedStatement(statement=>$_) };

has 'statement' => (is=>'ro', isa=>'OBO::Statement',handles=>qr/.*/);
1;
