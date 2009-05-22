package OBO::Amigo::Indexes::StatementIndex;
use Moose::Role;
use AmiGO::Model::Schema;

has schema => (is=>'rw', isa=>'AmiGO::Model::Schema');


1;


=head1 NAME

OBO::Amigo::Indexes::AmigoWrapper

=head1 SYNOPSIS

do not use this method directly

=head1 DESCRIPTION

Role of providing direct DB connectivity to the AmiGO/GO Database

=cut
