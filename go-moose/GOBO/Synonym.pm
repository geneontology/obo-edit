=head1 NAME

GOBO::Synonym

=head1 SYNOPSIS

=head1 DESCRIPTION

An alternate label for an GOBO::Labeled object

=cut

package GOBO::Synonym;
use Moose;
use Moose::Util::TypeConstraints;
use GOBO::Types;

with 'GOBO::Attributed';

has label => (is=>'rw',isa=>'Str');
has scope => (is=>'rw',isa=>'GOBO::Synonym::Scope');
has synonym_type => (is=>'rw',isa=>'GOBO::Node', coerce=>1);
has lang => (is=>'rw',isa=>'Str');

sub is_valid_synonym_scope {
	my $self = shift;
	my $scope = shift;
	if (grep { $scope eq $_ } qw( EXACT BROAD NARROW RELATED ))
	{	return 1;
	}
	return undef;
}

__PACKAGE__->meta->make_immutable;

1;
