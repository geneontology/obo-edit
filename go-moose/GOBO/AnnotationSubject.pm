=head1 NAME

GOBO::AnnotationSubject

=head1 SYNOPSIS

=head1 DESCRIPTION

A node for any kind of entity that can be the subject of an Annotation

=cut

package GOBO::AnnotationSubject;
use Moose;
use Moose::Util::TypeConstraints;

extends 'GOBO::Node';
# with 'GOBO::Attributed';

#coerce 'GOBO::AnnotationSubject'
# => from 'GOBO::Node'
# => via { bless $_, 'GOBO::AnnotationSubject' }
# => from 'Str'
# => via { new GOBO::AnnotationSubject(id=>$_) };

has gp_type => (is=>'rw', isa=>'GOBO::Node', coerce=>1);
has taxon => (is=>'rw', isa=>'GOBO::Node', coerce=>1);

__PACKAGE__->meta->make_immutable;

1;

