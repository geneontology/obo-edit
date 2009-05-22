=head1 NAME

GOBO::Attribute

=head1 SYNOPSIS

=head1 DESCRIPTION

A role for any kind of entity that can be attributed to some source (annotated). Here 'entity' includes GOBO::Statement objects

=head2 TBD

Is this over-abstraction? This could be simply mixed in with Statement

=cut

package GOBO::Attributed;
use Moose::Role;
use strict;

use Moose::Util::TypeConstraints;
require DateTime;

subtype 'Date'
    => as 'Object'
    => where { $_->isa('DateTime') };

coerce 'Date'
    => from 'Str'
    => via {
        if (/(\d\d\d\d)(\d\d)(\d\d)/) {
            DateTime->new(year=>$1,month=>$2,day=>$3);
        }
        else {
            undef;
        }
};

has source => ( is=>'rw', isa=>'GOBO::Node', coerce=>1);
has provenance => ( is=>'rw', isa=>'GOBO::Node', coerce=>1);
has date => ( is=>'rw', isa=>'Date', coerce=>1); # TODO -- coerce
has xrefs => ( is=>'rw', isa=>'ArrayRef[Str]'); # TODO -- make these nodes?
has alt_ids => ( is=>'rw', isa=>'ArrayRef[Str]'); 
has is_anonymous => ( is=>'rw', isa=>'Bool'); 
has comment => ( is=>'rw', isa=>'Str');  # TODO - multivalued?
has in_subsets => ( is=>'rw', isa=>'ArrayRef[GOBO::Node]'); 


sub add_xrefs {
    my $self = shift;
    $self->xrefs([]) unless $self->xrefs;
    foreach (@_) {
        push(@{$self->xrefs},ref($_) ? @$_ : $_);
    }
    $self->_make_xrefs_unique();
    return;
}

sub _make_xrefs_unique {
    my $self = shift;
    my $xrefs = $self->xrefs;
    my %xref_h = map { ($_ => $_) } @$xrefs;
    $self->xrefs([values %xref_h]);
    return;
}

sub date_compact {
    my $self = shift;
    my $date = $self->date;
    if ($date) {
        return sprintf("%04d%02d%02d",$date->year(),$date->month(),$date->day());
    }
}

1;

