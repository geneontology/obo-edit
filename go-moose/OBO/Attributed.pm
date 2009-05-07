=head1 NAME

OBO::Attribute

=head1 SYNOPSIS

=head1 DESCRIPTION

A role for any kind of entity that can be attributed to some source (annotated). Here 'entity' includes OBO::Statement objects

=head2 TBD

Is this over-abstraction? This could be simply mixed in with Statement

=cut

package OBO::Attributed;
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

has source => ( is=>'rw', isa=>'OBO::Node', coerce=>1);
has provenance => ( is=>'rw', isa=>'OBO::Node', coerce=>1);
has date => ( is=>'rw', isa=>'Date', coerce=>1); # TODO -- coerce
has xrefs => ( is=>'rw', isa=>'ArrayRef[Str]'); # TODO -- make these nodes?
has alt_ids => ( is=>'rw', isa=>'ArrayRef[Str]'); 
has is_anonymous => ( is=>'rw', isa=>'Bool'); 
has comment => ( is=>'rw', isa=>'Str');  # TODO - multivalued?
has in_subsets => ( is=>'rw', isa=>'ArrayRef[OBO::Node]'); 


sub add_xrefs {
    my $self = shift;
    $self->xrefs([]) unless $self->xrefs;
    push(@{$self->xrefs},@_);
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

