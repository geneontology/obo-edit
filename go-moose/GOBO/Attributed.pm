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
use Moose::Util::TypeConstraints;
use GOBO::Types;
use DateTime::Format::ISO8601;
require DateTime;

has version => ( is=>'rw', isa=>'Str');
has source => ( is=>'rw', isa=>'GOBO::Node', coerce=>1);
has provenance => ( is=>'rw', isa=>'GOBO::Node', coerce=>1);
has date => ( is=>'rw', isa=>'GOBO::Attributed::Date', coerce=>1);
has alt_ids => ( is=>'rw', isa=>'ArrayRef[Str]');
has is_anonymous => ( is=>'rw', isa=>'Bool');
has comment => ( is=>'rw', isa=>'Str');  # TODO - multivalued?
has subsets => ( is=>'rw', isa=>'ArrayRef[GOBO::Node]');
has property_value_map => ( is=>'rw', isa=>'HashRef');
has created_by => ( is=>'rw', isa=>'Str');
has creation_date => ( is=>'rw', isa=>'GOBO::Attributed::Date', coerce=>1);

has xref_h => ( is=>'rw', isa=>'HashRef[GOBO::Node]'); # TODO -- make these nodes?

sub xrefs {
	my $self = shift;
	if (@_)
	{	$self->add_xrefs(@_);
	}
	if ($self->xref_h && values %{$self->xref_h})
	{	return [ values %{$self->xref_h} ];
	}
	return [];
}

sub add_xrefs {
	my $self = shift;
	$self->xref_h({}) unless $self->xref_h;
	## convert any arrayrefs
	my @xrefs = map { if (ref $_ && ref $_ eq 'ARRAY') { @$_ } else { $_ } } @_;
	foreach (@xrefs)
	{	if (! ref $_)
		{	my $r = new GOBO::Node(id => $_);
			$self->xref_h({ %{$self->xref_h}, $r->id => $r });
		}
		elsif (ref $_ eq 'HASH')
		{	my $r = new GOBO::Node($_);
			$self->xref_h({ %{$self->xref_h}, $r->id => $r });
		}
		elsif ($_->isa('GOBO::Node'))
		{	$self->xref_h({ %{$self->xref_h}, $_->id => $_ });
		}
		else
		{	warn "Help! Don't know what to do with potential xref $_!";
		}
	}
	return;
}

#sub _make_xrefs_unique {
#    my $self = shift;
#    my $xrefs = $self->xrefs;
#    my %xref_h = map { ($_ => $_) } @$xrefs;
#    $self->xrefs([values %xref_h]);
#    return;
#}

sub date_compact {
    my $self = shift;
    my $date = $self->date;
    if ($date) {
        return sprintf("%04d%02d%02d",$date->year(),$date->month(),$date->day());
    }
}

sub add_subsets {
    my $self = shift;
    $self->subsets([]) unless $self->subsets;
    foreach (@_) {
        push(@{$self->subsets},ref($_) && ref($_) eq 'ARRAY' ? @$_ : $_);
    }
    return;
}

sub add_alt_ids {
    my $self = shift;
    $self->alt_ids([]) unless $self->alt_ids;
    foreach (@_) {
        push(@{$self->alt_ids},ref($_) && ref($_) eq 'ARRAY' ? @$_ : $_);
    }
    return;
}

sub set_property_value {
    my $self = shift;
    my ($p,$v) = @_;
    $self->property_value_map({}) unless $self->property_value_map;
    $self->property_value_map->{"$p"} = $v;
    return;
}

sub add_property_value {
    my $self = shift;
    my (%args) = @_;
#    print STDERR "Args: p: $args{prop}, v: $args{value}\n";
    $self->property_value_map({}) unless $self->property_value_map;
    push(@{$self->property_value_map->{"$args{prop}"}}, $args{value});
    return;
}

sub get_property_value {
    my $self = shift;
    my ($p) = @_;
    my $map = $self->property_value_map || {};
    return $map->{"$p"};
}

1;

