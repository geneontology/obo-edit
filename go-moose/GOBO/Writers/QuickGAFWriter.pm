package GOBO::Writers::QuickGAFWriter;
use Moose;
use strict;
extends 'GOBO::Writers::GAFWriter';
use GOBO::Graph;

use Data::Dumper;

has 'header_data' => (is=>'rw', isa=>'ArrayRef[Str]', predicate=>'has_header_data');
has 'assoc_array' => (is=>'rw', isa=>'ArrayRef', predicate=>'has_assoc_array');


override 'write_header' => sub {
	my $self = shift;
	if ($self->has_header_data)
	{	my $fh = $self->fh;
		map { print $fh "! " . $_ . "\n" } @{$self->header_data};
		print $fh "!\n";
	}
	return;
};

override 'write_body' => sub {
	my $self = shift;

	## Group assocs by target ID
	## put all the assocs for each term together so we can then sort them.
	my %assoc_h;
	map {
		my $a = $_;
		my $data = $a->node->data_arr;
		map { $_->{data}[5] = $a->target->id; $_->{data}[9] = GOBO::Writers::GAFWriter::_aspect($a->target) } @$data;
		push @{$assoc_h{ $a->target->id }}, $_ foreach @{$_->node->data_arr};
	}
	grep { $_->isa('GOBO::Annotation') } @{$self->assoc_array};

	foreach (sort keys %assoc_h)
	{	## retrieve the array data...
		foreach my $arr (map { $_->{data} } sort { $a->{id} cmp $b->{id} } @{$assoc_h{$_}})
		{	$self->printrow([ @$arr[1..$#$arr] ]);
		}
	}

};

1;
