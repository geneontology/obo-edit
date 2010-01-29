package GOBO::Parsers::QuickGAFParser;
use Moose;
extends 'GOBO::Parsers::GAFParser';

use Data::Dumper;

override '_parse' => sub {
	my $self = shift;
	return 0 unless $self->has_fh;

	# make sure we've checked our parser options
	$self->check_options;
	$self->parse_header if ! $self->parsed_header;
	return $self->parse_body;
};

override 'parse_body' => sub {
	my $self = shift;

#print STDERR "starting to parse the body... woohoo!\n";

	my $data;
	my @errs;

	while (my $line = $self->next_line) {
		next if $line =~ /^!/;
		chomp $line;
		my @arr = split("\t", $line);
		## add an extra array item to make it easier to work out which column is which
		unshift @arr, " ";
		#	association ID
		my $a_id = _create_assoc_id(\@arr);
#		print STDERR "arr[5]: $arr[5]; a_id: $a_id\n" if $ENV{VERBOSE};
		if (grep { $a_id eq $_ } @{$data->{by_t}{$arr[5]}}) # data already exists
		{	warn "Annotation $a_id to $arr[5] already exists!";
			next;
		}

		## undef the term and ontology abbrev, store the array data
		my $t_id = $arr[5];
#		$arr[5] = '';
#		$arr[9] = '';

		if ($data->{by_a}{$a_id})
		{	## if the assoc ID exists, we're in the strange situation of already
			## having v. similar assoc info
			if ( join("\t", @arr) ne join("\t", @{$data->{by_a}{$a_id}{arr}}) )
			{	warn "Error: association ID $a_id already exists with different association data. Keeping old data.";
				if ($ENV{VERBOSE})
				{	my $x;
					my @old = @{$data->{by_a}{$a_id}{arr}};
					my $errs;
					for ($x = 1; $x < 20; $x++)
					{	next if $x == 5 || $x == 9;  ## ignore term info
						if (! defined $arr[$x] && ! defined $old[$x] )
						{	next;
						}
						if (defined $arr[$x] && defined $old[$x] && $arr[$x] eq $old[$x])
						{	next;
						}
						push @$errs, [ $x, $old[$x], $arr[$x] ];
					}
					print STDERR "OLD: " . join("\t", map { $_->[1] } @$errs) . "\n" .
					"NEW: " . join("\t", map { $_->[2] } @$errs) . "\n\n" if $ENV{VERBOSE};
				}
			}
		}
		push @{$data->{by_a}{$a_id}{terms}}, $t_id;
		$data->{by_a}{$a_id}{arr} = [@arr] if ! $data->{by_a}{$a_id}{arr};
		push @{$data->{by_t}{$t_id}}, $a_id;
	}

	if (@errs)
	{	warn "The following (potentially) duplicate annotations were found:\n"
		. join("\n", map {
			my ($t, $a_id) = split("---", $_, 2);
			"$t, $a_id: " . Dumper($data->{by_t}{$t}{$a_id});
		} @errs )
		. "\n\n";
	}

#	print STDERR "data looks like this: " . Dumper($data) . "\n";

	return $data;
};




=cut

we should ensure that we don't get any duplicated annotations by keeping
a tally of what we've seen as we go along
to generate a unique "ID" for each annotation, we should save the following:

1  - DB
2  - DB_Object_ID
4  - Qualifier
6  - DB:Reference (|DB:Reference)
7  - Evidence code
8  - With (or) From
13 - taxon(|taxon)
14 - Date
15 - Assigned_by
16 - Annotation XPs

col 5, GO ID, is the data we want for slimming purposes

=cut

sub _create_assoc_id {
	my $arr = shift;
	my $str = $arr->[1];
	foreach (2, 4, 6, 7, 8, 13..$#$arr)
	{	$str .= "\0" . ($arr->[$_] || "");
	}
	return $str;
}


=head1 NAME

GOBO::Parsers::QuickGAFParser

=head1 SYNOPSIS

 my $parser = new GOBO::Parsers::QuickGAFParser( file => '/path/to/my_file' );
 my $data = $parser->parse;

 ## Association data is stored in a hash keyed by association ID
 foreach my $assoc_id (keys %{$data->{by_a}})
 {   my $assoc_data = $data->{by_a}{$assoc_id};
     ## do stuff with the original array data...
     foreach my $arr_item ( @{$assoc_data->{arr}} )
     {
      ...
     }
 }

=head1 DESCRIPTION

For quick 'n' dirty parsing of a GAF file where you don't want to create full
annotation objects (for whatever reason) and can make do with the association
data stored as an array. A unique ID is generated for each annotation using
the annotation data.

=cut



1;
