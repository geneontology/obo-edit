package GOBO::Util::Misc;

use Moose;
use Data::Dumper;
use Class::MOP;


=head2 read_term_file

Read in a file (stored in $opt->{termlist}) and extract anything term-like from it.

Input:  hash, containing
          termlist    => /path/to/file ## input file
          verbose     => 1             ## verbose mode
          term_regexp => qr/.../       ## regexp representing IDs to extract

Output: arrayref of term IDs

=cut

sub read_term_file {
	my $sub_h;  # we'll store the data in here
	my $opt = shift;

	# see if we have an OBO file...
	if ($opt->{termlist} =~ /\.obo$/)
	{	# looks like it! read in the file and get the term nodes
		## read in the OBO file and quickly pull out the slim terms
		my $regexp;
		if ($opt->{term_regexp})
		{	$regexp = $opt->{term_regexp};
		}
		{	local $/ = "\n[";
			open(IN, '<'.$opt->{termlist}) or die "The file ".$opt->{termlist}." could not be opened: $!\nDying";
			print "Loading " . $opt->{termlist} . "...\n" if $opt->{verbose};
			while (<IN>)
			{	if (/^Term].*?^id: .+$/sm && /^id: *(\S+)$/m)
				{	if ($regexp)
					{	my $id = $1;
						$sub_h->{$id}++ if $id =~ /$regexp/;
						next;
					}
					$sub_h->{$1}++;
				}
			}
			print "Finished loading ontology.\n" if $opt->{verbose};
			close(IN);
		}
	}
	else
	{	# this is a file of unknown origin
		{	local $/ = "\n";
			open(IN, '<'.$opt->{termlist}) or die "The file ".$opt->{termlist}." could not be opened: $!\nDying";
			print "Loading " . $opt->{termlist} . "...\n" if $opt->{verbose};
			my $regexp = $opt->{term_regexp} || qr/^\s*\S+/;
			while (<IN>)
			{	if (/($regexp)/)
				{	my $x = $1;
					$x =~ s/^\s*//;
					$x =~ s/\s*$//;
					$sub_h->{$x}++;
				}
			}
			print "Finished loading term file.\n" if $opt->{verbose};
			close(IN);
		}
	}

	if (! $sub_h)
	{	die "Could not find any terms in the file " . $opt->{termlist} . ". Dying";
	}

	print STDERR "Found subset terms: " . join(", ", keys %$sub_h) . "\n" if $opt->{verbose};

	return $sub_h;

}


=head2 compare_objects

Simplistic comparison of objects

input:  obj 1, obj 2
output: 1 if they are the same; 0 if they are not

=cut

sub compare_objects {
	return __compare_objects(@_);
	my ($obj_1, $obj_2) = (@_);
	return 0 unless ref $obj_1 eq ref $obj_2;

	print STDERR "obj_1: " . Dumper($obj_1) . "obj_2: " . Dumper($obj_2) . "\n\n";

	# if they both have IDs, do a quick comparison...
	if ($obj_1->isa('GOBO::Base') && $obj_1->can('id') && defined $obj_1->id && defined $obj_2->id)
	{	if ($obj_1->id eq $obj_2->id)
		{	return 1;
		}
		else
		{	return 0;
		}
	}

	## get the object attributes and compare them
	my $meta = Class::MOP::Class->initialize( ref($obj_1) );
	foreach my $a ($meta->get_all_attributes) {
		print STDERR "$a = " . $a->name . "; accessor = " . $a->accessor . "\n";
		my $acc = $a->accessor;
		my $obj_1_val = $obj_1->$acc;
		my $obj_2_val = $obj_2->$acc;

		## go on to the next characteristic if neither has this one
		next if ! defined $obj_1_val && ! defined $obj_2_val;

		## fail if obj_1 has the attribute but obj_2 doesn't, or vice versa
		return 0 if ( (defined $obj_1_val && ! defined $obj_2_val) || (! defined $obj_1_val && defined $obj_2_val) );

		## ok, both have this attribute
		## if both are scalars and they're not identical, fail
		if (! ref $obj_1_val && ! ref $obj_2_val)
		{	return 0 if $obj_1_val ne $obj_2_val;
			next;
		}

		if (ref $obj_1_val && $obj_1_val->can('id') && defined $obj_1_val->id && defined $obj_2_val->id)
		{	return 0 if $obj_1_val->id ne $obj_2_val->id;
		}
		## more tests...?
	}
	## maybe they are the same...!
	return 1;
}


sub __compare_objects {
	my ($o1, $o2) = (shift, shift);
	return 1 if ! defined $o1 && ! defined $o2;
	## one is defined, one isn't
	return 0 if ! defined $o1 || ! defined $o2;
	## two scalars, unequal
	return 0 if ! ref $o1 && ! ref $o2 && $o1 ne $o2;
	## different object types
	return 0 if ref $o1 ne ref $o2;
	if (ref $o1 eq 'ARRAY')
	{	return 0 if scalar @$o1 ne scalar @$o2;
		my $i = 0;
		while ($i < scalar @$o1)
		{	my $result = __compare_objects( $o1->[$i], $o2->[$i] );
			return 0 unless $result;
		}
		return 1;
	}
	elsif (ref $o1 eq 'HASH')
	{	return 0 if scalar keys %$o1 ne scalar keys %$o2;
		return 0 if join("\0", sort keys %$o1) ne join("\0", sort keys %$o2);
		foreach (keys %$o1)
		{	my $result = __compare_objects( $o1->{$_}, $o2->{$_} );
			return 0 unless $result;
		}
		return 1;
	}
	# if they both have IDs, do a quick comparison...
	elsif ($o1->isa('GOBO::Base'))
	{	if($o1->can('id') && defined $o1->id && defined $o2->id)
		{	if ($o1->id eq $o2->id)
			{	return 1;
			}
			else
			{	return 0;
			}
		}

		## get the object attributes and compare them
		my $meta = Class::MOP::Class->initialize( ref($o1) );
		foreach my $a ($meta->get_all_attributes) {
			print STDERR "$a = " . $a->name . "; accessor = " . $a->accessor . "\n";
			my $acc = $a->accessor;
			my $obj_1_val = $o1->$acc;
			my $obj_2_val = $o2->$acc;

			my $result = __compare_objects( $obj_1_val, $obj_2_val );
			next if $result;
			return 0;
		}
	}
	## other tests...?
	return 1;
}

1;
