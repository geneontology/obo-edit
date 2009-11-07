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
		{	local $/ = "\n[";
			open(IN, '<'.$opt->{termlist}) or die "The file ".$opt->{termlist}." could not be opened: $!\nDying";
			print "Loading " . $opt->{termlist} . "...\n" if $opt->{verbose};
			while (<IN>)
			{	if (/^Term].*?^id: .+$/sm && /^id: ?(\S+)$/m)
				{	$sub_h->{$1}++;
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
			my $regexp = $opt->{term_regexp} || qr/^\s*\S+[\s$]/;
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
	my ($obj_1, $obj_2) = (@_);
	return 0 unless ref $obj_1 eq ref $obj_2;

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
	#	print STDERR "$a = " . $a->name . "; accessor = " . $a->accessor . "\n";
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

		if ($obj_1_val->isa('GOBO::Base') && $obj_1_val->can('id') && defined $obj_1_val->id && defined $obj_2_val->id)
		{	return 0 if $obj_1_val->id ne $obj_2_val->id;
		}
		## more tests...?
	}
	## maybe they are the same...!
	return 1;
}


1;
