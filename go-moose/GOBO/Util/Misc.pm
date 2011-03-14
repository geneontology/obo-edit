package GOBO::Util::Misc;

use Moose;
use Data::Dumper;
use Scalar::Util qw/blessed reftype/;

=head2 read_term_file

Read in a file (stored in $opt{termlist}) and extract anything term-like from it.

Input:  hash, containing
          termlist    => /path/to/file ## input file
          verbose     => 1             ## verbose mode
          term_regexp => qr/.../       ## regexp representing IDs to extract

Output: hashref of term IDs

=cut

sub read_term_file {
    my $sub_h;    # we'll store the data in here
    my %opt = (@_);

    # see if we have an OBO file...
    if ( $opt{termlist} =~ /\.obo$/ )
    {             # looks like it! read in the file and get the term nodes
        ## read in the OBO file and quickly pull out the slim terms
        my $regexp;
        if ( $opt{term_regexp} ) {
            $regexp = $opt{term_regexp};
        }
        {
            local $/ = "\n[";
            open( IN, '<' . $opt{termlist} )
                or die "The file "
                . $opt{termlist}
                . " could not be opened: $!\nDying";
            print "Loading " . $opt{termlist} . "...\n" if $opt{verbose};
            while (<IN>) {
                if ( /^Term].*?^id: .+$/sm && /^id: *(\S+)$/m ) {
                    if ($regexp) {
                        my $id = $1;
                        $sub_h->{$id}++ if $id =~ /$regexp/;
                        next;
                    }
                    $sub_h->{$1}++;
                }
            }
            print "Finished loading ontology.\n" if $opt{verbose};
            close(IN);
        }
    }
    else {    # this is a file of unknown origin
        {
            local $/ = "\n";
            open( IN, '<' . $opt{termlist} )
                or die "The file "
                . $opt{termlist}
                . " could not be opened: $!\nDying";
            print "Loading " . $opt{termlist} . "...\n" if $opt{verbose};
            my $regexp = $opt{term_regexp} || qr/^\s*\S+/;
            while (<IN>) {
                if (/($regexp)/) {
                    my $x = $1;
                    $x =~ s/^\s*//;
                    $x =~ s/\s*$//;
                    $sub_h->{$x}++;
                }
            }
            print "Finished loading term file.\n" if $opt{verbose};
            close(IN);
        }
    }

    if ( !$sub_h ) {
        die "Could not find any terms in the file "
            . $opt{termlist}
            . ". Dying";
    }

    print STDERR "Found subset terms: "
        . join( ", ", sort keys %$sub_h ) . "\n"
        if $opt{verbose};

    return $sub_h;

}

=head2 compare_objects

Simplistic comparison of objects

input:  obj 1, obj 2
output: 1 if they are the same; 0 if they are not

=cut

sub compare_objects {
    return __compare_objects(@_);
=cut
    my ( $obj_1, $obj_2 ) = (@_);
    return 0 unless ref $obj_1 eq ref $obj_2;

#	print STDERR "obj_1: " . Dumper($obj_1) . "obj_2: " . Dumper($obj_2) . "\n\n";

    # if they both have IDs, do a quick comparison...
    if (   $obj_1->isa('GOBO::Base')
        && $obj_1->can('id')
        && defined $obj_1->id
        && defined $obj_2->id )
    {
        if ( $obj_1->id eq $obj_2->id ) {
            return 1;
        }
        else {
            return 0;
        }
    }

    ## get the object attributes and compare them
    my $meta = Class::MOP::Class->initialize( ref($obj_1) );
    foreach my $a ( $meta->get_all_attributes ) {

   #		print STDERR "$a = " . $a->name . "; accessor = " . $a->accessor . "\n";
        my $acc       = $a->accessor;
        my $obj_1_val = $obj_1->$acc;
        my $obj_2_val = $obj_2->$acc;

        ## go on to the next characteristic if neither has this one
        next if !defined $obj_1_val && !defined $obj_2_val;

        ## fail if obj_1 has the attribute but obj_2 doesn't, or vice versa
        return 0
            if ( ( defined $obj_1_val && !defined $obj_2_val )
            || ( !defined $obj_1_val && defined $obj_2_val ) );

        ## ok, both have this attribute
        ## if both are scalars and they're not identical, fail
        if ( !ref $obj_1_val && !ref $obj_2_val ) {
            return 0 if $obj_1_val ne $obj_2_val;
            next;
        }

        if (   ref $obj_1_val
            && $obj_1_val->can('id')
            && defined $obj_1_val->id
            && defined $obj_2_val->id )
        {
            return 0 if $obj_1_val->id ne $obj_2_val->id;
        }
        ## more tests...?
    }
    ## maybe they are the same...!
    return 1;
=cut
}

sub __compare_objects {
	my ( $o1, $o2 ) = @_;

	return if !$o1 or !$o2;

	if (! ref $o1 && ! ref $o2)
	{	#warn "Running scalar tests\n";
		return if $o1 ne $o2;
		return 1;
	}

	## different type of references
	return if reftype($o1) ne reftype($o2);

	if ( blessed($o1) and ref($o1) =~ /^GOBO::/ )
	{	#warn "Running GOBO::Base tests";

		if ( $o1->can('id') and $o2->can('id')
			and defined($o1->id) and defined($o2->id) )
		{	#warn "Checking IDs...\n";
			return 1 if $o1->id eq $o2->id;
		}

		#warn "Did not perform ID tests. Checking attributes...\n";

		## get the object attributes and compare them
		ATTR:
		foreach my $attr ( $o1->meta->get_all_attributes ) {
			my $acc		  = $attr->accessor;
			my $obj_1_val = $o1->$acc;
			my $obj_2_val = $o2->$acc;

			next ATTR if ! defined($obj_1_val) && ! defined($obj_2_val);
			#warn "attr: " . $attr->name . "; obj1: $obj_1_val; obj2: $obj_2_val\n";
			next ATTR if __compare_objects( $obj_1_val, $obj_2_val );
			return;
		}
		return 1;
	}
	elsif ( reftype($o1) eq 'ARRAY' )
	{	#warn "Running array tests\n";
		return if scalar @$o1 ne scalar @$o2;
		for my $i ( 0 .. $#$o1 ) {
			return unless __compare_objects( $o1->[$i], $o2->[$i] );
		}
		return 1;
	}
	elsif ( ref $o1 eq 'HASH' ) {
		#warn "Running hash tests";
		return if scalar keys %$o1 ne scalar keys %$o2;
		return if join( "\0", sort keys %$o1 ) ne join( "\0", sort keys %$o2 );
		foreach my $key ( keys %$o1 ) {
			return unless __compare_objects( $o1->{$key}, $o2->{$key} );
		}
		return 1;
	}
	## other tests...?
	#return 1;
}

__PACKAGE__->meta->make_immutable;

1;
