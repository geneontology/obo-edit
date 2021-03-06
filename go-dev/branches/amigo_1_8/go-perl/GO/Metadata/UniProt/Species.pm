package GO::Metadata::UniProt::Species;
=head1 NAME

GO::Metadata::UniProt::Species - Access data in the F<speclist.txt>
file provided by UniProt

=head1 SYNOPSIS

 use GO::Metadata::UniProt::Species;
 my @yeast = GO::Metadata::Species::UniProt->new(qw/YEAST SCHPO/));
 for my $yeast (@yeast) {
  print $yeast->common_name . ' has the NCBI Taxa id of ' .
    $yeast->ncbi_taxon_id . "\n";
 }

=cut
use warnings;
use strict;
use File::Basename;
use IO::Uncompress::AnyUncompress qw/$AnyUncompressError/;
use List::Util qw/first/;
use Data::Dumper;
use Carp;

#our $SPECIES_LIST=File::Spec->catfile(dirname(__FILE__), 'speclist.txt.gz');
our $SPECIES_LIST=File::Spec->catfile(dirname(__FILE__), 'speclist.txt');
our $debug;

=head1 DESCRIPTION

=over

=item GO::Metadata::Species::UniProt->new(...);

Takes a list of NCBI Taxa ids and/or UniProt Species codes.  If an
item in the list is an integer it will seek for the NCBI Taxa IDs.

If no IDs are found it will return C<undef>.  If only one is found it
will return a scalar.  If many are found it will return a list.

=cut
sub new{
    my $class = shift;

    my %code; # Uniprot Species Codes
    my %node; # NCBI Taxa ID
    while (@_) {
	my $request = shift @_;
	if ($request =~ m/^\d+$/) {
	    $node{$request} = '';
	} else {
	    $code{$request} = '';
	}
    }

    warn "Opening $SPECIES_LIST (" . join(' ', keys(%node), '|', keys(%code)) . ")\n"
      if ($debug);

    my $sl = IO::Uncompress::AnyUncompress->new($SPECIES_LIST)
       or die "AnyUncompress failed: $AnyUncompressError\n";

    my @species;
    my $species;

    while (my $line = <$sl>) {
	chomp $line;

	if ($species->{code} &&
	    (($line =~ m/^\w/) || ($line =~ m/^\s*$/))
	   ) {
	    push @species, $species;
	    undef $species;
	}

	if ($line =~ m/^(\w*)\s+([A-Z])\s+(\d+):\s+N=(.*)$/) {
	    my %species =
	      (
	       code    => $1,
	       kingdom => $2,
	       node    => $3,
	       N       => $4,
	      );


	    if ((first {$_ == $species{node}} keys %node) ||
		(first {$_ eq $species{code}} keys %code)) {

		delete $node{$species{node}};
		delete $code{$species{code}};

		$species = \%species;
	    }

	} elsif ($species->{code}                &&
		 ($line =~ m/^\s+([A-Z])=(.*)$/)) {
	    die "Found multiple '$1' entries" if (exists $species->{$1});
	    $species->{$1} = $2;
	}

	if ((0 == scalar keys %code) && (0 == scalar keys %node)) {
	    push @species, $species if ($species->{code});
	    last;
	}
    }

    for (keys %code) {
	carp "Unable to find the UniProt code: $_";
	push @species, { code => $_ };
    }
    for (keys %node) {
	carp "Unable to find the NCBI node: $_";
	push @species, { node => $_ };
    }

    #if ((scalar(keys %code)) || (scalar(keys %node))) {
    #	warn join(' ', 'Skipped:', keys %code, keys %node);
    #}

    for (@species) {
	bless $_, $class;
    }

    return undef       if (0 == scalar @species);
    return $species[0] if (1 == scalar @species);
    return @species;
}



=item $s-E<gt>code

Returns the UniProt code that was used to create the object.

=cut
sub code{
    my $s = shift;
    my $out = $s->{code} || "node:$s->{node}";
    return $out;
}

=item $s-E<gt>ncbi_taxon_id

Returns the NCBI taxonomic ID of the species, or C<undef> if it doesn't have one.

=cut
sub ncbi_taxon_id{
    return shift()->{node};
}

=item $s-E<gt>scientific_name

Returns the scientific name of the organism.  This is ofter more then
genus and species.

=cut
sub scientific_name{
    return shift()->{N};
}

=item $s-E<gt>common_name

Returns the common name of the organism, or C<undef> if it doesn't have one.

=cut
sub common_name{
    return shift()->{C};
}

=item $s-E<gt>synonym

Returns a synonym for the species, or C<undef> if it doesn't have one.

=cut
sub synonym{
    return shift()->{S};
}

=item $s-E<gt>{kingdom}

This contains a single letter for what 'kingdom' it is in:

 'A' for archaea (=archaebacteria),
 'B' for bacteria (=prokaryota or eubacteria),
 'E' for eukaryota (=eukarya),
 'V' for viruses and phages (=viridae).

This will likely change if I ever need to access this information.

=back

=head1 SEE ALSO

L<http://www.uniprot.org/docs/speclist>

=head1 AUTHOR

Sven Heinicke E<lt>sven@genomics.princeton.eduE<gt>

=head1 COPYRIGHT

<See the L<http://www.uniprot.org/help/license> for license and copyright of the
F<speclist.txt> file.

=cut
1;
