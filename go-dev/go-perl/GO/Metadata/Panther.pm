package GO::Metadata::Panther;
use strict;
use warnings;
use Exporter;
use Memoize;
use List::Util qw/sum first/;
use Data::Dumper;
use Carp;

use base qw/GO::Metadata::UniProt::Species Exporter/;
our @EXPORT_OK = qw/panther_codes panther_all valid_panther_codes/;


=head1 NAME

GO::Metadata::Panther - Species info for data used by Panther Clusters

=head1 SYNOPSIS

 use GO::Metadata::Panther;
 my $s = GO::Metadata::Panther->code('YEAST');

=head1 DESCRIPTION

Inherits functions from L<GO::Metadata::UniProt::Species>.

Accesses information related to species in the Panther F<seq2pthr.gz>
file.  This file can be fetched from:
L<ftp://ftp.pantherdb.org/genome/pthr7.0/>

=cut

# Information needed but not provided by UniProt's speclist.txt file.

our %species =
  (
   #
   # A
   #

   ANOGA => { prefer => [ 'Gene' ] },
   ARATH => { id_filter => sub {
		  my ($dbname, $key) = (shift, shift);
		  if ($dbname eq 'gene') {
		      return ('TAIR', "locus:$key");
		  }
		  return ($dbname, $key);
	      }
	    },
   AQUAE => {},
   ASHGO => {},

   #
   # B
   #

   BACSU => {},
   BACTN => {},
   BOVIN => { prefer => [ 'UniProtKB',  'Ensembl' ] },
   BRAJA => {},

   #
   # C
   #

   CAEBR => {},
   CAEEL => { prefer => [ 'WB' ] },
   CANFA => {},
   CHLTA => {},
   CHLRE => {},
   CHLAA => {},
   CIOIN => {},

   #
   # D
   #

   DANRE => { prefer => [ 'ZFIN' ] },
   DEIRA => {},
   DICDI => {},
   DROME => { prefer => [ 'FB' ] },

   #
   # E
   #

   EMENI => {},
   ENTHI => {},
   ECOLI => { also_node => [ 511145 ],
	      prefer    => [ 'EcoCyc' ] },

   #
   # G
   #

   CHICK => {},
   GEOSL => {},
   GLOVI => { also_node => [ 251221 ] },


   #
   # H
   #

   HUMAN => { prefer => [ 'UniProtKB', 'ENSEMBL' ] },

   #
   # L
   #

   LEIMA => { also_node => [ 347515 ] },
   LEPIN => {},

   #
   # M
   #

   MACMU => { prefer => [ 'UniProtKB', 'ENSEMBL' ] },
   METAC => {},
   MONDO => {},
   MOUSE => { prefer => [ 'MGI' ] },

   #
   # N
   #

   NEUCR => {},

   #
   # O
   #

   ORNAN => {},
   ORYSJ => {},

   #
   # P
   #

   PANTR => {},
   PLAYO => {},
   PSEA7 => {},

   #
   # R
   #

   RAT => {  prefer => [ 'RGD', 'UniProtKB' ] },

   #
   # S
   #

   YEAST => {},
   SCHPO => {},
   STRCO => {},
   STRPU => {},
   SULSO => {},

   #
   # T
   #

   FUGRU => { is => 'TAKRU' },
   TAKRU => { was => 'FUGRU' },
   TETTH => { also_node => [ 312017 ] },
   THEMA => {},

   #
   # X
   #

   XENTR => { prefer => [ 'UniProtKB', 'ENSEMBL' ] },
);

=over

=head2 Exportable Subroutines

=item GO::Metadata::Panther::codes()

Returns the list of UniProt species codes that are used in Panther clusters.

=cut
sub codes{
    warn "Rename this function panther_codes";

    return map {
	defined $species{$_}->{is} ? () : $_;
    } keys %species;
}


=item GO::Metadata::Panther::all()

Returns a list of C<GO::Metadata::Panther> objects that are used in Panther clusters.

=cut
sub all{
    warn "Rename this function panther_all";

    my $c = shift;
    return $c->new(codes());
}

=item GO::Metadata::Panther::valid_codes(...)

Returns a true value in every argument is a UniProt species code used
in Panther cluster.  Otherwise returns false.

=cut
sub valid_codes{
    for my $code (@_) {
	return undef if (!exists $species{$code});
    }
    return '1';
}

=back

=head2 OO Function

=item GO::Metadata::Panther-E<gt>new(...);

This basically hands things off to L<GO::Metadata::UniProt::Species>'s
new function.   Populates that with other Panther/GO specific
information, and does some error correction.

=cut
our %_new_cache;
sub new{
    my $c = shift;

    my @have;
    my @all = map {
	if ($_new_cache{$_}) {
	    push @have, $_new_cache{$_};
	    ();
	} else {
	    $_;
	}
    } @_;

    ##########
    # Fix up also_node entries (see ECOLI)
    @all = map {
	my $all = $_;
	my $out = $all;
	if ($all =~ m/^\d+$/) {
	  BLA:
	    for my $code (keys %species) {
		for my $node (@{ $species{$code}->{also_node} }) {
		    if ($all eq $node) {
			$out = $code;
			last BLA;
		    }
		}
	    }
	}
	$out;
    } @all;
    # This bugs me
    ##########

    @all = $c->SUPER::new(map {
	if ($species{$_} && $species{$_}->{is}) {
	    warn "$_ -> $species{$_}->{is}";
	    $species{$_}->{is};
	} else {
	    $_;
	}
    } @all) if (scalar @all);

    for (@all) {
	if ($species{$_->code}) {
	    while (my ($k,$v) = each %{ $species{$_->code} }) {
		$_->{$k} = $v;
	    }
	} else {
	    warn $_->code . ' Not a Panther family.';
	}
    }

    for my $all (@all) {
	$_new_cache{$all->{node}} = $all;
	$_new_cache{$all->{code}} = $all;
    }
    push @all, @have;

    return undef   if (0 == scalar @all);
    return $all[0] if (1 == scalar @all);
    return @all;
}


=over

=item $s->ncbi_taxa_ids()

Returns the list of NCBI taxa identifiers associated with the UniProt
species code.  In a perfect word this will only every return one
value.  In any case, the first value will be the actual numeric
identifier associated.

=cut
sub ncbi_ids{
    my $s = shift;
    my @out = ($s->{node});
    push @out, @{ $s->{also_node} } if ($s->{also_node});
    return @out;
}

=item $s->prefers()

Returns a list of id types (generally to be populated in
C<dbxref.xref_dbname>) in order of preference of use.  If a null list,
we have never encountered a conflict that needed resolving.

=cut
sub prefers{
    my $s = shift;
    if ($s->{prefer}) {
	return @{ $s->{prefer} };
    }
    return qw/UniProtKB/;
}

sub id_filter{
    my $s = shift;

    if ($s->{id_filter}) {
	return ${ $s->{id_filter} }(@_);
    }
    @_;
}


=back

=head2 SEE ALSO

L<GO::Metadata::UniProt::Species>

=head2 AUTHOR

Sven Heinicke E<lt>sven@genomics.princeton.edu</gt>

=cut

1;
