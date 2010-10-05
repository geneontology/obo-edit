package GO::MatchID;
use warnings;
use strict;
use Data::Dumper;
use Carp;
use Memoize;
use List::Util qw/first/;
use Text::Wrap;

use GO::Metadata::Panther;

our $dbh;
our $debug;
our $quiet;

sub _select_one_row{
    my $sth = shift;
    $sth->execute(@_);
    my ($out) = $sth->fetchall_arrayref;
    my $rows = $sth->rows();

    if (0 == $rows) {
	return undef;
    } elsif (1 != $rows) {
	croak wrap('', ' ', $sth->{Statement}, ': (', join(', ', @_),
		   ') returned', $sth->rows(), 'rows.)');
    }
    return $out->[0];
}

memoize('_ncbi2species');
our $species_id_sth;
sub _ncbi2species{
    my $ncbi_taxa_id = shift;

    if (!$species_id_sth) {
	$species_id_sth = $dbh->prepare(<<SQL);
SELECT id FROM species WHERE ncbi_taxa_id=?
SQL
    }
    my $id = _select_one_row($species_id_sth, $ncbi_taxa_id);
    if (!defined $id) {
	die "Unable to find spices_id for NCBI taxa ID $ncbi_taxa_id";
    }
    $id = $id->[0];
    warn "ncbi2species $ncbi_taxa_id => $id" if ($debug);
    return $id;
}

sub species_metadata{
    my $s = shift;
    if (!$s->{species_metadata}) {
	$s->{species_metadata} =
	  GO::Metadata::Panther->new($s->{ncbi_taxa_id});
    }
    return $s->{species_metadata};
}

=item $s->species_id()

Returns the C<species.id> of the GO database.

=cut
sub species_id{
    my $s = shift;
    if (!$s->{species_id}) {
	if ($s->{ncbi_taxa_id}) {
	    $s->{species_id} = _ncbi2species($s->{ncbi_taxa_id});
	} else {
	    carp 'Unable to figure out the species_id';
	    # Use $s->species_metadata() to try to fill?
	}
    }
    return $s->{species_id}
}

=item $s->species_id()

Return a list of possible C<species.id> entries that this object might
be associated with.  We hope it is a list of one, but it isn't always.

=cut
sub species_ids{
    my $s = shift;
    my $md = $s->species_metadata();
    my @id = $md->ncbi_ids();
    # Test/set $s->{species_id} here?
    return map { _ncbi2species($_) } @id;
}

sub new{
    my $c = shift;
    my $s = bless (scalar(@_) ? { @_ } : {}, $c);

    if ($s->{panther_id}) {
	($s->{uniprot_species_code}, my @ids) = split(m/\|/, $s->{panther_id});
	$s->try(@ids);
    }

    return $s;
}

sub _scalar{
    my $s   = shift;
    my $key = shift;
    my $new = shift;

    if ($new) {
	my $old = $s->{$key};
	$s->{$key} = $new;
	return $old;
    }
    return $s->{$key};
}

sub _hash{
    my $s      = shift;
    my $key    = shift;
    my $filter = pop if (ref $_[-1]);

    while (@_) {
	my $kv = shift @_;
	die 'Trivial kv' if (_trivial($kv));
	if ($kv !~ m/:/) {
	    warn "Skipping '$kv'" unless ($quiet);
	    next;
	}
	my ($k, $v) = split(m/:/, $kv, 2);
	($k, $v) = &$filter($k, $v) if ($filter);
	$s->{$key}->{$k} = $v;
    }
    return $s->{$key};
}

sub _trivial{
    my $test = shift;

    return '1' if (!defined $test);
    $test =~ s/\s+$//;
    $test =~ s/^\s+//;
    return '1' if ($test eq '');
    return undef;
}

sub description{
    return shift()->_scalar('desc', shift);
}

sub try{
    my $s = shift;
    my $f = $s->species_metadata->{id_filter};

    return $s->_hash('try', @_, $f ? $f : ());
}

sub tagval{
    return shift()->_hash('tagval', @_);
}

##########

our @guess_gp = # guess gene_product
  (
   <<SQL,
SELECT gene_product.id AS gene_product_id
,dbxref.id AS dbxref_id
,xref_key
,xref_dbname
FROM dbxref
JOIN gene_product ON(dbxref.id=gene_product.dbxref_id)
WHERE xref_key=? AND species_id=?
SQL

# If we want to use prefers off the gene_product_dbrxef xref_key use
# this.

   <<SQL,
SELECT gene_product.id AS gene_product_id
,dbxref.id AS dbxref_id
,xref_key
,xref_dbname
FROM gene_product
JOIN gene_product_dbxref ON(gene_product.id=gene_product_dbxref.gene_product_id)
JOIN dbxref ON(gene_product_dbxref.dbxref_id=dbxref.id)
WHERE xref_key=? AND species_id=?
SQL


  );

my @guess_dbxref =
  (
   <<SQL,
SELECT id,xref_key,xref_dbname
FROM dbxref
WHERE xref_key=?
AND id NOT IN (SELECT dbxref_id FROM gene_product)
AND id NOT IN (SELECT dbxref_id FROM gene_product_dbxref)
ORDER BY id
SQL
  );

sub guess{
    my $s = shift;
    my $dbxref_p = shift; # Guess dbxref_id if no gene_product found?

    if ($s->{guessed}) {
	return ($s->{guessed}->{gene_product_id}, $s->{guessed}->{dbxref_id});
    }

    if (!ref($guess_gp[0])) {
	@guess_gp = map {
	    $dbh->prepare($_);
	} @guess_gp;
    }


    my @maybe;
    for my $guess (@guess_gp) {
	while (my ($dbname, $key) = each %{ $s->try }) {
	    for my $species_id ($s->species_ids()) {
		$guess->execute($key, $species_id) or die;
		my $matched = $guess->fetchall_arrayref();
		for my $row (0 .. ($guess->rows() - 1)) {
		    my %maybe =
		      (
		       try             => $dbname,
		       gene_product_id => $matched->[$row]->[0],
		       dbxref_id       => $matched->[$row]->[1],
		       xref_key        => $matched->[$row]->[2],
		       xref_dbname     => $matched->[$row]->[3],
		      );
		    push @maybe, \%maybe;
		}
	    }
	}

	my $maybe = scalar @maybe;
	if (1 == $maybe) {
	    $s->{guessed} = $maybe[0];
	} elsif (1 < $maybe) {
	    for my $prefer ($s->species_metadata->prefers()) {
		my $matched = first { $prefer eq $_->{xref_dbname} } @maybe;
		if ($matched) {
		    $s->{guessed} = $matched;
		    last;
		}

	    }

	    if (!$s->{guessed}) {
		local $Data::Dumper::Varname = 'MULT';
		die Dumper $s, \@maybe;
	    }
	}

	if ($s->{guessed}) {
	    warn $s->guessed if ($debug);
	    return ($s->{guessed}->{gene_product_id},
		    $s->{guessed}->{dbxref_id});
	} elsif (0 < scalar(@maybe)) {
	    local $Data::Dumper::Varname = 'MISS';
	    die Dumper $s, \@maybe;
	}
    }

    if (!$dbxref_p) {
	return (undef, undef);
    }


    # If we made it here we didn't find a gene_product.
    ##########
    # Lets seek a matching dbxref entry, for now lets only look for
    # UniProt IDs

    my @try = map {
	(m/\bUniProt(KB)?\b/i) ? $_ : ();
    } keys %{ $s->{try} };

    if (scalar @try) {

	if (!ref($guess_dbxref[0])) {
	    @guess_dbxref = map {
		$dbh->prepare($_);
	    } @guess_dbxref;
	}

	while (@try) {
	    my $xref_dbname = shift @try;
	    my $xref_key = $s->{try}->{$xref_dbname};

	    for my $guess (@guess_dbxref){
		$guess->execute($xref_key);
		my $matched = $guess->fetchall_arrayref();
		next if ($guess->rows == 0);

		my @matched = map {
		    ($_->[2] =~ m/\bUniProt(KB)?\b/i) ? $_ : ();
		} @{ $matched };

		$matched = scalar @matched;
		next if (0 == $matched);
		if (1 < $matched) {
		    warn <<TXT unless ($quiet);
Matched $matched UniProt dbxref entries for $xref_key, using $matched[0]->[2]
TXT
		}

		$s->{guessed} =
		  {
		   try         => $xref_dbname,
		   dbxref_id   => $matched[0]->[0],
		   xref_key    => $matched[0]->[1],
		   xref_dbname => $matched[0]->[2],
		  };

		warn $s->guessed if ($debug);
		return (undef, $matched[0]->[0]);
	    }
	}
    }

    #
    ##########

    warn $s->guessed if ($debug);
    return (undef, undef);
}

sub guessed{
    my $s = shift;
    #$s->guess() if (!$s->{guessed});

    if ($s->{guessed}) {
	my $try = $s->{guessed}->{try};
	$try .= ':' . $s->{try}->{$try};

	if (!$s->{guessed}->{xref_dbname} || !$s->{guessed}->{xref_key}) {
	    die Dumper $s;
	}

	my $got = $s->{guessed}->{xref_dbname} . ':' . $s->{guessed}->{xref_key};

	return $try . ($s->{guessed}->{gene_product_id} ? ' => ' : ' ~> ') . $got;
    }

    my @try;
    while (my ($dbname,$key) = each %{ $s->{try} }) {
	push @try, "$dbname:$key";
    }
    return join('|', @try) . ' => ?';
}

1;
