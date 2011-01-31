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
our $sensitive;
our $quiet;


# If a query returns more then one row it will croak.  If the query
# returns zero rows, it returns undef.  If the query only returns one
# row, it will return a reference to that row.
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


# Given an NCBI Taxa id, will return the species.id from the GO
# database.
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

=item $s->species_metadata



=cut
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
	    die 'Unable to figure out the species_id';
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
	$s->ids(@ids);
    }

    return $s;
}

# adding a scalar to this object
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
    my $s   = shift;
    my $key = shift;
    my $smd = $s->species_metadata();

    while (@_) {
	my $kv = shift @_;

	die 'Trivial kv' if (_trivial($kv));
	# if ($kv !~ m/:/) {
	#     warn "Skipping '$kv'" unless ($quiet);
	#     next;
	# }
	my ($k, $v) = split(m/:/, $kv, 2);
	if (exists $s->{$key}->{$v}) {
	    warn "Trying to reset $k" if ($debug);
	} else {
	    $s->{$key}->{$k} = $v;
	}
    }
    return $s->{$key};
}

sub tagval{
    return shift()->_hash('tagval', @_);
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
    die "Don't use try!";
}

sub _dbname_filter{
    my $dbname = shift;

    return 'UniProtKB' if ($dbname =~ m/UniProt/i);
    return $dbname;
}


my @skip_ids = qw/ENTREZ/;
sub ids{
    my $s   = shift;
    my $smd = $s->species_metadata();

    while (@_) {
	my @id = $smd->id_filter(split(m/:/, shift @_, 2));
	next if (first {$id[0] eq $_} @skip_ids);
	push @{ $s->{ids} }, \@id;
    }
    return @{ $s->{ids} };
}

# sub have_dbname{
#     my $s = shift;
#     my $dbname = shift;
#     my @id = $s->ids();

#     while (@id) {
# 	my $id = shift @id;
# 	return 1 if ($dbname eq $id->[0]);
#     }
#     return undef;
# }

# sub unique_ids{
#     my $s = shift;
#     my @id = $s->ids;
#     return @id if (1 >= scalar(@id));


#     if (scalar(@id) > 2) {
# 	warn Dumper $s;
# 	die "We don't yet suppert more then 2 ids, sorry";
#     }

#     if ($id[0]->[1] eq $id[1]->[1]) {
# 	my @p = $s->species_metadata()->prefers();

# 	for my $id (@id) {
# 	    if (first { $id->[0] eq $_ } @p) {
# 		local $Data::Dumper::Varname = 'BLA';
# 		return ( $id );
# 	    }
# 	}
#     } else {
# 	return @id;
#     }
#     local $Data::Dumper::Varname = 'UNIQ';
#     die Dumper $s;
# }


sub pick_id{
    my $s = shift;
    if (! $s->{guessed}) {
	my $md = $s->species_metadata();
	for my $prefer ($md->prefers()) {
	    my $id = first {
		lc($_->[0]) eq lc($prefer);
	    } $s->ids();

	    if ($id) {
		warn join(' ', 'picked', @$id) if ($debug);
		$s->{guessed} =
		  {
		   xref_dbname => $id->[0],
		   xref_key    => $id->[1],
		  };
		last;
	    }
	}
    }
    return $s->{guessed};
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


my @get_dbxref =
  (
   <<SQL,
SELECT id,xref_key,xref_dbname
FROM dbxref
WHERE xref_key=? AND xref_dbname=?
AND id NOT IN (SELECT dbxref_id FROM gene_product)
AND id NOT IN (SELECT dbxref_id FROM gene_product_dbxref)
ORDER BY id
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

#    if ($s->{guessed}) {
#	return ($s->{guessed}->{gene_product_id}, $s->{guessed}->{dbxref_id});
#    }

    if (!ref($guess_gp[0])) {
	@guess_gp = map {
	    $dbh->prepare($_);
	} @guess_gp;
    }

    ##########
    # The main loop that does the guessing
    my @maybe; # holds possible guesses
    for my $guess (@guess_gp) {
	for my $id ($s->ids) {
	    my ($dbname, $key) = @$id;

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
		    if (not first {
			$_->{dbxref_id} == $maybe{dbxref_id};
		    } @maybe) {
			push @maybe, \%maybe;
		    }
		}
	    }
	}

	my $maybe = scalar @maybe;
	if (1 == $maybe) {
	    $s->{guessed} = $maybe[0];
	} elsif (1 < $maybe) {
	    for my $prefer ($s->species_metadata->prefers()) {
		my @matched;

		my $matched = first {
		    lc($prefer) eq lc(_dbname_filter($_->{xref_dbname}));
		} @maybe;
		if ($matched) {
		    $s->{guessed} = $matched;
		    last;
		}

	    }

	    if (!$s->{guessed}) {
		local $Data::Dumper::Varname = 'MULT';
		warn Dumper $s, \@maybe;
		next;
	    }
	}

	if ($s->{guessed}) {
	    warn $s->pretty if ($debug);

	    return ($s->{guessed}->{gene_product_id},
		    $s->{guessed}->{dbxref_id});
	} elsif (0 < scalar(@maybe)) {
	    local $Data::Dumper::Varname = 'MISS';
	    die Dumper $s, \@maybe;
	}
    }
    #
    ##########

    if (!$dbxref_p) {
	return (undef, undef);
    }

    # If we made it here we didn't find a gene_product.
    ##########
    # Lets seek a matching dbxref entry

    my @id = $s->ids(); #unique_ids();

    if (scalar @id) {


	if (!ref($guess_dbxref[0])) {
	    @guess_dbxref = map {
		$dbh->prepare($_);
	    } @guess_dbxref;
	}

	if (!ref($get_dbxref[0])) {
	    @get_dbxref = map {
		$dbh->prepare($_);
	    } @get_dbxref;
	}

	my %sort_by;
	my @sort_by = $s->species_metadata()->prefers();
	for (my $loop = 0; $loop < scalar @sort_by; $loop++) {
	    $sort_by{$sort_by[$loop]} = $loop;
	}
	@id = sort {
	    my $A = $sort_by{$a->[0]} || 1000000;
	    my $B = $sort_by{$b->[0]} || 1000000;
	    $B <=> $A;
	} @id;


	for my $id (@id) {
	    my ($xref_dbname, $xref_key) = @$id;

	    for my $get (@get_dbxref) {
		$get->execute($xref_key, $xref_dbname);
		$s->_guessing($get, $xref_dbname, $xref_key);
		if ($s->{guessed}) {
		    return (undef, $s->{guessed}->{dbxref_id});
		}
	    }

	    next if ($xref_key =~ m/^\d+$/); # skip ids that are all numbers

	    for my $guess (@guess_dbxref){
		$guess->execute($xref_key);
		$s->_guessing($guess, $xref_dbname, $xref_key);
		if ($s->{guessed}) {
		    return (undef, $s->{guessed}->{dbxref_id});
		}
	    }
	}
    }

    #
    ##########

    warn $s->pretty if ($debug);
    return (undef, undef);
}

sub _guessing{
    my $s      = shift;
    my $sth    = shift;

    my $dbname = shift;
    my $key    = shift;


    my $matched = $sth->fetchall_arrayref();
    return if ($sth->rows == 0);

    my @matched;
    if ($dbname eq 'UniProtKB') {
	@matched = map {
	    ($_->[2] =~ m/\bUniProt(KB)?\b/i) ? $_ : ();
	    # There is a lot of rif raf in the database
	} @{ $matched };
    } else {
	@matched = @$matched;
    }

    $matched = scalar @matched;
    return if (0 == $matched);
    if ((1 < $matched) && not($quiet)) {
	warn <<TXT;
Matched $matched dbxref entries for $key, using $matched[0]->[2]
TXT
    }

    $s->{guessed} =
      {
       dbxref_id   => $matched[0]->[0],
       xref_key    => $matched[0]->[1],
       xref_dbname => $matched[0]->[2],
      };

    warn $s->pretty if ($debug);
    #return (undef, $matched[0]->[0]);
}

sub panther_id{
    my $s = shift;
    # set panther id here too?

    return $s->{panther_id} if ($s->{panther_id});

    return '~' . join('|', $s->species_metadata()->code(), map {
	join(':', @$_);
    } $s->ids());
}

sub seqIO{
    my $s = shift;
    my $seq = shift;

    $s->ids($seq->primary_id());

    my $kv = $seq->desc();
    $kv =~ s/\s+Description:\s*(.*)$//;
    $s->description($1);

    my @skip;
    my @kv = map {
	if (m/:/) {
	    $_;
	} else {
	    push @skip, $_;
	    ();
	}
    } split(m/\s+/, $kv);
    $s->ids(shift @kv);
    if ($debug and scalar(@skip)) {
	warn 'skipping from seqIO->desc(): ' . join(' ', @skip);
    }
    undef @skip;

    my @prefer = map {
	my $qm = quotemeta($_);
	qr/^$qm:/;
    } $s->species_metadata()->prefers();

  KV:
    while (@kv) {
	my $kv = shift @kv;
	for my $prefer (@prefer) {
	    if ($kv =~ m/$prefer/) {
		$s->ids($kv);
		next KV;
	    }
	}
	$s->tagval($kv);
    }

}

sub symbol{
    my $s = shift;
    if (!$s->{symbol}) {
	$s->{symbol} = $s->{tagval}->{GN};
	if (!$s->{symbol}) {
	    $s->{symbol} = 'n/a';
	    if ($debug) {
		local $Data::Dumper::Varname = 'SYM';
		die Dumper $s;
	    }
	}
    }
    return $s->{symbol};
}

sub pretty{
    my $s = shift;
    my $out = $s->panther_id . ' ';

    if ($s->{guessed}) {
	$out .= ($s->{guessed}->{gene_product_id} ? '=> ' : '~> ') .
	  $s->{guessed}->{xref_dbname} . ' ' . $s->{guessed}->{xref_key};
    } else {
	$out .= '=> ?';
    }
    return $out;
}

1;
