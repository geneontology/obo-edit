#!/usr/local/bin/perl
package My::Pthr;
use strict;
use warnings;
use Data::Dumper;
use List::Util qw/first/;
use Memoize;
use Pod::Usage;

use DBI;

#export DBI_DBNAME=DBI:mysql:mysql_socket=/Genomics/local/go/mysql/mysql.sock;database=go_latest
#export DBI_DBNAME=DBI:mysql:database=go_latest
#export DBI_USER=root
#export DBI_PASS=

use GO::Metadata::Panther;
use GO::AppHandle;

if (!@ARGV) {
    pod2usage();
    exit;
}
our $apph = GO::AppHandle->connect(\@ARGV);
our $dbh = $apph->dbh;
our $quiet;

our $debug;

# Often I only want one row returned from an SQL query.  This return
# undef if we get zero rows and dies if it gets more then one.
sub select_one{
    my $sth = shift;
    die Dumper \@_ if (!ref $sth);
    $sth->execute(@_) or die $dbh->errstr . ': ' .
      $sth->{Statement} . ': (' . join(',',@_) . ")\n";

    my $out = $sth->fetchall_arrayref();
    if ($sth->rows == 0) {
	return undef;
    } elsif ($sth->rows > 1) {
	die 'Expecting 1 row, found ' . $sth->rows . ":\n" .
	  $sth->{Statement} . ': (' . join(',', @_) . ')';
    }
    return $out->[0];
}


my $grant = $dbh->prepare('SHOW GRANTS');
$grant->execute();
$grant = $grant->fetchall_arrayref;
if (!first { $_->[0] =~ m/GRANT ALL PRIVILEGES/} @$grant) {
    warn "I don't think you have write access to the database!\n";
}


our $select_species_id_sth = $dbh->prepare(<<SQL);
SELECT id FROM species WHERE ncbi_taxa_id=?
SQL
sub species_ids{
    my $s = shift;
    my $species = $s->{species};
    if (!$species->{species_ids}) {
	for my $ncbi ($species->ncbi_ids) {
	    my $r = select_one($select_species_id_sth, $ncbi);

	    if ($r->[0]) {
		push @{ $species->{species_ids} }, $r->[0];
	    } else {
		die $species->code . ' (' .
		  $species->ncbi_taxa_id . ') was not fonud';
	    }
	}
    }
    return @{ $species->{species_ids} };
}


# The id names that panther uses isn't always the same as what GO
# uses. Here is the translation.
our %metonym =
  (
   UniProtKB => [ qw(UniProtKB UniProtKB/Swiss-Prot UniProtKB/TrEMBL
		     UniProt uniprot UNIprotKB SWISS-PROT Swiss-Prot) ],
   ENSEMBL   => [ qw/ENSEMBL Ensembl/ ],
   NCBI      => [ qw/NCBI RefSeq/ ],
   FB        => [ qw/FB FlyBase/ ],
   WB        => [ qw/WB WormBase/ ],
   ECOLI     => [ qw/EcoCyc/ ],
  );


# takes two arguments: 1> the panther id, 2> the cluster name
sub new{
    my $c = shift;

    my %s;
    $s{pthr}    = shift; # panther id
    $s{cluster} = shift; # phylotree cluster
    my $hash = shift || {};

    my ($species, @ids) = split(m/\|/, $s{pthr});
    if (exists $hash->{$species}) {
	$s{species} = $hash->{$species};
    } else {
	return undef;
	#$s{species} = GO::Metadata::Panther->new($species)
    }
    return undef if (!$s{species});

    $s{ids} = [ map {
	my ($dbname, $key) = split(m/:/, $_, 2);
	[ $dbname => $key ];
    } @ids ];

    return bless \%s, $c;
}

memoize('notice');
sub notice{
    return if $quiet;
    my $s = shift;
    print join(' ', $s->{pthr}, @_) . "\n";
}


our @select_gene_product =
  ($dbh->prepare(<<SQL),
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

   $dbh->prepare(<<SQL),
SELECT gene_product.id AS gene_product_id
,dbxref.id AS dbxref_id
,xref_key
,xref_dbname
FROM gene_product
JOIN gene_product_dbxref ON(gene_product.id=gene_product_dbxref.gene_product_id)
JOIN dbxref ON(gene_product_dbxref.dbxref_id=dbxref.id)
WHERE xref_key=? AND species_id=?
SQL



# if you want to compare the gene_product.dbxref_id use this.

   $dbh->prepare(<<SQL),
SELECT gene_product.id AS gene_product_id
,d2.id AS dbxref_id
,d2.xref_key
,d2.xref_dbname
FROM dbxref AS d1
JOIN gene_product_dbxref ON(d1.id=gene_product_dbxref.dbxref_id)
JOIN gene_product ON(gene_product_dbxref.gene_product_id=gene_product.id)
JOIN dbxref AS d2 ON(gene_product.dbxref_id=d2.id)
WHERE d1.xref_key=? AND species_id=?
SQL


  );


# Tries to get the gene_product_id for this item. It sets dbxref_id if
# it finds it too.
sub gene_product_id{
    my $s = shift;
    return $s->{gene_product_id} if (exists $s->{gene_product_id});

    my @out;
    for my $species_id ( $s->species_ids ) {
	my @id = map { $_->[1] } @{ $s->{ids} };
	for my $sgp (@select_gene_product) {
	    for my $id (@id) {
		my @args = ($id, $species_id);
		$sgp->execute(@args) or die;
		push @out, @{ $sgp->fetchall_arrayref() };
	    }

	    if (scalar(@out) > 1) {
		my %r; # report
		for my $o (@out) {
		    $r{$o->[3]}++;
		}

		@out = map {
		    if ($r{$_->[3]} > 1) {
			$s->notice('Skipping multiple dbxref.xref_dbname entry:',
				   $_->[3]);
			();
		    } else {
			$_;
		    }
		} @out;
	    }

	    # only try to get more if we didn't get any.
	    last if (scalar @out);
	}
    }

    my $found = scalar @out;
    if ($found == 0) {
	#$s->notice('No gene_product rows found');
	return ($s->{gene_product_id} = undef);
    }

    my $out;
    if ($found == 1) {
	$out = $out[0]
    } else {
	my @prefer = @{ $s->{species}->{prefer} || [] };
	while (@prefer) {
	    my $want = shift @prefer;
	    $out = first {
		lc($_->[3]) eq lc($want);
	    } @out;
	    last if ($out);
	}
	if ($out) {
	    $s->notice('matched', $found, 'gene_product rows');
	} else {
	    die "Don't know what to prefer for: $s->{pthr}\n" . Dumper(\@out);
	}
    }

    $s->{gene_product_id} = $out->[0];
    $s->{dbxref_id} = $out->[1];
    $s->{xref} = $out->[2];
    return $s->{gene_product_id};
}


my $select_type_id = $dbh->prepare(<<SQL);
SELECT id FROM term WHERE name=? AND term_type=?
SQL
memoize('type_id');
sub type_id{
    my $name = shift; # $species{$s->{species}}->{sequence};
    my $type = 'sequence';
    my $out = select_one($select_type_id, $name, $type) or die
      "can't find type ('$name', '$type')";
    warn "type_id ($name, $type) => $out->[0]" if ($debug);
    return $out->[0];
}

1;

package main;
use strict;
use warnings;
use Memoize;
use Data::Dumper;
use Text::Wrap;
use Getopt::Long;
use Pod::Usage;
use List::Util qw/first/;

=head1 NAME

seq2pthr2phylotree.pl - Takes PANTHR's F<seq2pthr.gz> file and
populates the C<phylotree> table.

=head1 SYNOPSIS

zcat seq2pth.gz | I<...>/seq2pthr2phylotree.pl -dbname go_latest
 -dbsocket I<...>/mysql.sock

zcat seq2pth.gz | I<...>/seq2pthr2phylotree.pl -dbname go_latest
 -s DANRE --tsv=./ens2zfin.tsv --from 0 --from 1 --to 2

seq2pthr2phylotree.pl -dbname I<db> [-dbsocket I<sock>]
[--every=I<seconds>]
[--species=I<pthr>]
[--tsv=I<file> --to=I<col> --from=I<col> [--from I<col> [...]]]
[--match-only]
[--no-dry-run]
[--quiet]

Plus other L<GO::AppHandle> options.

=head1 DESCRIPTION

=over

=item C<--every=I<seconds>>

Prints C<mark> every I<seconds> seconds.  Defaults to not printing
anything.

=item C<--species=I<pthr>>

Only process specified species.  Species specified with PANTHR species
id (the bit before the first pipe in the PATHER id).  This item can be
specified more then once.

=item C<--tsv=I<file>>

Specify a TSV file that contains alternate name for ids in the input
file.  When specified the C<--to>, and at least one C<--from> option
needs to be specified.

=over

=item C<--to=I<col>>

The column in the TSV file that we wish to map ids to.

=item C<--from=I<col>>

Columns in the TSV file to make ids from.  This can be specified more
then once.

=back

=item C<--match-only>

Stop after before database insertion start.  Use to check which items
are not in the database.

=item C<--no-dry-run>

By default C<seq2pthr2phylotree.pl> only print out the SQL commands
that would be used, and provides phony negative numbers for table IDs
that would of been created.  Use this item to acutally write data to
the database.

=item C<--quiet>

This will supress the SQL output when dry running.

=back

=cut
my @species;
my $dry_run = 10000; # start pho column ids at negative this number
my $pthr_xref_dbname = 'PantherDB'; # Make this an option when loading
                                    # somethings other then pthr stuff
my $every = 0; #30; # seconds
my $last = $^T;
my $match_only;

my $tsv;  # optional tsv file
my @from; # columns from the tsv file to read match from
my $to;   # column in file to use as id is matched


my %gene_product_report;

GetOptions
  (
   'every=i'      => \$every,
   'species=s'    => \@species,

   'tsv=s'        => \$tsv,
   'from=i'       => \@from,
   'to=i'         => \$to,

   'dry-run!'     => \$dry_run,
   'debug!'       => \$My::Pthr::debug,
   'quiet!'       => \$My::Pthr::quiet,
   'match-only!'  => \$match_only,
  ) or die pod2usage();


my @e; # error

if (scalar(@species) && !GO::Metadata::Panther::valid_codes(@species)) {
    push @e, wrap('Valid argument for --species include: ', "\t",
		  GO::Metadata::Panther->codes());
}

if ($tsv) {
    if (!$to or !scalar(@from)) {
	push @e, wrap('', '', <<TXT);
If --tsv is specified --to and --from are needed too.
TXT
    }
}

die pod2usage(join("\n", @e)) if (scalar @e);


if (!scalar @species) {
    @species = GO::Metadata::Panther::codes();
}

my %species = map {
    $_->{code} => $_;
} GO::Metadata::Panther->new(@species);


my %tsv;
if ($tsv) {
    open(TSV, $tsv) or die "Unable to read '$tsv', $!";
    while(<TSV>) {
	chomp;
	my @row = split(m/\t/);
	next if (!$row[$to]);
	for my $from (@from) {
	    next if (!exists $row[$from]);
	    $tsv{$row[$from]} = $row[$to];
	}
    }
    close TSV;

    if (0 == scalar keys %tsv) {
	die "Found no useful data in '$tsv'.";
    }
}

=head2 LOADING

The uncompress F<seq2pthr.gz> has two columns, the first in the
PANTHER id, the second is the name of the cluster that member is in.
The Panther id is a pipe separated list, the first element is the
species abbreviation.  All other elements are colon separated
I<dbname> I<key> pars of gene or protein identifiers.

The part scans the database and matches the given row to a
C<gene_product> row if possable.  Else it tries to match it to a
C<dbxref> row that isn't connect to a C<gene_product> row.

=cut
my %summary;
my @pthr;
while(<>) {
    chomp;

    my $row = My::Pthr->new(split(m/\t/, $_), \%species);
    next if (!$row);
    next if (!first{$_ eq $row->{species}->{code}} @species);


    push @pthr, $row;

    $summary{$row->{species}->{code}} = {}
      if (not $summary{$row->{species}->{code}});

    my $summary = $summary{$row->{species}->{code}};
    $summary->{total}++;

    if ($row->gene_product_id(\%tsv)) {
	$summary->{gene_product}++;
    } else {
	$summary->{nothing}++;
    }

} continue {

    if ($every and ((time - $last) > $every)) {
 	$last = time;
 	warn 'mark';
    }
}



=pod

Where finished matching it print out a summary of what is found.  If
the I<--match-only> option is used it will exit gracefully.

=cut

print join("\n", map {
    my $h = $summary{$_};
    "$_: " . join(',', map { "$_=>$h->{$_}" } keys %$h);
} keys %summary) . "\n";

exit (0) if ($match_only);


sub dry_run_sth{
    my $sth = shift;
    my $out = -($dry_run++);
    my $sql = $sth->{Statement};
    chomp $sql;

    print wrap
      ('dry run: ', "\t",
       $sql, ':(' . join(',',@_) . ") => $out") . "\n";
    return $out;
}

sub select_one{
    return My::Pthr::select_one(@_);
}


my $select_last_insert_id_sth = $dbh->prepare(<<SQL);
SELECT LAST_INSERT_ID()
SQL

my $insert_gene_product = $dbh->prepare(<<SQL);
INSERT INTO gene_product(symbol,dbxref_id,species_id,type_id)VALUES(?,?,?,?)
SQL


my $insert_dbxref = $dbh->prepare(<<SQL);
INSERT INTO dbxref(xref_dbname,xref_key)VALUES(?,?)
SQL

my $select_dbxref = $dbh->prepare(<<SQL);
SELECT id FROM dbxref WHERE xref_dbname=? AND xref_key=?
SQL



my $insert_phylotree = $dbh->prepare(<<SQL);
INSERT INTO phylotree(name,dbxref_id)VALUES(?,?)
SQL

my $select_phylotree = $dbh->prepare(<<SQL);
SELECT id FROM phylotree WHERE dbxref_id=?
SQL


my $insert_gene_product_phylotree = $dbh->prepare(<<SQL);
INSERT INTO gene_product_phylotree(gene_product_id,phylotree_id)VALUES(?,?)
SQL

my $select_gene_product_phylotree = $dbh->prepare(<<SQL);
SELECT id FROM gene_product_phylotree WHERE gene_product_id=? AND phylotree_id=?
SQL

my $insert_phylotree_property = $dbh->prepare(<<SQL);
INSERT INTO phylotree_property(phylotree_id,property_key,property_val)VALUES(?,?,?)
SQL

# my $select_phylotree_property = $dbh->prepare(<<SQL);
# SELECT id FROM phylotree_property WHERE
# property_key=? AND property_val=?
# SQL

# my $delete_phylotree_property = $dbh->prepare(<<SQL);
# DELETE FROM phylotree_property WHERE id=?
# SQL


memoize('gc_dbxref_id');
sub gc_dbxref_id{
    my $dbname = shift;
    my $key = shift;

    my @a = ($dbname, $key);
    my $dbxref_id = select_one($select_dbxref, @a);
    return $dbxref_id->[0] if ($dbxref_id);

    if ($dry_run) {
	return dry_run_sth($insert_dbxref, @a);
    } else {
	$insert_dbxref->execute(@a);
	$dbxref_id = select_one($select_last_insert_id_sth);
    }
    die "gc_dbxref_id dbname:$dbname key:$key" if (!$dbxref_id);
    return $dbxref_id->[0];
}

sub gc_phylotree_id{
    my $key = shift;
    my $dbxref_id = gc_dbxref_id($pthr_xref_dbname, $key);
    my $name = "$pthr_xref_dbname:$key";

    my $phylotree_id = select_one($select_phylotree, $dbxref_id);
    return $phylotree_id->[0] if ($phylotree_id);
    my @a = ($name, $dbxref_id);

    if ($dry_run) {
	$phylotree_id = dry_run_sth($insert_phylotree, @a);
    } else {
	$insert_phylotree->execute(@a);
	$phylotree_id = select_one($select_last_insert_id_sth)->[0];
    }
    die "gc_phylotree_id key:$key" if (!$phylotree_id);
    return $phylotree_id;
}

=head2 CREATING

I write the following assuming we are not dry running.  If we are dry
running F<seq2pth2phylotree.pl> will print the SQL that would of been
called.  Any rows that might of been created will use a negative
number as a fake column id.

We now know what we need to do for everything.  For each protein we
check if there is a C<phylotree> entry for the cluster.  It get
created, along with a related C<dbxref> row.  C<dbxref.xref_dbname>
will be I<PanthrDB> and C<dbxref.xref_key> will be the cluster name.
C<phylotree.name> will be a C<dbxref.xref_dbname||':'dbxref.xref_xey>
concatenation.

If we have a C<gene_product.id> the C<gene_product> and C<phylotree>
will be connected with C<gene_product_phylotree>.  Otherwise it will
skip that entry.

=cut
for my $pthr (@pthr) {
    $pthr->{phylotree_id} = gc_phylotree_id($pthr->{cluster});

    if (not($pthr->{dbxref_id}) or not($pthr->{gene_product_id})) {
	my @a = ($pthr->{phylotree_id}, 'missing', $pthr->{pthr});
	if ($dry_run) {
	    dry_run_sth($insert_phylotree_property, @a);
	} else {
	    $insert_phylotree_property->execute(@a) or
	      die $insert_gene_product_phylotree->errstr;
	}

	next;
    }



    my @a = ($pthr->{gene_product_id}, $pthr->{phylotree_id});
    my $gene_product_phylotree_id = select_one
      ($select_gene_product_phylotree, @a);
    if (!$gene_product_phylotree_id) {
	if ($dry_run) {
	    dry_run_sth($insert_gene_product_phylotree, @a);
	} else {
	    $insert_gene_product_phylotree->execute(@a) or
	      die $insert_gene_product_phylotree->errstr;
	}
    }
}

=head1 AUTHOR

Sven Heinice E<lt>sven@genomics.princeton.eduE<gt>.

=cut
