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

our $debug;

# At least BOVIN flopped from protein to gene
our $ignore_type = 1;

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
		die $species->key . ' (' .
		  $species->{ncbi_taxa_id} . ') was not fonud';
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
    my ($species, @ids) = split(m/\|/, $s{pthr});
    $s{species} = GO::Metadata::Panther->key($species);
    return undef if (!$s{species});

    $s{ids} = [ map {
	my ($dbname, $key) = split(m/:/, $_, 2);
	[ $dbname => $key ];
    } @ids ];

    return bless \%s, $c;
}


our $select_gene_product = $dbh->prepare
  (<<SQL . ($ignore_type ? '' : 'AND type_id=?'));
SELECT gene_product.id AS gene_product_id
,dbxref.id AS dbxref_id
FROM dbxref
JOIN gene_product ON(dbxref.id=gene_product.dbxref_id)
WHERE xref_dbname=? AND xref_key=? AND species_id=?
SQL
# Tries to get the gene_product_id for this item. It sets dbxref_id if
# it finds it too.
#
# If a hash reference is passed it, it will the use items in it as
# synonyms for the ids.
sub gene_product_id{
    my $s = shift;
    return $s->{gene_product_id} if (exists $s->{gene_product_id});
    my $hash = shift;

    my %hash = %{ $s->{species}->dbname_hash };
    my $out;
    while (my ($type, $dbnames) = each %hash) {
	my $type_id;
	$type_id = type_id($type) if (!$ignore_type);
	for my $species_id ( $s->species_ids ) {
	    for my $dbname (@$dbnames) {
		my @id = @{ $s->{ids} };

		if ($hash) {
		    my @new;
		    for my $id (@id) {
			my $new = $hash->{$id->[1]};
			if ($new) {
			    push @new, [ $dbname, $new ];
			}
		    }
		    if (scalar @new) {
			#warn Dumper \@new;
			push @id, @new;
		    }
		}

		for my $id (@id) {
		    next if ($dbname ne $id->[0]);
		    my $key = $id->[1];

		    if ($metonym{$dbname}) {
			for my $metonym (@{$metonym{$dbname}}) {
			    if ($ignore_type) {
				$out = select_one
				  ($select_gene_product,
				   $metonym, $key, $species_id);
			    } else {
				$out = select_one
				  ($select_gene_product,
				   $metonym, $key, $species_id, $type_id);
			    }
			    last if ($out);
			}
		    } else {
			if ($ignore_type) {
			    $out = select_one
			      ($select_gene_product,
			       $dbname, $key, $species_id);
			} else {
			    $out = select_one
			      ($select_gene_product,
			       $dbname, $key, $species_id, $type_id);
			}
		    }

		    if ($out) {
			$s->{gene_product_id} = $out->[0];
			$s->{dbxref_id} = $out->[1];
			$s->{xref} = $id;
			$s->{type_id} = $type_id if (!$ignore_type);
			return $s->{gene_product_id};
		    }
		}
	    }
	}
    }

    return ($s->{gene_product_id} = undef);
}


# select from dbxref only if we don't have an gene_product entry
our $select_dbxref_id = $dbh->prepare(<<SQL);
SELECT dbxref.id,xref_dbname FROM dbxref
LEFT JOIN gene_product ON(dbxref.id=gene_product.dbxref_id)
WHERE gene_product.id IS NULL
AND xref_dbname=? AND xref_key=?
SQL
# If not already set, this will seek a dbxref entry that isn't
# connected to a gene_product. If you don't care if it's connected or
# not call $s->gene_product_id first and then call this.
sub dbxref_id{
    my $s = shift;
    return $s->{dbxref_id} if ($s->{dbxref_id});

    my $out;

    for my $dbname ($s->{species}->id_types) {
	for my $id (@{ $s->{ids} }) {
	    next if ($dbname ne $id->[0]);
	    my $key = $id->[1];

	    if ($metonym{$dbname}) {
		for my $metonym (@{$metonym{$dbname}}) {
		    $out = select_one($select_dbxref_id, $metonym, $key);
		}
		last if ($out);
	    } else {
		$out = select_one($select_dbxref_id, $dbname, $key);
	    }

	    if ($out) {
		$s->{xref} = $id;
		$s->{dbxref_id} = $out->[0];
		return $s->{dbxref_id};
	    }
	}
    }

    return ($s->{dbxref_id} = undef);
}

# returns the xref.  If it doesn't have an xref it will pick one.
sub xref{
    my $s = shift;
    return $s->{xref} if ($s->{xref});
    my %dbname = %{ $s->{species}->dbname_hash };

    for my $type (keys %dbname) {
	for my $id ( @{ $s->{ids} } ) {
	    if ($dbname{$type}->[0] eq $id->[0]) {
		$s->{xref} = $id;
		$s->{type_id} = type_id($type) if (!$ignore_type);
		return $s->{xref};
	    }
	}
    }
    die 'xref ' . Dumper $s;
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
my $dry_run = 10000; # start pho column ids an negative this number
my $pthr_xref_dbname = 'PantherDB'; # Make this an option when loading
                                    # somethings other then pthr stuff
my $quiet = undef;
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
   'quiet!'       => \$quiet,
   'match-only!'  => \$match_only,
  ) or die pod2usage();


my @e; # error

if (scalar(@species) && !GO::Metadata::Panther::valid_keys(@species)) {
    push @e, wrap('Valid argument for --species include: ', "\t",
		  GO::Metadata::Panther->keys());
}

if ($tsv) {
    if (!$to or !scalar(@from)) {
	push @e, wrap('', '', <<TXT);
If --tsv is specified --to and --from are needed too.
TXT
    }
}

die pod2usage(join("\n", @e)) if (scalar @e);

@species = GO::Metadata::Panther::keys() if (!scalar @species);

my %tsv;
if ($tsv) {
    open(TSV, $tsv) or die "Unable to read '$tsv', $!";
    while(<TSV>) {
	chomp;
	my @row = split(m/\t/, $_);
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

    my $row = My::Pthr->new(split(m/\t/, $_));
    next if (!$row);
    next if (!first{$_ eq $row->{species}->{key}} @species);


    push @pthr, $row;

    $summary{$row->{species}->{key}} = {}
      if (not $summary{$row->{species}->{key}});

    my $summary = $summary{$row->{species}->{key}};
    $summary->{total}++;

    if ($row->gene_product_id(\%tsv)) {
	$summary->{gene_product}++;
    } elsif ($row->dbxref_id) {
	$summary->{dbxref_id}++;
	print "No gene_product entry for $row->{pthr}\n" if (!$quiet);
	#die Dumper $row;
    } else {
	$summary->{nothing}++;
	print "No gene_product/dbxref entry for $row->{pthr}\n" if (!$quiet);
	#die Dumper $row;
    }

    #$row->xref; # pick a dbxref if we don't have one

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
       $sql, ':(' . join(',',@_) . ") => $out") . "\n"
	 if (!$quiet);
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

    if (not $pthr->{dbxref_id}) {
	# my $xref   = $pthr->xref;
	# my $dbname = $xref->[0];
	# my $key    = $xref->[1];

	# $dbname = $My::Pthr::metonym{$dbname}->[0] if ($My::Pthr::metonym{$dbname});
	# $pthr->{dbxref_id} = gc_dbxref_id($dbname, $key);

	next;
    }

    ##########
    # Create a new gene_product entry
    if (not $pthr->{gene_product_id}) {

	# my @a = ($pthr->{pthr}, $pthr->{dbxref_id},
	# 	 $pthr->species_id, $pthr->{type_id});
	# if ($dry_run) {
	#     $pthr->{gene_product_id} = dry_run_sth($insert_gene_product, @a);
	# } else {
	#     $insert_gene_product->execute(@a)
	#       or die
	# 	wrap($insert_gene_product->errstr . ': ', "\t",
	# 	     $insert_gene_product->{Statement}, ':(', @a, ')');
	#     $pthr->{gene_product_id} = select_one
	#       ($select_last_insert_id_sth)->[0];
	# }


	next;
    }
    #
    ##########


    $pthr->{phylotree_id} = gc_phylotree_id($pthr->{cluster});

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
