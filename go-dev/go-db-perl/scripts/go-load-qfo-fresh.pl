#!/usr/local/bin/perl -w
use warnings;
use strict;
use Data::Dumper;
use File::Basename;
use File::Find;
use File::Temp qw/tempdir/;
use Getopt::Long;
use IO::Uncompress::AnyUncompress qw/$AnyUncompressError/;
use List::Util qw/first/;
use Net::FTP;
use Text::Wrap;

use Bio::SeqIO;

use GO::AppHandle;
use GO::Metadata::Panther;
use GO::MatchID;

=head1 NAME

go-load-qfo-fresh.pl - Load QFO files into GO database

Don't use this yet!  This will replace F<go-load-qfo-seq.pl>, right
now it it a work in progress.

=head1 SYNOPSIS

seq2pthr2phylotree.pl -dbname I<db> [-dbsocket I<sock>]
[ I<path_specification> ]
[--panther]
[--[no-]dry-run]
[--sensitive]
[--match-only]
[--unannotated-report]
[--debug]
[--quiet]

Plus other L<GO::AppHandle> options.

I<path_specification> = --fetch[=I<cache>] | I<fa1> [ I<fa2> [ I<..> ] ]

=head1 DESCRIPTION

=over

=item C<--fetch[=I<qfo_cache>]>

Fetch QFO files from
L<ftp://ftp.ebi.ac.uk/pub/databases/reference_proteomes/> and loads
them.  When I<qfo_cache> is not specified it will read the
I<QFO_CACHE> environmental variable.

If I<qfo_cache> has a false value it will load the files to a
temporary location and delete them when done.  Otherwise the files
will cached at I<qfo_cache> and save for next run.

If the <--fetch> option is not specified at all it will try to load
Fasta files specified on the command line. It expects the start of the
base name of the file to be the NCBI taxa id.

No matter where it gets the file from it opens them with the
L<IO::Uncompress::AnyUncompress> module.

=item C<--panther>

By default it will try to load all Fasta files found.  With this
option it will only load Fasta files that are in the Panther clusters.q

=item C<--[no-]dry-run>

By default it only does a dry run.  Use C<--no-dry-run> to actualy do the work.

=item C<--sensitive]

This will cause same warning statements to die.

=item C<--match-only>

Only matches ids, doesn't load them in to the database.

=item --unannotated-report

Print a report of C<gene_product>'s C<dbxref.xref_dbname> count when
fisinhed.

=item --quiet

Set L<GO::MatchID::quiet> to true.

=item --debug

Set L<GO::MatchID::debug> to true.

=back

=cut

# Only load QFO files that are used in panther clusters.  I wanted an
# option to only load reference genome species, but that data seems to
# be burried in GO::AmiGO::Aid someplace.
my $panther = 0;
my $verbose_p = 1;
my $dry_run = 1000;
my $match_only;
my $fetch_dir;
my $clean_p = 0;

#my $download_dir = "$ENV{HOME}/tmp"; # change to tempdir
my %ftp_opt = ( Passive => 1 ); # In doubt use passive, it's what
                                # wget(1) does
my $ftp_host = 'ftp.ebi.ac.uk';
my $ftp_dir = '/pub/databases/reference_proteomes/';
my $unannotated_report_p;
my $apph = GO::AppHandle->connect(\@ARGV);

GetOptions
  (
   'fetch:s'             => \$fetch_dir,

   'panther-species!'    => \$panther,
   'sensitive!'          => \$GO::MatchID::sensitive,
   'match-only!'         => \$match_only,
   'dry-run!'            => \$dry_run,
   'unannotated-report!' => \$unannotated_report_p,

   'quiet!'              => \$GO::MatchID::quiet,
   'debug!'              => \$GO::MatchID::debug,
  ) or die;

my $fasta_match;
if ($panther) {
    my $re = join('|', map {
	quotemeta($_->ncbi_taxon_id);
    } GO::Metadata::Panther->panther_all() );
    $fasta_match = qr/^($re)[^\d].*?\.fasta(\.gz)?$/;
} else {
    $fasta_match = qr/\.fasta(\.gz)?$/;
}


sub ftp_connect{
    my $out = okp(Net::FTP->new($ftp_host, %ftp_opt), "[FTP] connecting to $ftp_host:", $@);
    okp($out->login('anonymous', 'anonymous'), '[FTP] loggig in:', $out->message);
    okp($out->binary(), '[FTP] binary mode:', $out->message);
    okp($out->cwd($ftp_dir), "[FTP] changing to directory '$ftp_dir':", $out->message);
    return $out;
}

my @file;


if (defined $fetch_dir) {
    if (!$fetch_dir) {
	$fetch_dir                    =
	  $ENV{QFO_CACHE}             ?
	  $ENV{QFO_CACHE}             :
	  tempdir(CLEANUP => $clean_p);
    }

    sub okp{
	my $check = shift;
	chomp $_[-1];
	my $message = join(' ', @_) . "\n";
	if (!$check) {
	    die "Failed: $message";
	} elsif ($verbose_p) {
	    warn $message
	}
	return $check;
    }

    my $ftp = ftp_connect();

    @file = map {
	if (m/$fasta_match/) {
	    my $file = File::Spec->catfile($fetch_dir, $_);
	    my @stat = stat($file);

	    if (-e $file) { # Just check if it exists
		okp(1, '[FTP] Already have', $file);
	    } else {
		warn "[FTP] fetching $file\n";
		if (!$ftp->get($_, $file)) {
		    unlink $file;
		    die "Failed [FTP] Got $file:", $ftp->message;
		}
	    }

	    $file;
	} else {
	    ();
	}

    } $ftp->ls();
    $ftp->quit();
} else {
    my @directory;

    while (@ARGV) {
	my $file = shift @ARGV;

	if (-d $file) {
	    push @directory, $file;
	} elsif (-f $file) {
	    push @file, $file;
	}
    }

    sub wanted{
	if (m/$fasta_match/) {
	    push @file, $File::Find::name;
	}
    }

    find(\&wanted, @directory) if (scalar @directory);
}

if ($verbose_p) {
    if (1 == scalar @file) {
	warn "Going to load one fasta file: $file[0]\n";
    } else {
	warn 'Going to load ' . scalar(@file) . " fasta files.\n";
    }
}

my %sth =
  (
   select_term    => 'SELECT id FROM term WHERE term_type=? AND name=?',
   last_insert_id => 'SELECT LAST_INSERT_ID()',
   insert_dbxref  => 'INSERT INTO dbxref(xref_dbname,xref_key)VALUES(?,?)',
   insert_gene_product => <<SQL,
INSERT INTO gene_product(symbol,dbxref_id,species_id,type_id)VALUES(?,?,?,?)
SQL
  );





my $type_id;
if (!$match_only) {
    my $dbh = $apph->dbh();
    for my $key (keys %sth) {
	$sth{$key} = $dbh->prepare($sth{$key});
    }

    if ($dry_run) {
	$type_id = -($dry_run++);
    } else {
	$type_id = GO::MatchID::_select_one_row
	  ($sth{select_term}, 'sequence', 'protein');
	$type_id = $type_id->[0];
    }
}


my %stats;
$GO::MatchID::dbh = $apph->dbh();
my @needed;
while (@file) {
    my $fasta = shift @file;

    my $ncbi_taxa_id = my $basename = basename $fasta;
    if ($ncbi_taxa_id =~ s/_.*//) {
    } else {
	die "Can't parse out ncbi_taxa_id from '$ncbi_taxa_id'";
    }

    $stats{$basename}->{count} = 0;
    warn "opening '$fasta'\n";
    my $fa = IO::Uncompress::AnyUncompress->new($fasta)
       or die "AnyUncompress failed opening '$fasta': $AnyUncompressError\n";

    my $seqIO = Bio::SeqIO->new(-fh => $fa, -format => 'fasta');

    while (my $seq = $seqIO->next_seq()) {
	my $guesser = GO::MatchID->new(ncbi_taxa_id => $ncbi_taxa_id);
	$stats{$basename}->{count}++;
	$guesser->seqIO($seq);

	my ($gene_product_id, $dbxref_id) = $guesser->guess(1);

	#local $Data::Dumper::Varname = "LINE$.";
	#die Dumper $guesser;

	if ($gene_product_id) {
	    $stats{$basename}->{gene_product}++;
	    next;
	} elsif ($dbxref_id) {
	    $stats{$basename}->{dbxref}++;
	} else {
	    $stats{$basename}->{missing}++;
	}

	next if ($match_only);

	##########
	# Here is the bit that inserts new dbxref and gene_product
	# entries.


	if (!$dbxref_id) {

	    if (!$guesser->pick_id()) {
		local $Data::Dumper::Varname = 'PICK';
		die Dumper $guesser;
	    }

	    my $worked = $guesser->{guessed}->{dbxref_id} = create_id
	      ($sth{insert_dbxref},
	       $guesser->{guessed}->{xref_dbname},
	       $guesser->{guessed}->{xref_key});
	    if (!$worked) {
		if ($GO::MatchID::sensitive) {
		    local $Data::Dumper::Varname = 'IDX';
		    die Dumper $guesser;
		} else {
		    # DBD shold of returned a warning
		    next;
		}
	    }

	}

	#my $symbol = $guesser->{tagval}->{GN} || $guesser->{guessed}->{xref_key};
	my $worked = $guesser->{guessed}->{gene_product_id} = create_id
	  ($sth{insert_gene_product}, $guesser->symbol(),
	   $guesser->{guessed}->{dbxref_id}, $guesser->species_id,
	   $type_id);
	if (!$worked) {
	    local $Data::Dumper::Varname = 'IGP';
	    die Dumper $guesser;
	}

	# replace stuff in sequence table here.  Use
	# go-load-qfo-seqs.pl's store_seq() subroutine.
	##########

    }
}
warn Dumper \%stats if ($GO::MatchID::debug);
print unannotated_report() if ($unannotated_report_p);


sub unannotated_report{
    my $r = $apph->dbh()->selectall_arrayref(<<SQL);
SELECT
 x.xref_dbname,
 COUNT(*) AS num_unannotated
FROM gene_product AS g
 INNER JOIN dbxref AS x ON (g.dbxref_id=x.id)
WHERE g.id NOT IN
   (SELECT DISTINCT gene_product_id FROM association)
GROUP BY
 x.xref_dbname
SQL

    my @head = qw/xref_dbname num_unannotated/;
    my $fmt = '%' . length($head[0]) . 's %' . length($head[1]);
    my $out = sprintf $fmt . "s\n", @head;
    $fmt .= "d\n";
    while (scalar @$r) {
	my $d = shift @$r;
	$out .= sprintf $fmt, $d->[0], $d->[1];
    }
    return $out;
}


sub create_id{
    my $sth = shift;

    if ($dry_run) {
	my $out = -($dry_run++);
	warn wrap
	  ('', ' ', $sth->{Statement}, ': (', join(', ', @_),
	   ") = $out")
	    unless ($GO::MatchID::quiet);
	return $out;
    }

    $sth->execute(@_) or return undef;
    my $out = GO::MatchID::_select_one_row($sth{last_insert_id});
    if (!$out) {
	die wrap
	  ('', ' ', $sth->{Statement}, ': (', join(', ', @_),
	   ') returned', $sth->rows(), 'rows)');
    }
    return $out->[0];
}

=head1 AUTHOR

Sven Heinice E<lt>sven@genomics.princeton.eduE<gt>.

=cut
