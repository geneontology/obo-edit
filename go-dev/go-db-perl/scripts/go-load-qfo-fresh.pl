#!/usr/local/bin/perl -w
use warnings;
use strict;
use Data::Dumper;
use File::Basename;
use File::Find;
use Getopt::Long;
use IO::Uncompress::AnyUncompress qw/$AnyUncompressError/;
use List::Util qw/first/;
use Text::Wrap;
use Net::FTP;

use Bio::SeqIO;

use GO::AppHandle;
use GO::Metadata::Panther;
use GO::MatchID;

# Only load QFO files that are used in panther clusters.  I wanted an
# option to only load reference genome species, but that data seems to
# be burried in GO::AmiGO::Aid someplace.
my $panther = 0;
my $verbose_p = 1;
my $dry_run = 1000;
my $match_only;
my $fetch_p;

my $download_dir = "$ENV{HOME}/tmp"; # change to tempdir
my %ftp_opt = ( Passive => 1 ); # In doubt use passive, it's what
                                # wget(1) does
my $ftp_host = 'ftp.ebi.ac.uk';
my $ftp_dir = '/pub/databases/reference_proteomes/';


my $apph = GO::AppHandle->connect(\@ARGV);


GetOptions
  (
   'panther-species!' => \$panther,
   'quiet!'           => \$GO::MatchID::quiet,
   'debug!'           => \$GO::MatchID::debug,
   'match-only!'      => \$match_only,
   'dry-run!'         => \$dry_run,
   'fetch!'           => \$fetch_p,
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

if ($fetch_p) {
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
	    my $file = File::Spec->catfile($download_dir, $_);
	    my @stat = stat($file);

	    if (-e $file) { # Just check if it exists
		okp(1, '[FTP] Already have ', $file);
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
	$guesser->ids( $seq->primary_id() );

	my $parse_me = $seq->desc();
	$parse_me =~ s/\s+Description:\s*(.*)$//;
	$guesser->description($1);

	my ($id, @tagval) = split(m/\s+/, $parse_me);
	$guesser->ids($id);
	undef $id;
	$guesser->tagval(@tagval);
	undef @tagval;


	my ($gene_product_id, $dbxref_id) = $guesser->guess(1);
	#warn $guesser->guessed. "\n";
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

	    $guesser->{guessed}->{dbxref_id} = create_id
	      ($sth{insert_dbxref},
	       $guesser->{guessed}->{xref_dbname}, $guesser->{guessed}->{xref_key})
		or die Dumper $guesser;

	}

	$guesser->{guessed}->{gene_product_id} = create_id
	  ($sth{insert_gene_product}, ($guesser->{tagval}->{GN} || ''),
	   $guesser->{guessed}->{dbxref_id}, $guesser->species_id,
	   $type_id) or die Dumper $guesser;

	# replace stuff in sequence table here.  Use
	# go-load-qfo-seqs.pl's store_seq() subroutine.
	##########

    }

}
warn Dumper \%stats;



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
