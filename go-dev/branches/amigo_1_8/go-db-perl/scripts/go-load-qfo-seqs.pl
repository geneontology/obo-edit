#!/usr/local/bin/perl -w
####
#### Load the QfO sequences after downloading them from the website.
####
# BEGIN {
#     if (defined($ENV{GO_ROOT})) {
# 	use lib "$ENV{GO_ROOT}/perl-api";
#     }
# }
use strict;
use GO::AppHandle;
use GO::Model::Seq;
use GO::Model::GeneProduct;

use Data::Dumper;

use IO::Uncompress::AnyUncompress qw/$AnyUncompressError/;
use List::Util qw/first/;

if (!@ARGV) {
    print usage();
    exit;
}

use Getopt::Long;

my $apph = GO::AppHandle->connect(\@ARGV);
my $dbh = $apph->dbh;
my $opt_h = {};
GetOptions($opt_h,
           "help=s",
           "fetch",
           "noseq",
	   "verbose|v",
	   "use_cached",
           "nomods|n",
);

if ($opt_h->{help}) {
    print usage();
    exit;
}

my $fetch_species_sth = $dbh->prepare("SELECT id FROM species WHERE ncbi_taxa_id=?");
my $fetch_dbxref_sth = $dbh->prepare("SELECT id FROM dbxref WHERE xref_dbname=? AND xref_key=?");
my $fetch_gp_sth = $dbh->prepare("SELECT id FROM gene_product WHERE dbxref_id=?");
my $insert_dbxref_sth = $dbh->prepare("REPLACE INTO dbxref (xref_dbname,xref_key) VALUES (?,?)");
my $insert_gp_sth = $dbh->prepare("REPLACE INTO gene_product (dbxref_id,species_id,type_id,symbol,full_name) VALUES (?,?,?,?,?)");
my $insert_gp_dbxref_sth = $dbh->prepare("REPLACE INTO gene_product_dbxref (gene_product_id, dbxref_id) VALUES (?,?)");

# warning - this script must be called after loading GAFs..
my $type_h =
    $dbh->selectall_hashref("SELECT DISTINCT t.name,t.id FROM term AS t WHERE t.id IN (SELECT DISTINCT type_id FROM gene_product)", "name");

my $time = localtime(time);

my $n_seqs = 0;
my $n_files = 0;
my $n_new_gps = 0;

## Internally fetch QfO from FTP. The method from inside
## go_db_install.pl should be preferred as it is safe (cleans self up)
## during problems.
if ($opt_h->{fetch}) {
  my $ftpurl = "ftp://ftp.ebi.ac.uk/pub/contrib/qfo/";
  if (system("wget -np -r -l 1 $ftpurl")) {
    die "cannot fetch from $ftpurl";
    # retry?
  }
  @ARGV = split(/\n/,`ls ftp.ebi.ac.uk/pub/contrib/qfo/*fasta`);
}


my @bad = ();
foreach my $f (@ARGV) {
    #warn "Loading $f\n";
    if ($f =~ /^ftp:.*\/(\S+)/) {
	my $localf = "$1";
	logmsg("downloading $1 to $localf");
	
	if (-f $localf && $opt_h->{use_cached}) {
	    logmsg("using previously downloaded $localf");
	    $f = $localf;
	}
	else {
	    # e.g ftp://ftp.ebi.ac.uk/pub/contrib/qfo/10090_mus_musculus.fasta
	    if (system("wget $f -O $localf.tmp && mv $localf.tmp $localf")) {
		push(@bad, "cannot download $f");
	    }
	    $f = $localf;
	}
    }
    load_fasta($f);

}

logmsg("n_files: $n_files");
logmsg("n_seqs: $n_seqs");
logmsg("n_new_gps: $n_new_gps");

if (@bad) {
    print "ERRORS:\n";
    print "$_\n" foreach @bad;
    exit 1;
}

exit 0;

sub logmsg {
    if ($opt_h->{verbose}) {
	print STDERR "@_\n";
    }
}

sub load_fasta {
    my $f = shift;
    my %unknown_ids = ();

    logmsg("loading $f");
    my ($ncbitaxid, $binomial);
    if ($f =~ /(\d+)_(.*)\.fasta/) {
	($ncbitaxid, $binomial) = ($1,$2);
    }
    if (!$ncbitaxid) {
	die $f;
    }
    
    $fetch_species_sth->execute($ncbitaxid);
    my ($species_id) = $fetch_species_sth->fetchrow_array;
    if (!$species_id) {
	die ("$ncbitaxid not in db");
    }

    #my ($seqid,$modid,$symbol,$desc);
    my %last;

    #my $seqid;
    #my $gp_id;
    #my $seq = '';

    # this handel uncompress files too
    my $fh = IO::Uncompress::AnyUncompress->new($f)
	  or die "AnyUncompress failed: $AnyUncompressError\n";
    while (<$fh>) {
	chomp;

	if (s/^\>\s*//) {
	    my $head = $_;
	    $head =~ s/\s*$//;

	    $head =~ s/\s+Description:\s*(.*)//;
	    my $desc = $1;

	    my ($seqid,$modid,@tagvals) = split(m/\s+/, $head);

	    die $head if (!$seqid or !$modid);

	    my $symbol = '';
	    for my $tv (@tagvals) {
		my ($t,$v) = split(m/:/,$tv,2);

		if (!$v) {
		    if ($t =~ m/^ENSG\d{11}$/) {
			$v = $t;
			$t = 'ENSEMBL';
		    } else {
			logmsg("what dbname is '$t'?");
			next;
		    }
		}

		if ($t eq 'GN') {
		    $symbol = $v;
		} elsif ($t eq 'MGI') {
		    # http://wiki.geneontology.org/index.php/Identifiers#MGI
		    if ($v !~ m/^MGI:/) {
			$v = "MGI:$v";
		    }
		} elsif (($head =~ m/\bUniProtKB\b/) or
			 first {
			     $t eq $_;
			 } qw/PE EMBL EntrezGene protein_id HGNC RGD
			      RefSeq_peptide ENSEMBL RefSeq_dna/) {
		    # do nothing...om.
		} elsif (substr($modid,0,length($t)) eq $t) {
		    # e.g. >UniProtKB/Swiss-Prot:Q9HGQ1 GeneDB_Spombe:SPAC212.01c GeneDB_Spombe:SPBCPT2R1.04c PE:2
		    # multiple genes on the same line.
		    # here we just ignore 2nd
		} else {
		    logmsg("wot iz $t?");

		    ## Count odd ids.
		    $unknown_ids{$t} = 0 if ! defined $unknown_ids{$t};
		    $unknown_ids{$t}++;
	      }
	    }

	    # unless this is the first header, store the LAST sequence
	    store_seq(@last{qw/gp_id seqid seq/}) if $last{seq};
	    $n_seqs++;

	    $last{seq} = '';

	    # now the new stuff:
	    #die Dumper ($modid,$species_id,$symbol,$desc);
	    $last{gp_id} = get_gp_id($modid,$species_id,$symbol,$desc);
	    if (!$last{gp_id}) {
		if ($opt_h->{nomods}) {
		    next;
		}
		die "no gp";
	    }


	    $last{seqid} = $seqid;
	} elsif (/^[A-Z\*]+$/) {
	    $last{seq} .= $_;
	} else {
	    die "$_ is not a sequence";
	}
    }
    close $fh;

    ## Final warning about unknown ids.
    #foreach my $kid (keys %unknown_ids){
    my @uid_list = keys %unknown_ids;
    warn("WARNING: Unknown id prefixes: " . join(' ', @uid_list)) if scalar(@uid_list);
    #}

    # store the final sequence
    store_seq(@last{qw/gp_id seqid seq/});

    $n_files ++;
}

sub get_dbxref_id {
    my ($db, $acc) = @_;
    $fetch_dbxref_sth->execute($db,$acc);
    my ($dbxref_id) = $fetch_dbxref_sth->fetchrow_array;
    if (!$dbxref_id) {
	if ($opt_h->{nomods}) {
	    return;
	}
	($dbxref_id) = $insert_dbxref_sth->execute($db,$acc);
	$fetch_dbxref_sth->execute($db,$acc);
	($dbxref_id) = $fetch_dbxref_sth->fetchrow_array;
    }
    if (!$dbxref_id) {
	die "weird, cannot insert/get $db:acc";
    }
    return $dbxref_id;
}

sub get_gp_id {
    my ($modid,$species_id,$symbol,$desc) = @_;
    my ($db,$acc);
    if ($modid =~ /^([\w\/\-]+):(.*)/) {
	($db,$acc) = ($1,$2);
    }
    else {
	die $modid;
    }

    if (!$symbol) {
	$symbol = $acc;
    }

    my $type_id = $type_h->{gene}->{id};
    if ($db =~ /uniprot/i) {
	$type_id = $type_h->{protein}->{id};
    }

    logmsg("fetching $modid ($symbol)");

    # first get/insert the dbxref
    # (note it's possible we have the xref but not the gp - e.g if the gp was used as evidence)
    my ($dbxref_id) = get_dbxref_id($db,$acc);

    # now get/insert the gp
    $fetch_gp_sth->execute($dbxref_id);
    my ($gp_id) = $fetch_gp_sth->fetchrow_array;
    if (!$gp_id) {
	if ($opt_h->{nomods}) {
	    return;
	}
	logmsg("storing $modid ($symbol) xref_id=$dbxref_id species_id=$species_id");
	# (dbxref_id,species_id,type_id,symbol,full_name)
	($gp_id) = $insert_gp_sth->execute($dbxref_id,$species_id, $type_id,$symbol,$desc);
	$fetch_gp_sth->execute($dbxref_id);
	($gp_id) = $fetch_gp_sth->fetchrow_array;
	$n_new_gps++;
    }
    if (!$gp_id) {
	die "weird, cannot insert/get gene_product for $db:acc $dbxref_id";
    }

    return $gp_id;
}

sub store_seq {
    my ($gp_id,$seqid,$res) = @_;

    if ($seqid =~ /(\S+):(\S+)/) {
	my ($db, $acc) = ($1,$2);
	if ($db =~ /uniprot/i) {
	    $db = 'UniProtKB'; # standardize
	}
	my ($dbxref_id) = get_dbxref_id($db,$acc);
	$insert_gp_dbxref_sth->execute($gp_id, $dbxref_id);
    }

    return if $opt_h->{noseq};
    #logmsg("storing $gp_id <-> $seqid. Res=$res");

    my $gp = GO::Model::GeneProduct->new;
    $gp->id($gp_id);

    my $seqobj = GO::Model::Seq->new;
    $seqobj->seq($res);
    $seqobj->display_id($seqid);
    $seqobj = $apph->add_seq($seqobj);

    $apph->set_product_seq($gp,$seqobj);
    return;
}

sub usage {
    print "go-load-qfo-seqs.pl  [-d dbname] [-h dbhost] [-dbms dbms] <FILES>\n";
    print <<EOM;

This script will load the QuestForOrtholog sequences. See:

  ftp://ftp.ebi.ac.uk/pub/contrib/qfo/

Any file in the list of files can be a URL - in which case wget will
be used to retrieve the fasta file into the current dir. If
-use_cached is passed, then a copy of that file in the current dir
will be used.

 -v        verbose
 -noseq    does not load sequences

A beneficial side-effect of this loader is the insertion of new rows
into gene_product. The metadata from the fasta header is used to populate the row.

Currently if you want to run this for all datasets you have to do the file prep yourself:

  wget -r ftp://ftp.ebi.ac.uk/pub/contrib/qfo/
  cd ftp.ebi.ac.uk/pub/contrib/qfo/
  go-load-qfo-seqs.pl -d mydb *.fasta >& LOG

EOM
}
