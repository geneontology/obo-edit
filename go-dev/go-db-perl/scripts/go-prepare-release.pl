#!/usr/local/bin/perl

use GO::Admin;
use Getopt::Long;
use strict;

my $pfile;
if (-f "go-manager.conf") {
    $pfile = "go-manager.conf";
}

my $dbname;
my $dbhost;
my $dbms;
my $dateopt;
my $data_root;
my $interactive;
my $releasename;
my $monthly;
my $weekly;
my $daily;
my @skip = ();
my $noupdatesp;
my $norefresh;
my $bulkload;
my $donothing;

### Please note that we (The Stanford GO group) have commented 
### out all the lines with the variables $nocderel.  
### We will merge this bit of code with the other big changes we have made at 
### Stanford and see if it breaks anything or not.  
### This is only a temporary solution and hopefully we can uncomment these 
### lines in the near future.  May 13, 2005
### If you have any questions, please e-mail us go-admin@genome.stanford.edu

### my $nocoderel;

my $suff = "godb";
my $load;

GetOptions("Date=s"=>\$dateopt,
	   "name|n=s"=>\$suff,
	   "dbname|d=s"=>\$dbname,
	   "host|h=s"=>\$dbhost,
	   "help"=>sub {system("perldoc $0");exit},
	   "dbms|m=s"=>\$dbms,
	   "load|f=s"=>\$load,
	   "dataroot|r=s"=>\$data_root,
	   "releasename=s"=>\$releasename,
	   "interactive|i"=>\$interactive,
	   "monthly"=>\$monthly,
	   "weekly"=>\$weekly,
	   "daily"=>\$daily,
	   "skip=s@"=>\@skip,
	   "noupdatesp"=>\$noupdatesp,
	   "norefresh"=>\$norefresh,
	   "bulk"=>\$bulkload,
	   "donothing"=>\$donothing, # just a hack to test commands+ARGS
###	   "nocoderel"=>\$nocoderel,
	  );


$bulkload = 'bulk' if $bulkload;

my $time_started = localtime(time);

my $admin = GO::Admin->new;

$admin->data_root($ENV{GODATA_ROOT});

if ($load) {
    $pfile = $load;
    $admin->loadp($load);
}

# temporary; can be remove after all production versions of db have gene_product_count.species_id
$ENV{GO_HAS_COUNT_BY_SPECIES} = 1;

# call the check_environment method to check environmental variables like ??? 
#print "\nCalling check_environment method...\n";
#&check_environment($pfile);
# Don't need this?

# overwrite options
$admin->dbname($dbname) if $dbname;
$admin->dbhost($dbhost) if $dbhost;
$admin->dbms($dbms) if $dbms;
$admin->releasename($releasename) if $releasename;
$admin->data_root($data_root) if $data_root;

my $emailSubject = 'FINAL REPORT';

if ($monthly) {
    my ($y, $m) = getym();
    $dateopt = "$y-$m-01" unless $dateopt;
    $admin->releasename("go_$y$m");

    $emailSubject .= " - gofull $dateopt";
}

if ($weekly) {
    my ($y, $m, $d) = getym();
    $dateopt = "$y-$m-$d" unless $dateopt;
    $admin->releasename("go_$y$m$d");

    $emailSubject .= " - golite $dateopt";
}

if ($daily) {
    my ($y, $m, $d) = getym();
    $dateopt = "$y-$m-$d" unless $dateopt;
    $admin->releasename("go_daily");

    $emailSubject .= " - goterm $dateopt";
}

my @steps = ($norefresh ? () : ['refresh_data_root', ARG=>$dateopt]);


#if ($weekly) {
#    push(@steps, ['pre_process_files_for_ieas']);
#}


push(@steps,
   (
###   $nocoderel ? () : ['makecoderelease', ARG=>$dateopt],
   ['makecoderelease', ARG=>$dateopt],
   ['newdb'],
   ['load_schema'],
   ['load_termdb', CHECK=>sub{shift->guess_release_type eq 'termdb'}],
   ['load_xrf_abbs'],    
   ['make_release_tarballs', ARG=>'termdb'],
   ['dumprdfxml', ARG=>'termdb'],
   ['dumpoboxml', ARG=>'termdb'],
   ['dumpowl', ARG=>'termdb']
   # TERMDB DONE
));

unless ($daily) {
    push(@steps,
	 (
          ['update_speciesdir'],
 	  ['load_species'],
 	  ($weekly ?
              (['load_assocdb', ARG=>$bulkload, # now has IEAs
	       'CHECK'=>sub{shift->guess_release_type eq 'assocdb'}, ] 
              ) :
	      (['load_assocdb', ARG=>$bulkload,
	      'CHECK'=>sub{shift->guess_release_type eq 'assocdb'},]
              )
          ),
          ['load_refg'],

 	  ['make_release_tarballs', ARG=>'assocdb'], 
 	  
 	  # DON'T DUMP RDFXML YET - WAIT TIL WE HAVE REMOVED IEAs 	  
 	  # ASSOCDB DONE

 	  ($noupdatesp ? () : (['updatesp'])),

          ($weekly ?
              (['load_seqs',  # Now has IEAs
               'CHECK'=>sub{shift->guess_release_type eq 'seqdb'}]
              ) :
	     ()
          ),

 	  # SEQDB DONE ONLY WEEKLY

	  ));
}

if ($monthly) {	 
    push(@steps, 
            (
             ['dumprdfxml', ARG=>'assocdb']      # XML has no seqs
         ));
}  
 	  
if ($weekly) {	  	  
 	 push(@steps, 
	      (
	       ['make_release_tarballs', ARG=>'seqdb'],   
	       ['dumpseq', ARG=>'seqdb'],   
	       ['dumprdfxml', ARG=>'assocdb']      # XML has no seqs
	       ));
}
  
push(@steps, ['check_release_tarballs']);

no strict 'refs';
my @bad = ();
my $i = 0;
foreach my $step (@steps) {
    $i++;
    my ($cmd, @p) = @$step;
    my %ph = @p;
    my @args = ();
    push(@args, $ph{ARG}) if $ph{ARG};
    my $t = time;
    my $ppt = localtime($t);
    printf "INIT: $t $ppt $cmd ARGS:@args\n";
    my $should_i_skip;
    if (@skip) {
	if (grep {$_ eq $cmd} @skip) {
	    print "SKIPPING: $i - $cmd(@args)\n";
	    $should_i_skip = 1;
	}
    }
    next if $should_i_skip;

    if ($interactive) {
	my $yes = getinput("COMMAND:$i - $cmd(@args) -- ok?");
	if ($yes =~ /^a/i) {
	    $interactive = 0;
	    $yes = 'y';
	}
	next unless $yes =~ /^y/i;
    }
    eval {
	print "CALLING: $i - $cmd(@args)\n";
	$admin->$cmd(@args) unless $donothing;
    };
    if ($@) {
	push(@bad, [$i, $cmd, $@]);
    }
    my $check = $ph{CHECK};
    if ($check) {
	my $ok;
	eval {
	    $ok = $admin->$check();
	};
	if ($@) {
	    push(@bad, [$i, "$cmd-check", $@]);
	}
	if (!$ok) {
	    push(@bad, [$i, $cmd, "FAILED CHECK ARGS:@args"]);
	}
    }
    $t = time;
    $ppt = localtime($t);
    printf "DONE: $t $ppt $cmd ARGS:@args\n";
}
my $time_finished = localtime(time);

my $report = "OK!";
if (@bad) {
    $report = 
      sprintf("BAD:%s\n",
	      join('',
		   map {sprintf("%5d:%20s:%s%s%s\n\n", @$_)} @bad));
}

my @p = $admin->_valid_params;
$report =
  sprintf("FINAL REPORT:\n\n%s\n%s\n\n%s\n\n$report\n",
	  "TIME STARTED : $time_started",
	  "TIME FINISHED: $time_finished",
	  join('', map {sprintf("%20s:%s\n", $_, $admin->$_())} @p));

my $mail = $admin->administrator;

print $report;
open(F, ">FINAL_REPORT") || warn("can't write report");
print F $report;
close(F);
my @addresses = split(/\,/, $mail);
foreach (@addresses) {
    if (/\@/) {
	system("/usr/bin/mailx -s '$emailSubject' $mail < FINAL_REPORT");
    }
}
exit(1) if scalar(@bad);
exit 0;

###################################################################################

sub check_environment {
# check and see if the names.dmp file exists in the directory specified by "speciesdir" in go-manager.conf 
# currently this is "ncbi" but check for it anyway

my $confFile = shift;

my ($speciesdir, $swissdir);

open(CONF, "$confFile") || die "Can't open $confFile file for reading:$!\n";
while(defined (my $line = <CONF>)) {
    chomp $line;
    if ($line =~ /^speciesdir\:(.+)$/) {
	$speciesdir = $1; 
    }
    elsif ($line =~ /^swissdir\:(.+)$/) {
	$swissdir = $1; 
    }
    else {
	next;
    }
}
close(CONF);

print "\nChecking for names.dmp file ...\n";
if(! -e "$speciesdir/names.dmp") {
    print ("The file ${speciesdir}/names.dmp does not exist\n");
    exit;
}

else {
    print "${speciesdir}/names.dmp file exists...\n\n";
}
# check and see if the directory specified by "swissdir" in go-manager.conf (currently "proteomes") exists
# currently "proteomes"  but check for it anyway

# also, check if it is owned the same user who launched the program
# NOTE: write permission isn't enough, since it tries to do a "chmod" on it
#print "Checking for ${swissdir} directory permissions ...\n";
#if(! -o "${swissdir}/") {
#    print "You do not own ${swissdir} directory.\n";
#    print "Please modify the directory file permissions and try again\n";
#    exit;
#}
#else {
#    print "You own ${swissdir} directory.\n";
#}

print "\n***Done running check_environment method***\n";

}

###################################################################################

sub sayyes {
    my $answer;
    while (!$answer) {
	$answer = getinput(@_); 
	chomp $answer;
    }
    return $answer =~ /^y/i;
}

###################################################################################

sub getinput {
    my $m = shift;
    my $def = shift;
    print $m if $m;
    if (defined $def) {
	print " [$def] ";
    }
    my $i = <STDIN>;
    chomp $i;
    if (!$i && defined($def)) {
	$i = $def;
    }
    return $i;
}

###################################################################################

sub getym {
    my $t = time;
    my @lt = localtime($t);
    my $y = $lt[5]+1900;
    my $d = $lt[3]; ### By AS, March 31, 2005
    $d = '0' . $d if ($d < 10);
    return ($y, sprintf("%02d", $lt[4]+1), $d);
}

###################################################################################

=head1 NAME 

go-prepare-release.pl

=head1 SYNOPSIS

  go-prepare-release.pl -f mygo.conf 

=head1 DESCRIPTION

Builds a GO database from various flat file components

It will build 4 databases in order, each database subsumes the
previous one. Two mysqldumps will be generated for each db

=head1 PRERQUISITES

You need both go-perl and go-db-perl installed

=head2 DATABASES CONSTRUCTED

=over

=item termdb

A db containing only the ontology

required files:

  $GO/ontology/*.obo
  $GO/ontology/GO.defs
  $GO/ontology/external2go/*

A CVS update will be performed to get the correct files for the release

extra files generated:

  rdf-xml
  coderelease

=item assocdb

A db subsuming termdb that contains the gene associations

required files:

  $GO/gene-associations/*

=item seqdb

A db subsuming assocdb that includes some sequence data

required files:

  $GO/gp2protein/*
  ./proteomes/*SPC

The proteomes dir is automatically downloaded from EBI

  ftp://ftp.ebi.ac.uk/pub/databases/SPproteomes/swissprot_files/proteomes

=item seqdblite

The same as seqdb but with the IEA associations removed

extra files generated:

  fasta
  rdf-xml

=head1 ARGUMENTS

=over

=item -f CONFFILE

=item -i

interactive mode

=item -monthly

make a monthly release

will cvs check out from the 1st of the month and build a release
corresponding to that date

=item -daily

make a daily release

=back

=head1 SEE ALSO

=cut
