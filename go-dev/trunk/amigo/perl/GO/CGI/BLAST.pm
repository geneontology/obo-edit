package GO::CGI::BLAST;

BEGIN { require "config.pl" if -f "config.pl" ; }
use lib "$ENV{GO_DEV_ROOT}/go-perl";
use lib "$ENV{GO_DEV_ROOT}/go-db-perl";
use lib "$ENV{GO_DEV_ROOT}/amigo/perl";

use strict;
use Carp;
#use GO::Utils qw(rearrange);
use GO::CGI::Utilities qw(:std);
use GO::CGI::NameMunger qw(case_replace);
use GO::CGI::Query qw(get_gp_details);
use DirHandle;
use FileHandle;
use Exporter;
use File::Temp;
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);
@ISA = ('Exporter');
#@EXPORT_OK = qw(set_off_blast launch_job check_blast get_blast_results);
@EXPORT_OK = qw(set_off_blast launch_job get_blast_results);
%EXPORT_TAGS = (
	all => [@EXPORT_OK],
);

use Data::Dumper;
$Data::Dumper::Indent = 1;

## Some new stuff to try...
use AmiGO;
my $core = AmiGO->new();


=head1 SYNOPSIS

package GO::CGI::BLAST

=cut

=head2 set_off_blast

	Launches the BLAST query.

	Usage:

	my $result_h = set_off_blast($apph, $error, $blast_dir, 
	                  $inputs,        # hash containing input seqs
	                  $blast_options  # values for threshold, maxhits,
	                                  # blast_filter and boolean use_filters
	
	$result_h is of the form
	
	$result_h->{error}     # contains fatal/warning/info messages
	$result_h->{results}   # the data set as the BLAST params. Will be empty
	                       # unless the BLAST submission was successful
	results looks like this:
	blast_filter => {on|off},
	maxhits => XX,
	threshold => XX,
	input =>
	{	type => { direct | seq_id },
		direct => < sequence >
		OR seq_id => < seq_id >, seqs => < seq_id's sequence >
	}

=cut

sub set_off_blast {
  my $apph = shift;
  my $error = shift;
  my $blast_settings = shift; # required settings from the CGI
  my $inputs = shift;         # hash containing the input seqs
  my $blast_options = shift;  # from the CGI

  # where the results will go
  my $blast_dir = $blast_settings->{blast_dir};

  foreach ($apph, $error, $inputs, $blast_options, $blast_dir) {
    $core->kvetch(Dumper($_));
  }

  my $max_seq_num = $blast_settings->{max_seq_num} || 100;
  my $max_seq_len = $blast_settings->{max_seq_length} || 3000000;

  my @seqs;       # array for storing our checked sequences
  my $input_seqs; # store the sequence source (e.g. seq ID or 
                  # text entered into box) and the sequence itself
  my $program;    # which blast program to use

  require GO::Model::Seq;

  # check we have some inputs
  if (scalar keys %$inputs == 0){
    $core->kvetch("No inputs found. Quitting...");
    $error = set_message($error, 'fatal', 'no_input');
    return { error => $error };
  }elsif (scalar keys %$inputs > 1){
    $core->kvetch("Too many inputs found! Warning.");
    $error = set_message($error, 'warning', 'too_many_inputs');
  }else{
    $core->kvetch("Inputs clear.");
  }

  ##
  if (!$apph || !$blast_dir){
    $core->kvetch("Missing required input apph / blast_dir");
    $error = set_message($error, 'fatal', 'config_error',
			 "No apph / user BLAST directory found!");
    return { error => $error };
  }else{
    $core->kvetch("Locations clear.");
  }

  # OK, let's get going on the query now!
  if ($inputs->{seq_id} || $inputs->{uniprot_id}) {
    my $gps;
    my $id = $inputs->{seq_id}; # this is from an AmiGO BLAST link
    if (!$id){
      $id = $inputs->{uniprot_id};
      $id =~ s/^.*[\|:]//;
      $id = "UniProt:".$id;
    }
    $core->kvetch("Work from id: " . $id);
    my $result_h = get_gp_details($apph, $error, { gpxref => $id },
				  { tmpl => { seq => 1 } });
    # no GPs found...
    if( ! $result_h->{results} ){
      $core->kvetch("No gps found...");
      return { error => $result_h->{error} };
    }
    $error = $result_h->{error};
    $gps = $result_h->{results};

    if ($gps->[0]->to_fasta){
      $input_seqs =
	{
	 type => 'seq_id',
	 seq_id =>
	 uc($gps->[0]->xref->xref_dbname) . "|" . $gps->[0]->xref->xref_key,
	 seqs => [ $gps->[0]->to_fasta ],
	};
    }else{
      $core->kvetch("Bad seq ID - GP does not have a sequence");
      $error = set_message($error, 'fatal', 'no_seq_found',
			   $input_seqs->{seq_id});
      return { error => $error };
    }
    $program = 'blastp';
    push @seqs, @{$input_seqs->{seqs}};
  }elsif ($inputs->{seq} || $inputs->{seq_from_file}){
    # uploaded sequences
    my $sequence = "";
    my $seq;

    if ($inputs->{seq_from_file}){
      $seq = $inputs->{seq_from_file};
    }elsif ($inputs->{seq}) {
      $seq = $inputs->{seq};
    }
    $core->kvetch("sequence from param seq:\n".Dumper($seq));

    #		$seq =~ s/^\s*//m;
    #		$seq =~ s/\s*$//m;
    #		$seq =~ s/\r/\n/g;
    # remove line feeds.
    $seq =~ s/\x0D//g;

    if (!$seq || $seq !~ /[a-z]/im){
      $core->kvetch("No actual sequence found.");
      $error = set_message($error, 'fatal', 'no_input');
      return { error => $error };
    }

    $seq =~ s/>/\n\n>/gm;

    #	split up the sequence by > and empty lines
    my @seq_arr = split(/\n{2,}/, $seq);
    my @msgs;
    my $l_acc;
    my $length;
    my $head;
    my $tail;
    #$core->kvetch("seq_arr: ".Dumper(\@seq_arr);

    foreach (@seq_arr){
      $core->kvetch("sequence: ".Dumper($_));
      $sequence = '';

      foreach my $seqline ( split(/\n/, $_) ){
	chomp $seqline;
	#$seqline =~ s/^[ \t\f]*//;
	#$seqline =~ s/[ \t\f]*$//;
	$core->kvetch("line before: >$seqline<");
	$seqline =~ s/^\s*//;
	$seqline =~ s/\s*$//;
	$core->kvetch("line after : >$seqline<");
	$sequence .= $seqline."\n";
      }

      if (!$sequence){
	$core->kvetch("No actual sequence found.");
	next;
	# $error = set_message($error, 'fatal', 'no_input');
	# return { error => $error };
      }

      $head = '';
      $tail = '';
      if ($sequence =~ /^>/){
	# remove the seq header
	($head, $tail) = split("\n", $sequence, 2);
	# $_ =~ s/\ //g unless ($sequence =~ /^>/);
      }else{
	$head = '>sequence';
	$tail = $_;
      }
      next if (!$tail || $tail !~ /[a-z]/i);
      $tail =~ s/\s*//g;

      $core->kvetch("sequence: ".Dumper($tail));
      #	check the sequence isn't too long
      $length = 0;
      eval {
	chomp($tail);
	my $seq = GO::Model::Seq->new(-seq=>$tail);
	$length = $seq->length;
      };
      if($@) {
	$core->kvetch("bad sequence: $@");
	push @msgs, { 'bad seq' => $@ };
	# $error = set_message($error, 'fatal', 'bad_seq', $@);
      }elsif ($length > $max_seq_len) {
	$core->kvetch("seq too long: $length");
	push @msgs, { 'seq_too_long' => '' };
	# $error = set_message($error, 'fatal', 'seq_too_long');
      }else{
	$l_acc += $length;
	if ($l_acc > $max_seq_len){
	  # too many seqs! exit the loop
	  $error = set_message($error, 'warning', 'too_many_seqs');
	  last;
	}
	push @seqs, $head."\n".$tail;

	if (scalar @seqs == $max_seq_num){
	  $core->kvetch("Reached max number of seqs - stopping parser");
	  last;
	  $error = set_message($error, 'warning', 'too_many_seqs');

	}
      }
    }
    if (!@seqs)	{
      $core->kvetch("No valid seqs found!");
      $error = set_message($error, 'fatal', 'no_input');
      return { error => $error };
    }

    $input_seqs =
      {
       type => 'direct',
       direct => join("\n\n", @seqs),
      };

    $program = get_program($seqs[0]);
    $input_seqs->{seqs} = [@seqs];
  }

  #	All is well with the sequence(s).
  #	Now let's go on to launching the job.

  $core->kvetch("Starting launchJob...");

  ## Save session to disk.
  my $file = new FileHandle;
  $core->kvetch("Eval-ing the find blast_dir command");
  eval {
    #	`find $data_dir/$blast_dir -name "*" -exec rm -f {} \\;`;
    `find $blast_dir -mindepth 1 -name "*" -exec rm -rf {} \\;`;
  };

  if ($@){
    $core->kvetch("Error: $@");
  }

  my $i = 0;
  my $bfilter_opt = " ";
  $bfilter_opt = " -filter SEG"
    if($blast_options->{blast_filter} && $blast_options->{blast_filter} eq 'on');
  my $ncpus = 1; #restrict blast load on servers

  #	my $tmpdir = File::Temp::tempdir( CLEANUP => 1 );
  #	my $cmd_file = "$tmpdir/command.sh";
  my $cmd_file = "$blast_dir/command.sh";

  for my $seq (@seqs) {
    $i++;
    my $seq_file = "$blast_dir/current_seq.$i";
    my $result = "$blast_dir/result.$i";
    my $result_tmp = "$blast_dir/result_tmp.$i";
    my $error = "$blast_dir/error.$i";

    my $seqfile = new FileHandle("> $seq_file");
    $seqfile->print($seq);
    $seqfile->close;
    `chmod a+r $seq_file`;
    # create the blast command: multi commands in one file
    my $file = new FileHandle(">> $cmd_file");
    if ($i == 1) {
      $file = new FileHandle("> $cmd_file");
      $file->print('#!/bin/sh'."\n");
    }
    #$file->print("WUBLASTFILTER=/usr/local/bdgp/wublast/filter; export WUBLASTFILTER\n");
    $file->print(
		 $blast_settings->{$program}." "
		 .$blast_settings->{fasta_db}
		 ." $seq_file"
		 ." -v".$blast_options->{maxhits}
		 ." -b".$blast_options->{maxhits}
		 .$bfilter_opt
		 ." -e".$blast_options->{threshold}
		 ." -cpus=$ncpus> $result_tmp 2>$error\n");
    $file->print("mv $result_tmp $result\n");
    $file->print("chmod a+r $result\n");
    $file->print("chmod a+r $error\n");
    $file->close;
  }
  `chmod a+r $cmd_file`;

  $core->kvetch("finished setting all the settings");

  if (lc($blast_settings->{blast_method}) eq 'pbs'){
    my $qsub = $blast_settings->{qsub};
    my $pbs_user = $blast_settings->{pbs_user} || 'public';
    my $queue = $blast_settings->{queue};

    #launch the job
    my $job = `$qsub -u $pbs_user -q $queue $cmd_file`;
    chomp $job;
    if (!$job){
      $core->kvetch("qsub failed to submit blast job to cluster!");
      $error = set_message($error, 'fatal', 'config_error',
			   "qsub failed to submit blast job to cluster!\n");
      return { error => $error };
    }
  }else{
    eval {
      system ("/bin/sh $cmd_file &")
    };

    if ($@){
      die("Error: $@");
    }

=old code here
		require LWP::UserAgent;
		my $ua = LWP::UserAgent->new;
		my $blast_url = get_environment_param('blast_url');
		$core->kvetch("blast url: $blast_url");
		my %form = ( blast_dir => $blast_dir); #, temp_dir => $tmpdir );

		$core->kvetch("starting post at ".localtime());
		my $response = $ua->post($blast_url, \%form);
#		$core->kvetch("response: ".Dumper($response));
		$core->kvetch("finished post at ".localtime());

=cut
  }
  return { error => $error, results => $input_seqs };
}


sub get_program {
	my $seq = shift;
	my @lines = split '\n', $seq;
	foreach my $line (@lines) {
		if ($line !~ /^>/) {
			if (is_na($line)) {
				return "blastx";
			} else {
				return "blastp";
			}
		}
	}
}

=head2 get_blast_results

	Parses the BLAST results file and returns the results shown by AmiGO
	
	Usage:

	my $result_h = get_blast_results($apph, $error, $blast_dir, 
	                  $seqno,         # which of the uploaded sequences to
	                                  # retrieve results for (defaults to 1)
	                  $session_id,
	                  $use_filters);  # whether or not to filter the gene
	                                  # products displayed in the results
	
	$result_h is of the form
	
	$result_h->{error}     # contains fatal/warning/info messages
	$result_h->{to_cache}  # data to be cached
	$result_h->{results}   # the useful results data
	->{results}{raw_data}  # parsed raw blast data
	->{results}{sequence}  # the sequence file
	->{results}{product_h} # hash containing the GPs returned by the BLAST
	->{results}{omitted}   # count of the number of GPs not returned due to
	                       # filtering

=cut

sub get_blast_results {
	my $apph = shift;
	my $error = shift;
	my $blast_dir = shift;
	my $seqno = shift;
	my $session_id = shift;
	my $use_filters = shift;
	$seqno = 1 if (!$seqno || $seqno =~ /\D/);
	my $result_file = "$blast_dir/result.$seqno";
	my $seq_file = "$blast_dir/current_seq.$seqno";

	if (!$apph || !$session_id || !$blast_dir || !$result_file || !$seq_file)
	{	$core->kvetch("Missing apph / session_id / blast_dir / result file / sequence file");
		$error = set_message($error, 'fatal', 'config_error', "Missing apph / session_id / blast_dir / result file / sequence file");
		return { error => $error };
	}

	my $data;
	my %xrefs;
	my $space = '            ';

	my $seq;
	if (open (SEQFILE, $seq_file))
	{	my @seqlines;
		while (my $line = <SEQFILE>) {
			push @seqlines, $line;
		}
		close SEQFILE;
		$data->{sequence} = { header => shift @seqlines, seq => join("", @seqlines) };
#		$data->{sequence} = $seq;
	}
	else
	{	$core->kvetch("Could not open sequence file $seq_file: $!");
		$error = set_message($error, 'fatal', 'config_error', "Could not open sequence file: $!");
		return { error => $error };
	}

	my $raw;
	if (-f $result_file)
	{	my $parsed = 0;
		if (open (FILE, $result_file))
		{	while (my $line = <FILE>)
			{	if ($line =~ /^\>(.*?) /)
				{	my $gpxref = $1;
					my $gpurl = case_replace($1);
					#	replace the gpxref with spaces
					$line = $space . substr($line, (length($gpxref) + 4) );
		
					my $linktext = 
					#	create an anchor
						'<span id="' . $gpurl
					#	. '-' . $seqno
						. '">&gt;</span>' . $gpxref .
					#	convert the gpxref and the name bit into a link to the gp details page
						' [<a href="gp-details.cgi?gp=' . $gpurl .'&amp;session_id=' . $session_id .'">details</a>] [<a href="gp-assoc.cgi?gp=' . $gpurl .'&amp;session_id=' . $session_id .'">associations</a>]';
						
					$line = $linktext . "\n" . $line;
		
					if ($parsed == 1)
					{	$parsed = 0; # turn off the line parser
					}
				}
				elsif ($line =~ /Sequences producing/) {
					$parsed = 2; # turn on the line parser
				}
				elsif ($parsed == 1 && $line =~ /\w/) {
					my @w = split(/\s+/, $line);
					my $gpxref = case_replace($w[0]);
					#	create a link to the relevant bit of the raw data
					$line =~ s/^(\Q$w[0]\E)/<a href="#$gpxref">$1<\/a>/i;
		
					$xrefs{ $gpxref } = 1;
					#	a list with the gpxref and the p score
					push @{$data->{gplist}}, [ $gpxref , $w[-2] ];
				} elsif ($parsed > 0 && $line !~ /\w/) {
					$parsed--;
				}
				$raw .= $line;
			}
			close FILE;
		}
		else
		{	$core->kvetch("Could not open result file $result_file: $!");
			$error = set_message($error, 'fatal', 'config_error', "Could not open results file: $!");
			return { error => $error };
		}
	}
	if (!$raw)
	{	$core->kvetch("blast.cgi: No raw data found!");
		$error = set_message($error, 'fatal', 'config_error', 'No data returned by BLAST');
		return { error => $error };
	}
	
#	$raw =~ s/^Warning\:\ no.*\n//;
#	$raw =~ s/^Thus\ no\ job.*//;
	$data->{raw_data} = $raw;

	if (!keys %xrefs)
	{	$core->kvetch("blast.cgi: No gene products found!");
		$error = set_message($error, 'fatal', 'no_blast_results');
		return { results => $data, error => $error };
	}

	my $option_h = { tmpl => { spp => 1, gptype => 1 } };
	if ($use_filters)
	{	$option_h->{use_filters} = 1;
		$option_h->{ignore_errors} = 1;
	}
	
	my $result_h = get_gp_details($apph, $error, { gpxref => [keys %xrefs] }, $option_h);

	my $product_h;
	map { $product_h->{ $_->xref->xref_dbname . ":" . $_->xref->xref_key } = $_ } @{$result_h->{results}};
	$error = $result_h->{error};

	$data->{product_h} = $product_h;
	my $omitted = (scalar keys %xrefs) - (scalar keys %$product_h);
	$core->kvetch("omitted: $omitted");
	$data->{omitted} = $omitted if ($omitted != 0);

#	$core->kvetch("results structure: ".Dumper($data));
	
	return
	{	results => $data, 
		error => $error,
		to_cache => { blast_scores => $data->{gplist}},
	};
}

=head2 is_na

Given an amino acid or nucleotide sequence, return 'n' if the
sequence contains no illegal letters, 0 otherwise.

See:

http://www.ncbi.nlm.nih.gov/BLAST/fasta.html

=cut


sub is_na {

	my $maybe = shift;
	if ($maybe =~ /[EFIJLOPQZ]/i) {
		#msglog("is_na: $maybe: NOT NUCLEOTIDE - return 0");
		return 0;
	}
	else {
		return 'n';
	}
}

=head2 is_aa

Given an amino acid or nucleotide sequence, return 'p' if the
sequence contains no illegal letters, 0 otherwise.

See:

http://www.ncbi.nlm.nih.gov/BLAST/fasta.html

=cut


sub is_aa {

	my $maybe = shift;

	if ($maybe =~ /[JO]/i) {
		#msglog("is_aa: $maybe: NOT PROTEIN - return 0");
		return 0;
	}
	else {
		return 'p';
	}
}

1;
