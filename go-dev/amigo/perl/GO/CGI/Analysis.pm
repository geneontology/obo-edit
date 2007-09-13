package GO::CGI::Analysis;

use strict;
use Carp;
use GO::Utils qw(rearrange);
use GO::CGI::Utilities qw(:std);
use GO::CGI::NameMunger qw(case_replace);
use GO::CGI::Query qw(get_gp_details);
use DirHandle;
use FileHandle;
use Exporter;
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);
@ISA = ('Exporter');
@EXPORT_OK = qw(set_off_blast launch_job check_blast get_blast_results);
%EXPORT_TAGS = (
	all => [@EXPORT_OK],
);

use Data::Dumper;
$Data::Dumper::Indent = 1;

=head1 SYNOPSIS

package GO::CGI::Analysis

=cut

=head2 set_off_blast

	Launches the BLAST query.

	Usage:

	my $result_h = set_off_blast($apph, $msg_h, $blast_dir, 
	                  $inputs,        # hash containing input seqs
	                  $blast_options  # values for threshold, maxhits,
	                                  # blast_filter and boolean use_filters
	
	$result_h is of the form
	
	$result_h->{msg_h}     # contains fatal/warning/info messages
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
	my $msg_h = shift;
	my $blast_dir = shift;      # where the results will go
	my $inputs = shift;         # hash containing the input seqs
	my $blast_options = shift;  # from the CGI

	foreach ($apph, $msg_h, $inputs, $blast_options, $blast_dir)
	{	print STDERR Dumper($_)."\n";
	}

	my $max_seq_num = get_environment_param('max_seq_num') || 100;
	my $max_seq_len = get_environment_param('max_seq_length') || 3000000;

	my @seqs;       # array for storing our checked sequences
	my $input_seqs; # store the sequence source (e.g. seq ID or 
	                # text entered into box) and the sequence itself
	my $program;    # which blast program to use

	require GO::Model::Seq;

	#	check we have some inputs
	if (scalar keys %$inputs == 0)
	{	print STDERR "No inputs found. Quitting...\n";
		$msg_h = set_message($msg_h, 'fatal', 'no_input');
		return { msg_h => $msg_h };
	}
	elsif (scalar keys %$inputs > 1)
	{	print STDERR "Too many inputs found! Warning.\n";
		$msg_h = set_message($msg_h, 'warning', 'too_many_inputs');
	}


	if (!$apph || !$blast_dir)
	{	print STDERR "Missing required input apph / blast_dir\n";
		$msg_h = set_message($msg_h, 'fatal', 'config_error', "No apph / user BLAST directory found!");
		return { msg_h => $msg_h };
	}


=cut
	#	check we've got a blast directory
	if (!defined $blast_dir)
	{	print STDERR "No user BLAST directory found.\n";
		$msg_h = set_message($msg_h, 'fatal', 'config_error', "No user BLAST directory found!");
		return { msg_h => $msg_h };
	}
=cut

	#	check the DB is there...
	my $fasta_db = get_environment_param('fasta_db');
	if (!$fasta_db)
	{	print STDERR "FASTA db not found at $fasta_db!\n";
		$msg_h = set_message($msg_h, 'fatal', 'config_error', "FASTA db not found at $fasta_db!");
		return { msg_h => $msg_h };
	}

	#	check we've got both the blast programs
	foreach ('blastp', 'blastx')
	{	my $bin = get_environment_param($_);
		if (! defined $bin || !$bin)
		{	print STDERR "$_ not defined or not found!\n";
			$msg_h = set_message($msg_h, 'fatal', 'config_error', "$_ not found!");
			return { msg_h => $msg_h };
		}
	}

	#	check the prerequisites for our submission methods are there
	my $blast_method = get_environment_param('blast_method');
	if (!defined $blast_method) {
		$blast_method = 'pbs';
	}

	#	check our blast submission method is OK
	if (lc($blast_method) eq 'pbs')
	{	my $qsub = get_environment_param('qsub');
		my $queue = get_environment_param('queue');

		if (!$qsub || !$queue || $qsub =~ /[^\w\/.]/ || $queue =~ /[^\w\\\/\@]/)
		{	print STDERR "Problem with PBS configuration\n";
			$msg_h = set_message($msg_h, 'fatal', "config_error", "Problem with PBS configuration");
			return { msg_h => $msg_h };
		}
	}
	elsif (lc($blast_method) ne 'cgi')
	{	# unknown input method - throw an error
		print STDERR "Unknown blast method specified: $blast_method\n";
		$msg_h = set_message($msg_h, 'fatal', 'config_error', "Unknown GO_BLAST_METHOD specified: $blast_method\n");
		return { msg_h => $msg_h };
	}

	#	OK, let's get going on the query now!
	if ($inputs->{seq_id} || $inputs->{sptr_id}) {
		my $gps;
		my $id = $inputs->{seq_id}; # this is from an AmiGO BLAST link
		if (!$id)
		{	$id = $inputs->{sptr_id};
			$id =~ s/^.*[\|:]//;
			$id = "UniProt:".$id;
		}
		my $result_h = get_gp_details($apph, $msg_h, { gpxref => $id }, { tmpl => { seq => 1 } });
		return { msg_h => $msg_h } if !$result_h->{results};  # no GPs found
		$msg_h = $result_h->{msg_h};
		$gps = $result_h->{results};
		
		if ($gps->[0]->to_fasta)
		{	$input_seqs = {
				type => 'seq_id',
				seq_id => uc($gps->[0]->xref->xref_dbname)."|".$gps->[0]->xref->xref_key,
				seqs => [ $gps->[0]->to_fasta ],
			};
		}
		else
		{	print STDERR "Bad seq ID - GP does not have a sequence\n";
			$msg_h = set_message($msg_h, 'fatal', 'no_seq_found', $input_seqs->{seq_id});
			return { msg_h => $msg_h };
		}
		$program = 'blastp';
		push @seqs, @{$input_seqs->{seqs}};
	} 
	elsif ($inputs->{seq} || $inputs->{upfile})
	{	#	uploaded sequences
		my $sequence = "";
	
		if ($inputs->{upfile})
		{	print STDERR "Uploaded file found: ".$inputs->{upfile}."\n";
			my $upfile = $inputs->{upfile};
			my $seqline;
			while (defined($seqline = <$upfile>))
			{	$seqline =~ s/^[ \t]*//;
				$seqline =~ s/[ \t]*$//;
				$sequence .= $seqline;
			}
			# remove line feeds.
			$sequence =~ s/\x0D//g;
		} 
		elsif ($inputs->{seq}) {
			my $seq = $inputs->{seq};
			print STDERR "sequence from param seq:\n".Dumper($seq)."\n";
			
			foreach my $seqline( split "\n", $seq )
			{	$seqline =~ s/^[ \t]*//;
				$seqline =~ s/[ \t]*$//;
				
				print STDERR "line: $seqline\n";
				$sequence .= $seqline."\n";
			}
		}
	
		if (!$sequence)
		{	print STDERR "No actual sequence found.\n";
			$msg_h = set_message($msg_h, 'fatal', 'no_input');
			return { msg_h => $msg_h };
		}
	
		print STDERR "A: sequence: ".Dumper($sequence)."\n\n";
		if ($sequence =~ m/^>/)
		{	$sequence =~ s/\s$//g;
			$sequence =~ s/\ //g unless ($sequence =~ /^>/);
	
			print STDERR "B: sequence: ".Dumper($sequence)."\n\n";
		}
		else 
		{	$sequence = ">your sequence\n$sequence";
			$sequence =~ s/\s$//g;
			print STDERR "C: sequence: ".Dumper($sequence)."\n\n";
		}
		$input_seqs = {
			type => 'direct',
			direct => $sequence,
		};

		my $this_sequence = $sequence;
		$this_sequence =~ s/^>.*?\n//;
		$this_sequence =~ s/\n>.*//s;
		print STDERR "D: sequence: ".Dumper($this_sequence)."\n\n";

		#	check the sequence isn't too long
		my $length = 0;
		eval {
			my $new_seq = $this_sequence;
			$new_seq =~ s/\s//g;
			chomp($new_seq);
			my $seq = GO::Model::Seq->new(-seq=>$new_seq);
			$length = $seq->length;
		};
		if($@) {
			$msg_h = set_message($msg_h, 'fatal', 'bad_seq', $@);
			return { msg_h => $msg_h };
		}
		if ($length > $max_seq_len) {
			$msg_h = set_message($msg_h, 'fatal', 'seq_too_long');
			return { msg_h => $msg_h };
		}

		$program = get_program($sequence);
		my @lines = split /\n/, $sequence;
		my $last_seq = "";
		my $s = 0;
		my $seq_len = 0;
		for my $l (@lines) {
			next unless ($l);
			if ($l =~ /^>/) {
				if ($last_seq) {
					push @seqs, $last_seq;
					$last_seq = "";
					last if (++$s > $max_seq_num or $seq_len > $max_seq_len);
				}
			} else {
				$seq_len += length($l);
			}
			$last_seq .= "$l\n";
		}
		push @seqs, $last_seq if ($last_seq);

		if (scalar(@seqs) > $max_seq_num or $seq_len > $max_seq_len) {
			pop @seqs;
			print STDERR "too many seqs or seq length exceeded max length\n";
			$msg_h = set_message($msg_h, 'warning', 'too_many_seqs');
		}

		if (!@seqs)
		{	print STDERR "No sequences found.\n";
			$msg_h = set_message($msg_h, 'fatal', 'no_input');
			return { msg_h => $msg_h };
		}

		$input_seqs->{seqs} = [@seqs];
	}

	#	All is well with the sequence(s).
	#	Now let's go on to launching the job.

	print STDERR "Starting launchJob...\n";

	## Save session to disk.
	my $file = new FileHandle;
	print STDERR "Eval-ing the blast_dir command\n";
	eval {
		#	`find $data_dir/$blast_dir -name "*" -exec rm -f {} \\;`;
			`find $blast_dir -name "*" -exec rm -rf {} \\;`;
	};
	
	if ($@)
	{	print STDERR "Error: $@";
	}
	
	if (!new DirHandle($blast_dir)) {
		print STDERR "Eval-ing the create blast_dir command\n";
		eval {
			mkdir($blast_dir, 0755);
			`chmod a+rw $blast_dir`;
		};
		if ($@)
		{	print STDERR "Error: $@";
			$msg_h = set_message($msg_h, 'fatal', 'config_error', $@);
			return { msg_h => $msg_h };
		}
	}

	my $i = 0;
	my $cmd_file = "$blast_dir/command.sh";
	my $program_bin = get_environment_param($program);
	
	foreach my $p qw(maxhits threshold blast_filter)
	{	if (!$blast_options->{$p})
		{	$blast_options->{$p} = get_environment_param($p);
		}
	}
	my $bfilter_opt = " ";
	$bfilter_opt = " -filter SEG" if ($blast_options->{blast_filter} eq 'on');
	my $ncpus = 1; #restrict blast load on servers

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
#		$file->print("WUBLASTFILTER=/usr/local/bdgp/wublast/filter; export WUBLASTFILTER\n");
		$file->print("$program_bin $fasta_db $seq_file"
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

	print STDERR "finished setting all the settings\n";

	if (lc($blast_method) eq 'pbs')
	{	my $qsub = get_environment_param('qsub');
		my $pbs_user = get_environment_param('pbs_user') || 'public';
		my $queue = get_environment_param('queue');

		#launch the job
		my $job = `$qsub -u $pbs_user -q $queue $cmd_file`;
		chomp $job;
		if (!$job)
		{	print STDERR "qsub failed to submit blast job to cluster!\n";
			$msg_h = set_message($msg_h, 'fatal', 'config_error', "qsub failed to submit blast job to cluster!\n");
			return { msg_h => $msg_h };
		}
	}
	elsif (lc($blast_method) eq 'cgi')
	{	require LWP::UserAgent;
		my $ua = LWP::UserAgent->new;
		my $blast_url = get_environment_param('blast_url');
		my %form = ( blast_dir => $blast_dir );

		print STDERR "starting post at ".localtime()."\n";
		my $response = $ua->post($blast_url, \%form);
		print STDERR "finished post at ".localtime()."\n";
	}
	return { msg_h => $msg_h, results => $input_seqs };
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

sub check_blast {
	my $blast_results_dir = shift;
	my $n_seqs = shift || 1;
	my $result_file = "$blast_results_dir/result.$n_seqs";

	print STDERR "looking for $result_file...\n";

	if(-f $result_file) {
		return 1;
	} else {
		return 0;
	}
}

=head2 get_blast_results

	Parses the BLAST results file and returns the results shown by AmiGO
	
	Usage:

	my $result_h = get_blast_results($apph, $msg_h, $blast_dir, 
	                  $seqno,         # which of the uploaded sequences to
	                                  # retrieve results for (defaults to 1)
	                  $session_id,
	                  $use_filters);  # whether or not to filter the gene
	                                  # products displayed in the results
	
	$result_h is of the form
	
	$result_h->{msg_h}     # contains fatal/warning/info messages
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
	my $msg_h = shift;
	my $blast_dir = shift;
	my $seqno = shift;
	my $session_id = shift;
	my $use_filters = shift;
	$seqno = 1 if (!$seqno || $seqno =~ /\D/);
	my $result_file = "$blast_dir/result.$seqno";
	my $seq_file = "$blast_dir/current_seq.$seqno";

	if (!$apph || !$session_id || !$blast_dir || !$result_file || !$seq_file)
	{	print STDERR "Missing apph / session_id / blast_dir / result file / sequence file\n";
		$msg_h = set_message($msg_h, 'fatal', 'config_error', "Missing apph / session_id / blast_dir / result file / sequence file");
		return { msg_h => $msg_h };
	}

	my $data;
	my %xrefs;
	my $space = '            ';

	my $seq;
	if (open (SEQFILE, $seq_file))
	{	while (my $line = <SEQFILE>) {
			$seq .= $line;
		}
		close SEQFILE;
		$data->{sequence} = $seq;
	}
	else
	{	print STDERR "Could not open sequence file $seq_file: $!\n";
		$msg_h = set_message($msg_h, 'fatal', 'config_error', "Could not open sequence file: $!");
		return { msg_h => $msg_h };
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
		{	print STDERR "Could not open result file $result_file: $!\n";
			$msg_h = set_message($msg_h, 'fatal', 'config_error', "Could not open results file: $!");
			return { msg_h => $msg_h };
		}
	}
	if (!$raw)
	{	print STDERR "No raw data found!\n";
		$msg_h = set_message($msg_h, 'fatal', 'config_error', 'No data returned by BLAST');
		return { msg_h => $msg_h };
	}
	
	$raw =~ s/^Warning\:\ no.*\n//;
	$raw =~ s/^Thus\ no\ job.*//;
	$data->{raw_data} = $raw;

	if (!keys %xrefs)
	{	print STDERR "No gene products found!\n";
		$msg_h = set_message($msg_h, 'fatal', 'no_results');
		return { results => $data, msg_h => $msg_h };
	}

	my $option_h = { tmpl => { spp => 1, gptype => 1 } };
	if ($use_filters)
	{	$option_h->{use_filters} = 1;
		$option_h->{ignore_errors} = 1;
	}
	
	my $result_h = get_gp_details($apph, $msg_h, { gpxref => [keys %xrefs] }, $option_h);

	my $product_h;
	map { $product_h->{ $_->xref->xref_dbname . ":" . $_->xref->xref_key } = $_ } @{$result_h->{results}};
	$msg_h = $result_h->{msg_h};

	$data->{product_h} = $product_h;
	my $omitted = (scalar keys %xrefs) - (scalar keys %$product_h);
	print STDERR "omitted: $omitted\n";
	$data->{omitted} = $omitted if ($omitted != 0);

#	print STDERR "results structure: ".Dumper($data)."\n";
	
	return
	{	results => $data, 
		msg_h => $msg_h,
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
