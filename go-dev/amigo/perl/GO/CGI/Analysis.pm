package GO::CGI::Analysis;

use Exporter;

@EXPORT = qw(check_sequence launch_job check_blast get_blast_results);
@ISA = qw(Exporter);

use strict;
use Carp;
use GO::Utils qw(rearrange);
use GO::CGI::NameMunger qw(case_replace);
use GO::CGI::Query qw(get_gp_details);
use DirHandle;
use FileHandle;

use Data::Dumper;
$Data::Dumper::Indent = 1;

=head1 SYNOPSIS

package GO::CGI::Analysis

=cut

sub check_sequence {
	my $session = shift;

	require GO::Model::Seq;

	my @inputs;
	foreach my $p qw(seq_id sptr_id upfile seq)
	{	if ($session->get_param($p))
		{	push @inputs, $p;
		}
	}

	if (scalar @inputs == 0)
	{	#$session->add_message('fatal', 'too_many_inputs');
		$session->suicide_message('no_input');
	}
	elsif (scalar @inputs > 1)
	{	$session->add_message('warning', 'too_many_inputs');
	}

	if ($session->get_param('seq_id') || $session->get_param('sptr_id')) {
		my $gps;
		my $id = $session->get_param('seq_id'); # this is from AmiGO
		if ($id)
		{	$gps = get_gp_details($session, { gpxref => $id }, { seq => 1 });
		}
		else
		{	$id = $session->get_param('sptr_id');
			$id =~ s/^.*[\|:]//;
			$gps = get_gp_details($session, { gpxref => "uniprot:".$id }, {seq=>1});
		}
		if ($gps->[0]->to_fasta) {
			$session->__set_param(-field=>'sequence',
										 -query=>'current_query',
										 -values=>[$gps->[0]->to_fasta]
										);
			$session->__set_param(-field=>'seq_id',
										 -query=>'current_query',
										 -values=>[ uc($gps->[0]->xref->xref_dbname)."|".$gps->[0]->xref->xref_key ]
										);
		} else {
		#	$session->__set_param(-field=>'deadly_seq_input_error',
		#								 -query=>'current_query',
		#								 -values=>['bad_seq_id']);
			$session->suicide_message('bad_seq_id');
		}
	} 
	#	uploaded sequences
	elsif ($session->get_param('seq') || $session->get_param('upfile')) {

#		get_sequence($session); # for upfile and seq

	my $sequence = "";

	if ($session->get_param('upfile')) {
		my $upfile = $session->get_cgi->param('upfile');
		my $seqline;
		while (defined($seqline = <$upfile>)) {
			$seqline =~ s/^[ \t]*//;
			$seqline =~ s/[ \t]*$//;
			$sequence .= $seqline;
		}
		# remove line feeds.
		$sequence =~ s/\x0D//g;
	} 
	elsif ($session->get_param('seq')) {
#		$sequence = "";
		my $seq = $session->get_param('seq');
		print STDERR "sequence from param seq:\n".Dumper($seq)."\n";
		
		foreach my $seqline( split "\n", $seq ) {
			$seqline =~ s/^[ \t]*//;
			$seqline =~ s/[ \t]*$//;
			
			print STDERR "line: $seqline\n";
			$sequence .= $seqline."\n";
		}
	}

	if (!$sequence)
	{	$session->suicide_message('no_input');
	}

	print STDERR "A: sequence: ".Dumper($sequence)."\n\n";
	if ($sequence =~ m/^>/) {
		$sequence =~ s/\s$//g;
		$sequence =~ s/\ //g unless ($sequence =~ /^>/);

	print STDERR "B: sequence: ".Dumper($sequence)."\n\n";
	} else {
		$sequence = ">your sequence\n$sequence";
		$sequence =~ s/\s$//g;
	print STDERR "C: sequence: ".Dumper($sequence)."\n\n";
	}
	$session->__set_param(-field=>'seq',
								 -query=>'current_query',
								 -values=>[$sequence]
								);
	$session->__set_param(-field=>'sequence',
								 -query=>'current_query',
								 -values=>[$sequence]
								);

	my $this_sequence = $session->get_param('seq');
	$this_sequence =~ s/^>.*?\n//;
	$this_sequence =~ s/\n>.*//s;
	print STDERR "D: sequence: ".Dumper($this_sequence)."\n\n";
		my $length = 0;
		eval {
			my $new_seq = $this_sequence;
			$new_seq =~ s/\s//g;
			chomp($new_seq);
			my $seq = GO::Model::Seq->new(-seq=>$new_seq);
			$length = $seq->length;
		#	if ($seq->length > $session->get_param('max_seq_length')) {
		#		$session->__set_param(-field=>'deadly_seq_input_error',
		#									 -query=>'current_query',
		#									 -values=>['seq_too_long']);
		#		$session->add_message('fatal', 'seq_too_long');
		#		$session->suicide_message('seq_too_long');
		#	}
		};
		if($@) {
		#	$session->__set_param(-field=>'deadly_seq_input_error',
		#								 -query=>'current_query',
		#								 -values=>['bad_seq']);
		#	$session->add_message('fatal', 'bad_seq');
			$session->suicide_message('bad_seq');
		}
		if ($length > $session->get_param('max_seq_length')) {
			$session->suicide_message('seq_too_long');
		}
	}
	return;
}

#still does checking but mainly set input sequence(s) into seq/sequence parameters!
sub get_sequence {
	my $session = shift;
	my $sequence = "";

	if ($session->get_param('upfile')) {
		my $upfile = $session->get_cgi->param('upfile');
		my $seqline;
		while (defined($seqline = <$upfile>)) {
			$seqline =~ s/^[ \t]*//;
			$seqline =~ s/[ \t]*$//;
			$sequence .= $seqline;
		}
		# remove line feeds.
		$sequence =~ s/\x0D//g;
	} 
	elsif ($session->get_param('seq')) {
#		$sequence = "";
		my $seq = $session->get_param('seq');
		print STDERR "sequence from param seq:\n".Dumper($seq)."\n";
		
		foreach my $seqline( split "\n", $seq ) {
			$seqline =~ s/^[ \t]*//;
			$seqline =~ s/[ \t]*$//;
			
			print STDERR "line: $seqline\n";
			$sequence .= $seqline."\n";
		}
	}

	print STDERR "A: sequence: ".Dumper($sequence)."\n\n";
	if ($sequence =~ m/^>/) {
#		#one more check too many seq and remove extra seq
#		if (0 && $sequence =~ m/\n>.*/s) { 
#			$sequence =~ s/\n>.*//s;
#	print STDERR "XXX: sequence: ".Dumper($sequence)."\n\n";
#			$session->__set_param(-field=>'sequence',
#										 -query=>'current_query',
#										 -values=>[$sequence]
#										);
#			$session->__set_param(-field=>'seq_input_error',
#										 -query=>'current_query',
#										 -values=>['too_many_seqs']
#										);
#			$session->add_message('warning', 'too_many_seqs');
#		}
		$sequence =~ s/\s$//g;
		$sequence =~ s/\ //g unless ($sequence =~ /^>/);

	print STDERR "B: sequence: ".Dumper($sequence)."\n\n";
	} else {
		$sequence = ">your sequence\n$sequence";
		$sequence =~ s/\s$//g;
	print STDERR "C: sequence: ".Dumper($sequence)."\n\n";
	}
	$session->__set_param(-field=>'seq',
								 -query=>'current_query',
								 -values=>[$sequence]
								);
	$session->__set_param(-field=>'sequence',
								 -query=>'current_query',
								 -values=>[$sequence]
								);
}

=head2 launch_job


=cut

sub launch_job {
	my $session = shift;

	print STDERR "Starting launchJob...\n";

#	my $seq = $session->get_param('sequence');
	## Save session to disk.
	my $file = new FileHandle;
	my $blast_dir = $session->get_blast_results_dir;
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
			$session->suicide_message("AmiGO configuration error: $@");
		}
	}

	my $program;
	my $max_seq_num = $session->get_param('max_seq_num');
	my $max_seq_len = $session->get_param('max_seq_length');
	my @seqs = ();
	my $seq_len = 0;
	my $s = 0;
	if ($session->get_param('seq_id') || $session->get_param('sptr_id')) {
		$program = "blastp";
		push @seqs, $session->get_param('sequence');
	} elsif ($session->get_param('seq')) {
		$program = get_program($session);

		my $sequence = $session->get_param('seq');
	#	my $sequence = $session->get_param('sequence');
		my @lines = split /\n/, $sequence;
		my $last_seq = "";
		for my $l (@lines) {
#			$l =~ s/\s$//g;
#			if ($l !~ /^>/)
#			{	$l =~ s/\ //g;
#			}
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
	}
	
	print STDERR "seq list:\n".join("\n", @seqs)."\n";
	
	if (scalar(@seqs) > $max_seq_num or $seq_len > $max_seq_len) {
		pop @seqs;
		$session->add_message('warning', 'too_many_seqs');
		$session->__set_param(-field=>'seq_input_error',
									-query=>'current_query',
									-values=>['too_many_seqs']
									);
	}

	$session->__set_param(-field=>'n_seqs', -query=>'1', -values=>[scalar(@seqs)]);

	my $i = 0;
	my $cmd_file = "$blast_dir/command.sh";
	my $program_bin = $session->get_param($program);
	my $fasta_db = $session->get_param('fasta_db');
	my $threshold = $session->get_param('threshold');
	my $bfilter_opt = "-filter SEG";
	my $ncpus = 1; #restrict blast load on servers
	$bfilter_opt = "" if ($session->get_param('blast_filter') && $session->get_param('blast_filter') eq 'off');
	my $hits = $session->get_param('maxhits') || 50;
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
		$file->print("$program_bin $fasta_db $seq_file -v$hits -b$hits $bfilter_opt -e$threshold -cpus=$ncpus> $result_tmp 2>$error\n");
		$file->print("mv $result_tmp $result\n");
		$file->print("chmod a+r $result\n");
		$file->print("chmod a+r $error\n");
		$file->close;
	}
	`chmod a+r $cmd_file`;
#	$session->__save_params(-query=>'1',
#							-params=>['n_seqs']);

	print STDERR "finished setting all the settings\n";

	if (!defined $ENV{GO_BLAST_METHOD}) {
		$ENV{GO_BLAST_METHOD} = 'pbs';
	}
		
	if (lc($ENV{GO_BLAST_METHOD}) eq 'pbs') {
		return unless ($program_bin && $fasta_db && defined($threshold));
		return if ($program =~ /[^\w\/.]/);
		return if ($fasta_db =~ /[^\w\/.]/);
		return if ($threshold =~ /[^\w.]/);

		#launch the job

		my $qsub = $session->get_param('qsub');
		my $pbs_user = $session->get_param('pbs_user') || 'public';
		my $queue = $session->get_param('queue');

		if ($qsub && $queue) {
			if (!($qsub =~ /[^\w\/.]/)) {
				if (!($queue =~ /[^\w\\\/\@]/)) {
					my $job = `$qsub -u $pbs_user -q $queue $cmd_file`;
					chomp $job;
					confess("qsub failed to submit blast job to cluster") unless ($job);
					$session->__set_param(-field=>'last_job',
										  -query=>'1',
										  -values=>[$job]
										 );
					$session->__save_params(-query=>'1', -params=>['last_job']);
				}
			}
		}
	} elsif (lc($ENV{GO_BLAST_METHOD}) eq 'cgi') {
		require LWP::UserAgent;

		return unless ($program_bin && $fasta_db && $threshold);

		my $ua = LWP::UserAgent->new;
		my $blast_url = $ENV{GO_BLAST_URL};
		my %form = (
				#	data_dir => $data_dir,
					blast_dir => $blast_dir,
				   );

		print STDERR "starting post at ".localtime()."\n";
		my $response = $ua->post($blast_url, \%form);
		print STDERR "finished post at ".localtime()."\n";
	} else {
		# throw an error
		print STDERR "Unknown GO_BLAST_METHOD specified in ENV{GO_BLAST_METHOD}: ".$ENV{GO_BLAST_METHOD}."\n";
		return;
	}
#}
	return 1;
}

sub get_program {
	my $session = shift;
	my @lines = split '\n', $session->get_param('seq');
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
	my $session = shift;
	my $seqno = $session->get_param('n_seqs') || 1;
	my $result_file = $session->get_blast_results_dir."/result.$seqno";

	if(-f $result_file) {
		return 1;
	} else {
		return 0;
	}
}

sub get_blast_results {
	my $session = shift;
	my $apph = $session->apph;
	my $blast_dir = $session->get_blast_results_dir;
	my $seqno = $session->get_param('page') || 1;
	my $result_file = "$blast_dir/result.$seqno";
	my $seq_file = "$blast_dir/current_seq.$seqno";

	my $data;
	my %xrefs;
	my $space = '            ';
	my $results;

# cut this for now. multiple blast results on one page is TMI!
	#	page is an alias for the sequence number
#	if ($session->get_param('page')) {
#		if ($session->get_param('page') =~ /^all/i) {
#			push @{$data->{'output_seq_nums'}}, (1..$session->get_param('n_seqs'));
#		} else {
#			push @{$data->{'output_seq_nums'}}, $session->get_param('page');
#		}
#	} else {
#		push @{$data->{'output_seq_nums'}}, 1;
#	}
#	
#	foreach my $seqno (@{$data->{'output_seq_nums'}})
#	{	

	my $seq;
	open (SEQFILE, $seq_file) || $session->suicide_message("Could not open results file: $!");
	while (my $line = <SEQFILE>) {
		$seq .= $line;
	}
	close SEQFILE;
	$results->{sequence} = $seq;

	my $raw;
	if (-f $result_file)
	{
	}
	else
	{	$session->add_message('fatal', 'The BLAST results file could not be found.');
		print STDERR "No result file found!\n";
		return;
	}
	
	my $parsed = 0;
	open (FILE, $result_file) || $session->suicide_message("Could not open results file: $!");
	while (my $line = <FILE>) {
		if ($line =~ /^\>(.*?) /) {
			my $gpxref = $1;
			my $gpurl = case_replace($1);
			#	replace the gpxref with spaces
			$line = $space . substr($line, (length($gpxref) + 4) );

			my $linktext = 
			#	create an anchor
				'<span id="' . $gpurl
			#	. '-' . $seqno
				. '">&gt;</span>' . $gpxref .
			#	convert the gpxref and the name bit into a link to the gp details page
				' [<a href="gp-details.cgi?gp=' . $gpurl .'&amp;session_id=' . $session->get_param('session_id').'">details</a>] [<a href="gp-assoc.cgi?gp=' . $gpurl .'&amp;session_id=' . $session->get_param('session_id').'">associations</a>]';
				
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
			push @{$results->{gplist}}, [ $gpxref , $w[-2] ];
		} elsif ($parsed > 0 && $line !~ /\w/) {
			$parsed--;
		}
		$raw .= $line;
	}
	close FILE;
	if (!$raw)
	{	$session->add_message('warning', 'No data returned by BLAST');
		return $data;
	}
	
	$raw =~ s/^Warning\:\ no.*\n//;
	$raw =~ s/^Thus\ no\ job.*//;
	$results->{raw_data} = $raw;
#	$data->{$seqno} = $results;
	$data->{results} = $results;
#	}

	my $tmpl = { no_seq => 1 };
	if ($session->get_param('use_filters') && $session->get_param('use_filters') == 1)
	{	$tmpl = { no_seq => 1, use_filters => 1 };
	}
	
	my $product_list = get_gp_details($session, { gpxref => [keys %xrefs] }, $tmpl);
	
	my $product_h;
	
	map { $product_h->{ $_->xref->xref_dbname . ":" . $_->xref->xref_key } = $_ } @$product_list;
	
	$data->{product_h} = $product_h;
	
	$session->set_all_caching_params({ blast_scores => $results->{gplist}});

	return $data;
}

sub get_graph_from_blast_results_old {
	my $session = shift;
	my $raw = shift;
	my $apph = $session->apph; 
	my @lines = split(/\n/, $raw);
	my $in_summary = 0;

	my @symbols = ();
	my @xrefs = ();
	my @id_symbols = ();
	my $symbol_h = {};
	my $score_h = {};
	my $products = {};
	foreach (@lines) {
		chomp;
		if (!$_) {
			next;
		}
		if (/^Query/) {
			next;
		}
		if (/^\>/) {
			last;
		}
		if (/Sequences producing/) {
			$in_summary = 1;
			next;
		}
		else {
			my @w = split;
			my ($n, $p, $score, @rest) = reverse @w;
			my $hit = join(" ", @rest);
			if ($hit =~ /symbol:(\S+)/) {
				my $s = $1;
				push(@symbols, $s);
				$symbol_h->{$s} = {score=>$score};
				$score_h->{$s} = $p;
				my ($ds, $acc) = split('\|', $rest[-1]);
				if ($ds && $acc) {
	my @refs = ();
					push @xrefs, {xref_key=>$acc,xref_dbname=>$ds};
	push @refs, {xref_key=>$acc,xref_dbname=>$ds};
	my $prods = $apph->get_products({dbxrefs=>[@refs]});
	$products->{$s} = $prods;
					push @id_symbols, {id=>lc("$ds:$acc"),symbol=>$s};
				}
			}
		}
	}
	my ($terms, $prods);
	eval {
				if (@xrefs) {
						$prods = $apph->get_products({dbxrefs=>[@xrefs]});
						$terms = $apph->get_terms({products=>$prods});#[@symbols]});
				}
#		 $terms = $apph->get_terms({products=>$prods});
	};
	if (!$terms) {
		return undef;
	}

	my %asso_h;
	map{map{$asso_h{$_->id}=$_;}@{$_->selected_association_list || []}}@$terms;
	$apph->get_qualifiers([values %asso_h]);
	$apph->get_assigned_by([values %asso_h]);

#	 my $graph = $apph->get_graph_by_terms($terms, 0);
#	 $apph->filters({});
	my %result;
#	 $result{'graph'} = $graph;
	$result{'score_h'} = $score_h;
	
#	from hash struct, we need:
#	GP model
	$result{'hash_struct'} = GO::CGI::Query->_make_hash_struct_old($terms);
	$result{'id_symbols'} = \@id_symbols;

	$result{'prods'} = $products;


	return \%result;
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
