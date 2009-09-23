#!/usr/bin/perl -w
# remap terms to their nearest and dearest GO slim terms

=head1 NAME

go-slim-annotations.pl - remap annotations to terms in a selected subset

=head1 SYNOPSIS

 go-slim-annotations.pl -i gene_ontology.obo -s goslim_generic -g my_associations.gaf -o remapped.gaf

=head1 DESCRIPTION

Insert description here.

=head2 Input parameters

=head3 Required

=over

=item -i || --ontology /path/to/obo_file

input file (ontology file in OBO format) with path

=item -g || --gene_association /path/to/gaf_file

input gene association file with path

=item -o || --output /path/to/file_name

output file with path

=item  -s || --subset I<subset_name>  OR  -t || --termfile /path/to/file_name

The subset can be specified using either the -s option, which will extract terms
where the subset matches I<subset_name>, or using a separate file of containing
the terms to be extracted. This can be a file in OBO format, or a plain text list
of IDs with each ID on a separate line. In the latter case, you can additionally
specify a regular expression for that term IDs must match using the B<-r> or
B<--term_regexp> option. For example:

 -t term_list.txt -r "GO:\d+"

This would extract all terms matching the regular expression C</GO:\d+/> in the
file C<term_list.txt>.

=back

=head3 Optional switches

=over

=item -c || --count

Show annotation counts to subset terms, rather than producing a new gene
association file

=item -n || --show_names

Show term names (in addition to IDs) in the annotation counts file. Only
works in conjunction with the B<--count> [above].

=item -v || --verbose

prints various messages

=back

=head2 Output

go-slim-annotations.pl has two different modes of operation, B<count mode> and
B<remapping mode>.

=head3 Count mode

The number of annotations that remap to each slim term are reported.

=head3 Remapping mode

If you do not specify the B<-c> or B<--count> option, the output will be a
gene association file with the original annotations remapped to subset terms.

=head1 AUTHOR

Amelia Ireland

=head1 SEE ALSO

L<GOBO::Graph>, L<GOBO::InferenceEngine>, L<GOBO::Doc::FAQ>

=cut

use strict;
use FileHandle;
use Carp;
use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use GOBO::Graph;
use GOBO::Parsers::OBOParserDispatchHash;
use GOBO::InferenceEngine;
use GOBO::Util::GraphFunctions;

=cut

options offered by online slimmer:

The gene product counts option is the default result type and what most people find useful.

The gene association file option does pretty much what you'd expect and generates a gene association file view of the results.

=cut

my $options = parse_options(\@ARGV);

my $data;
my $trimmed;
my $subset;
my $graph;

if ($options->{mapping_file})
{	$data = GOBO::Util::GraphFunctions::load_mapping({ mapping_file => $options->{mapping_file} });
	$trimmed->{graph} = $data->{graph};
}
else
{	# before we check out the graph, let's see if the file actually contains any terms...
	if ($options->{termlist})
	{	$data->{to_find} = get_subset_terms($options);
	}

	# parse the input file and check we get a graph
	my $parser = new GOBO::Parsers::OBOParserDispatchHash(file=>$options->{input},
	options => {
		body => {
			parse_only => {
				term => [ qw( name namespace relation is_a subset is_obsolete ) ],
				typedef => '*',
			},
		},
	});
	$parser->parse;

	die "Error: parser could not find a graph in " . $options->{input} . "!\nDying" unless $parser->graph;
	print STDERR "Finished parsing file " . $options->{input} . "\n" if $options->{verbose};

	$graph = $parser->graph;

	# get the nodes matching our subset criteria
	if ($options->{subset})
	{	$data = GOBO::Util::GraphFunctions::get_subset_nodes({ graph => $graph, options => $options });
		print STDERR "Done GOBO::Util::GraphFunctions::get_subset_nodes!\n" if $options->{verbose};

		# move the subset to $subset
		foreach my $s (keys %{$data->{subset}})
		{	$subset = $data->{subset}{$s};
		}
	}
	else
	{	my @fail;
		# we got the terms from a file. Check that they exist in our graph!
		foreach (keys %{$data->{to_find}})
		{	if ($graph->get_term($_) && ! $graph->get_term($_)->obsolete)
			{	$subset->{$_} = 1;
			}
			else
			{	push @fail, $_;
			}
		}
		if (! $subset || ! keys %$subset)
		{	my $msg = "None of the terms specified in " . $options->{termlist} . " were found in the ontology file.";
			if ($options->{verbose})
			{	die "$msg mapmaker.pl was looking for the following terms:\n" . join(", ", sort keys %{$options->{termlist}}) . "\nPlease try again.\nDying";
			}
			else
			{	die "$msg Please try again.\nDying";
			}
		}
		## make sure that we have the root nodes
		map { $subset->{ $_->id }++ } @{$graph->get_roots};
	}

	if (scalar keys %$subset < 5 && ! $options->{force_continue})
	{	print STDERR "subset keys found:\n" . join("\n", keys %$subset) . "\n" if $options->{verbose};
		die "Only " . (scalar keys %$subset) . " terms from the subset specified could be found. To continue anyway, please run the script again with the extra command line parameter -f\nDying";
	}

	# get the relations from the graph
	$data->{relations} = GOBO::Util::GraphFunctions::get_graph_relations({ graph => $graph, options => $options });
		print STDERR "Done GOBO::Util::GraphFunctions::get_graph_relations!\n" if $options->{verbose};

	my $ie = new GOBO::InferenceEngine(graph => $graph);

	# get the links between the nodes
	$data->{nodes} = GOBO::Util::GraphFunctions::get_graph_links({ inf_eng => $ie, subset => $subset, graph => $graph, options => $options });
	print STDERR "Done GOBO::Util::GraphFunctions::get_graph_links!\n" if $options->{verbose};

	# populate the node look up hashes
	GOBO::Util::GraphFunctions::populate_lookup_hashes({ graph_data => $data->{nodes} });
	print STDERR "Done GOBO::Util::GraphFunctions::populate_lookup_hashes!\n" if $options->{verbose};

	# remove redundant relationships between nodes
	GOBO::Util::GraphFunctions::remove_redundant_relationships({ node_data => $data->{nodes}, rel_data => $data->{relations}, graph => $graph, options => $options });
	print STDERR "Done GOBO::Util::GraphFunctions::remove_redundant_relationships!\n" if $options->{verbose};

	# repopulate the node look up hashes
	GOBO::Util::GraphFunctions::populate_lookup_hashes({ graph_data => $data->{nodes} });
	print STDERR "Done GOBO::Util::GraphFunctions::populate_lookup_hashes!\n" if $options->{verbose};

	# slim down dem nodes
	$trimmed = GOBO::Util::GraphFunctions::trim_graph({ graph_data => $data->{nodes}, options => $options });
	print STDERR "Done GOBO::Util::GraphFunctions::trim_graph!\n" if $options->{verbose};
}

## get the GA data
my $assoc_data = quickparse_gaf({ options => $options });

print STDERR "Done quickparse_gaf!\n" if $options->{verbose};

## check term remappings
my @errs;
my $remap;
my $new_assoc_data;

foreach my $t (keys %$assoc_data)
{	# check term exists in slimmed graph
	if (! $trimmed->{graph}{$t})
	{	push @errs, $t;
		$remap->{missing}{$t} += scalar keys %{$assoc_data->{$t}};
		next;
	}
	# get all the ancestors of $t, regardless of rlnship
	foreach my $rel (keys %{$trimmed->{graph}{$t}})
	{	foreach (keys %{$trimmed->{graph}{$t}{$rel}})
		{	$remap->{n_t}{$t}{$_}++;
			push @{$remap->{t_n}{$_}}, $t;
		}
	}

#	print STDERR "$t remapped to " . join(", ", keys %{$remap->{n_t}{$t}} ) . "\n";
	## move the associations to the new terms
	map
	{	my $remapped = $_;
		$new_assoc_data->{$remapped} = { %{$new_assoc_data->{$remapped} || {} }, %{$assoc_data->{$t}} };
#		map
#		{
#			$new_assoc_data->{$remapped}{$_} = $assoc_data->{$t}{$_}
#		} keys %{$assoc_data->{$t}};
	} keys %{$remap->{n_t}{$t}};
}

if (@errs)
{	warn "The following terms were not found in the list of slimmed terms:\n"
	. join("\n",
	map { $_ . ": " . $remap->{missing}{$_} } @errs) . "\n\n\n";
}

## now print out the results
if ($options->{rewrite})
{	print_gaf_file( $new_assoc_data, $options );
}
elsif ($options->{count})
{	print_counts( $new_assoc_data, $options, $graph );
}

print STDERR "Job complete!\n" if $options->{verbose};

exit(0);

###

# parse the options from the command line
sub parse_options {
	my $args = shift;

	my $opt;

	while (@$args && $args->[0] =~ /^\-/) {
		my $o = shift @$args;
		if ($o eq '-i' || $o eq '--ontology') {
			if (@$args && $args->[0] !~ /^\-/)
			{	$opt->{input} = shift @$args;
			}
		}
		elsif ($o eq '-s' || $o eq '--subset') {
			while (@$args && $args->[0] !~ /^\-/)
			{	my $s = shift @$args;
				$opt->{subset}{$s}++;
			}
		}
		elsif ($o eq '-t' || $o eq '--termlist') {
			## this should be the name of a file with a list of terms
			$opt->{termlist} = shift @$args if @$args && $args->[0] !~ /^\-/;
		}
		elsif ($o eq '-r' || $o eq '--term_regexp') {
			## a regular expression for isolating terms from a text file
			$opt->{term_regexp} = shift @$args if @$args && $args->[0] !~ /^\-/;
		}
		### load the slim data from a mapping file
		elsif ($o eq '-m' || $o eq '--mapping_file') {
			$opt->{mapping_file} = shift @$args if @$args && $args->[0] !~ /^\-/;
		}
		### annotation input options
		elsif ($o eq '-g' || $o eq '--gene_association') {
			$opt->{ga_input} = shift @$args if @$args && $args->[0] !~ /^\-/;
		}
		### output
		elsif ($o eq '-o' || $o eq '--output') {
			$opt->{output} = shift @$args if @$args && $args->[0] !~ /^\-/;
		}
		### action options
		elsif ($o eq '-c' || $o eq '--count') {
			$opt->{count} = 1;
		}
#		elsif ($o eq '-b' || $o eq '--buckets') {
#			$opt->{buckets} = 1;
#		}
		elsif ($o eq '-n' || $o eq '--show_names') {
			$opt->{show_names} = 1;
		}
		elsif ($o eq '-h' || $o eq '--help') {
			system("perldoc", $0);
			exit(0);
		}
		elsif ($o eq '-v' || $o eq '--verbose') {
			$opt->{verbose} = 1;
		}
		else {
			die "Error: no such option: $o\nThe help documentation can be accessed with the command 'go-slimdown.pl --help'\n";
		}
	}
	return check_options($opt);
}


## process the input params
sub check_options {
	my $opt = shift;
	my $errs;

	if (!$opt)
	{	die "Error: please ensure you have specified a gene association file, an ontology file, and a subset.\nThe help documentation can be accessed with the command 'go-slim-annotations.pl --help'\nDying";
	}

	if (!$opt->{ga_input})
	{	push @$errs, "specify a gene association file using -i /path/to/<file_name>";
	}
	elsif (! -e $opt->{ga_input})
	{	push @$errs, "the file " . $opt->{input} . " could not be found.\n";
	}

	if ($opt->{mapping_file})
	{	if (! -e $opt->{mapping_file})
		{	push @$errs, "the file " . $opt->{mapping_file} . " could not be found.\n";
		}

		if ($opt->{input} || $opt->{subset} || $opt->{termlist} || $opt->{term_regexp})
		{	warn "Found a mapping file: ignoring other options";
		}
	}
	else
	{
		if (!$opt->{input})
		{	push @$errs, "specify an input file using -i /path/to/<file_name>";
		}
		elsif (! -e $opt->{input})
		{	push @$errs, "the file " . $opt->{input} . " could not be found.\n";
		}

		if (!$opt->{subset} && !$opt->{termlist}) # && !$opt->{subset_regexp})
		{	push @$errs, "specify a subset using -s <subset_name> or a file containing a list of terms using -t /path/to/<file_name>";
		}
		else
		{	if ($opt->{subset} && $opt->{termlist})
			{	push @$errs, "specify *either* named subset(s) ( '-s <subset_name>' )\n*or* a file containing a list of terms ( '-t' )";
			}

			if ($opt->{termlist})
			{	# check the term list exists
				if (! -e $opt->{termlist})
				{	push @$errs, "the file " . $opt->{termlist} . " could not be found.";
				}

				# if a term regular expression was specified, make sure it is OK
				if ($opt->{term_regexp})
				{	eval { "" =~ /$opt->{term_regexp}/; 1 };
					if ($@)
					{	push @$errs, "the regular expression specified was invalid: $@";
					}
					else
					{	$opt->{term_regexp} = qr/$opt->{term_regexp}/;
					}
				}
			}

			if ($opt->{subset} && scalar keys %{$opt->{subset}} > 1)
			{	# only one subset can be used. sorry!
				my $ss;
				foreach my $k (sort keys %{$opt->{subset}})
				{	$ss = $k;
					last;
				}
				warn "More than one subset was specified; using $ss";
				$opt->{subset} = { $ss => 1 };
			}
		}
	}

	if (! $opt->{count})
	{	$opt->{rewrite} = 1;
	}

	### output - if we're in rewrite mode, we MUST have an output file
	if (! $opt->{output} && $opt->{rewrite})
	{	push @$errs, "specify an output gene association file using -o /path/to/<file_name>";
	}
	elsif (! $opt->{output} && $opt->{count})
	{	warn "No output file specified; results will be printed in the terminal window";
	}

	$opt->{verbose} = 1 if ! $opt->{verbose} && $ENV{VERBOSE};

	if ($errs && @$errs)
	{	die "Error: please correct the following parameters to run the script:\n" . ( join("\n", map { " - " . $_ } @$errs ) ) . "\nThe help documentation can be accessed with the command\n\tgo-slim-annotations.pl --help\n" . Dumper($opt);
	}

	return $opt;
}


sub get_subset_terms {
	my $sub_h;  # we'll store the data in here
	my $opt = shift;

	# see if we have an OBO file...
	if ($opt->{termlist} =~ /\.obo$/)
	{	# looks like it! read in the file and get the term nodes
		## read in the OBO file and quickly pull out the slim terms
		{	local $/ = "\n[";
			open(IN, '<'.$opt->{termlist}) or die "The file ".$opt->{termlist}." could not be opened: $!\nDying";
			print "Loading " . $opt->{termlist} . "...\n" if $opt->{verbose};
			while (<IN>)
			{	if (/^Term].*?^id: .+$/sm && /^id: ?(\S+)$/m)
				{	$sub_h->{$1}++;
				}
			}
			print "Finished loading ontology.\n" if $opt->{verbose};
			close(IN);
		}
	}
	else
	{	# this is a file of unknown origin
		{	local $/ = "\n";
			open(IN, '<'.$opt->{termlist}) or die "The file ".$opt->{termlist}." could not be opened: $!\nDying";
			print "Loading " . $opt->{termlist} . "...\n" if $opt->{verbose};
			my $regexp = $opt->{term_regexp} || qr/^\s*\S+[\s$]/;
			while (<IN>)
			{	if (/($regexp)/)
				{	my $x = $1;
					$x =~ s/^\s*//;
					$x =~ s/\s*$//;
					$sub_h->{$x}++;
				}
			}
			print "Finished loading term file.\n" if $opt->{verbose};
			close(IN);
		}
	}

	if (! $sub_h)
	{	die "Could not find any terms in the file " . $opt->{termlist} . ". Dying";
	}

	print STDERR "Found subset terms: " . join(", ", keys %$sub_h) . "\n" if $options->{verbose};

	return $sub_h;

}


=head2 quickparse_gaf

input:  options  => option_h, containing gaf => GAF file name/location

output:

=cut

sub quickparse_gaf {
	my $args = shift;

	my $options = $args->{options};
#	my $slimmed = $options->{slimmed};

	confess( (caller(0))[3] . ": missing required arguments. Dying" ) unless $options && $options->{ga_input};

	my $fh;
	if ($options->{ga_input} =~ /\.gz$/)
	{	$fh = FileHandle->new("gzip -dc " . $options->{ga_input} . "|") or confess "Could not create a filehandle for " . $options->{ga_input} . ": $! ";
	}
	else
	{	$fh = FileHandle->new($options->{ga_input}) or confess "Could not create a filehandle for " . $options->{ga_input} . ": $! ";
	}

	my $data;
	while (my $line = <$fh>) {
		next if $line =~ /^!/;
		chomp $line;
		my @arr = split("\t", $line);
		## add an extra array item to make it easier to work out which column is which
		unshift @arr, " ";
		#	association ID
		my $a_id = _create_assoc_id(\@arr);

		if ($data->{$arr[5]}{$a_id}) # data already exists
		{	warn "Annotation $a_id to $arr[5] already exists!\n$line";
		}

		push @{$data->{$arr[5]}{$a_id}}, [@arr];
	}
	return $data;

}


=cut

we should ensure that we don't get any duplicated annotations by keeping
a tally of what we've seen as we go along
to generate a unique "ID" for each annotation, we should save the following:

1 - DB
2 - DB_Object_ID
4 - Qualifier
6 - DB:Reference (|DB:Reference)
7 - Evidence code
8 - With (or) From
13 - taxon(|taxon)
14 - Date
15 - Assigned_by

col 5, GO ID, is the data we want for slimming purposes

=cut

sub _create_assoc_id {
	my $arr = shift;
	return join("\0", $arr->[1], $arr->[2], $arr->[4], $arr->[6], $arr->[7], $arr->[8], $arr->[13], $arr->[14], $arr->[15]);
}


sub print_counts {
	my $assocs = shift;
	my $options = shift;
	my $graph = shift;

	my $print_term = sub {
		return shift;
	};

	if ($options->{show_names})
	{	$print_term = sub {
			my ($id, $g) = @_;
			my $t = $g->get_term($id);
			return $t->id . ", " . $t->label;
		};
	}

	if ($options->{show_names})
	{	foreach my $t (sort keys %$assocs)
		{	print STDERR $print_term->($t, $graph) . ": " . ( scalar keys %{$assocs->{$t}} ) . "\n";
		}
	}
	else
	{	foreach my $t (sort keys %$assocs)
		{	print STDERR "$t: " . ( scalar keys %{$assocs->{$t}} ) . "\n";
		}
	}
}


sub print_gaf_file {
	my $assocs = shift;
	my $options = shift;

#	print STDERR "assocs: " . Dumper($assocs);

	foreach my $t (sort keys %$assocs)
	{	foreach my $a (map { $assocs->{$t}{$_}[0] } sort keys %{$assocs->{$t}})
		{
			print STDERR
			join("\t", @$a[1..4])
			. "\t" . $a->[5] . "\t"
			. join("\t", @$a[6..$#$a])
			. "\n";
		}
	}
}

