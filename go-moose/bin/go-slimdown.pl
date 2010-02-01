#!/usr/bin/perl -w
# find GO slim nodes and generate a graph based on them, removing any nodes not
# in the slim

use strict;
use FileHandle;
use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use GOBO::Graph;
use GOBO::Parsers::OBOParserDispatchHash;
use GOBO::InferenceEngine::CustomEngine;
use GOBO::InferenceEngine;
use GOBO::Writers::OBOWriter;
use GOBO::Writers::TreeWriter;
use GOBO::Util::GraphFunctions;
use Storable qw(dclone);

my $options = parse_options(\@ARGV);
my $subset_min = $options->{minimum_subset_terms} || 5;

if (! $options->{verbose})
{	$options->{verbose} = $ENV{GO_VERBOSE} || 0;
}

# parse the input file and check we get a graph
my $parser = new GOBO::Parsers::OBOParserDispatchHash(file => $options->{input});
$parser->parse;
die "Error: parser could not find a graph in " . $options->{input} . "!\n" unless $parser->graph;

print STDERR "Finished parsing file " . $options->{input} . "\n" if $options->{verbose};

# get the nodes matching our subset criteria
# will die if no subset terms found
my $data = GOBO::Util::GraphFunctions::get_subset_nodes( graph => $parser->graph, options => $options );
	print STDERR "Done GOBO::Util::GraphFunctions::get_subset_nodes!\n" if $options->{verbose};

$options->{return_as_graph} = 1;
$options->{remove_unlinked_terms} = 1;
$parser->graph->duplicate_statement_ix('ontology_links', 'asserted_links');

my @errs;
foreach my $s (keys %{$data->{subset}})
{
	# check there are sufficient terms in the subset
	if (scalar keys %{$data->{subset}{$s}} < $subset_min && ! $options->{force_continue})
	{	push @errs, "Error: only " . (scalar keys %{$data->{subset}{$s}}) . " terms from the subset " . Dumper($s) . " could be found. To continue anyway, please run the script again with the extra command line parameter -f";
		print STDERR "subset keys found:\n" . join("\n", keys %{$data->{subset}{$s}}) . "\n" if $options->{verbose};
		next;
	}
	my @subset_arr = values %{$data->{subset}{$s}};
	my $ie = new GOBO::InferenceEngine::CustomEngine(graph => $g);

	## clone the graph
	my $ie = new GOBO::InferenceEngine::CustomEngine(graph => dclone $parser->graph);

	# slim down the graph...
	# in these slims, the input set is the same as the mapping set
	$ie->slim_graph( subset_ids => [ keys %{$data->{subset}{$s}} ], input_ids => [ keys %{$data->{subset}{$s}} ], from_ix => 'asserted_links', save_ix => 'inferred_ontology_links', options => $options );

#	print STDERR "post slimming\nsubset: " . join(", ", sort keys %{$data->{subset}{$s}}) . "\nterms:  " . join(", ", sort { $a->id cmp $b->id } @{$ie->graph->terms}) . "\nn indices: " . (scalar $ie->graph->get_statement_ix_h_names ) . ": " . join(", ", sort $ie->graph->get_statement_ix_h_names ) . "\n\n\n" if $options->{verbose};


#	print STDERR "n terms: " . (scalar @{$ie->graph->terms}) . "\nn relations: " . (scalar @{$ie->graph->relations} ) . "\n\n" if $ENV{VERBOSE};

#	print STDERR "new graph statements:\n" . join("\n", @{$ie->graph->statements}) . "\nontology_links: " . join("\n", @{$ie->graph->ontology_links}) . "\n" if $options->{verbose};


	if ($options->{tree_format})
	{	my $writer = new GOBO::Writers::TreeWriter( file => $options->{output}, graph => $ie->graph, edge_ix => 'inferred_' . $s . '_links' );
		$writer->write;
	}
	else
	{	if ($options->{basename})
		{	# create the file name from the slim name
			($options->{output} = $options->{basename}) =~ s/SLIM_NAME/$s/;
		}
		my $writer = GOBO::Writers::OBOWriter->create(file=>$options->{output}, format=>'obo', edge_ix => 'inferred_' . $s . '_links', graph=>$ie->graph);
#		$writer->graph($ie->graph);
		$writer->write();
		print STDERR "Done write_graph_to_file for " . $options->{output} . "!\n" if $options->{verbose};
	}
}

if (@errs)
{	warn join("\n", @errs);
}

print STDERR "Job complete!\n" if $options->{verbose};

exit(0);

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
		elsif ($o eq '-o' || $o eq '--output') {
			$opt->{output} = shift @$args if @$args && $args->[0] !~ /^\-/;
		}
		elsif ($o eq '-b' || $o eq '--basename') {
			$opt->{basename} = shift @$args if @$args && $args->[0] !~ /^\-/;
		}
		elsif ($o eq '-c' || $o eq '--combined') {
			## use a combination of more than one subset nodes
			$opt->{combined} = 1;
		}
		elsif ($o eq '-a' || $o eq '--get_all_subsets') {
			$opt->{get_all_subsets} = 1;
		}
		elsif ($o eq '-r' || $o eq '--regexp') {
			# this option is "hidden" at the moment - enter a text string to be
			# qr//'d and use as a regexp
			$opt->{subset_regexp} = shift @$args if @$args && $args->[0] !~ /^\-/;
		}
		elsif ($o eq '-t' || $o eq '--tree') {
			# output can be formatted as a denormalised tree
			$opt->{tree_format} = 1;
		}
		elsif ($o eq '-f' || $o eq '--force') {
			$opt->{force_continue} = 1;
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
	{	die "Error: please ensure you have specified an input file, a subset, and an output file.\nThe help documentation can be accessed with the command 'go-slimdown.pl --help'\n";
	}

	if (!$opt->{input})
	{	push @$errs, "specify an input file using -i /path/to/<file_name>";
	}
	elsif (! -e $opt->{input})
	{	push @$errs, "the file " . $opt->{input} . " could not be found.\n";
	}

	if (!$opt->{get_all_subsets} && ! $opt->{subset_regexp} && !$opt->{subset})
	{	push @$errs, "specify a subset using -s <subset_name>";
	}

	if (!$opt->{output} && !$opt->{basename})
	{	push @$errs, "specify an output file using -o /path/to/<file_name>";
	}

	if ($opt->{basename} && $opt->{basename} !~ /SLIM_NAME/)
	{	push @$errs, "specify a valid basename (containing SLIM_NAME) for the output files";
	}

	if (($opt->{subset} && scalar values %{$opt->{subset}} > 1)
		|| $opt->{get_all_subsets}
		|| $opt->{subset_regexp})
	{	## if we have more than one subset, make sure that we have specified a base name for the file
		if (!$opt->{combined} && !$opt->{basename})
		{	push @$errs, "specify a base file name (containing SLIM_NAME) for the output files using -b /path/to/<file_name>";
		}
		elsif ($opt->{combined})
		{	# only one output file if we're combining subsets
			if (! $opt->{output})
			{	push @$errs, "there should only be a single output file specified if subsets are to be combined";
			}
		}
	}

	my $cnt;
	if ($opt->{subset_regexp})
	{	eval { "" =~ /$opt->{subset_regexp}/; 1 };
		if ($@)
		{	push @$errs, "the regular expression specified was invalid: $@";
		}
		else
		{	$opt->{subset_regexp} = qr/$opt->{subset_regexp}/;
		}
		$cnt++;
	}
	$cnt++ if $opt->{get_all_subsets};
	$cnt++ if values %{$opt->{subset}};

	# make sure we only have one subset-related criterion specified
	if ($cnt && $cnt > 1)
	{	push @$errs, "specify *either* named subset(s) ( '-s <subset_name>' )\n*or* to get all subsets ( '-a' )";
	}

	if ($opt->{output} && $opt->{basename})
	{	## if we have any of the options which allow more than one subset
		## and the combined flag is off, use 'basename'
		if ((($opt->{subset} && scalar values %{$opt->{subset}} > 1)
			|| $opt->{get_all_subsets} || $opt->{subset_regexp})
			&& !$opt->{combined})
		{	warn "Using file path specified by the '-b' / '--basename' option\n";
		}
		else
		{	warn "Using file path specified by the '-o' / '--output' option\n";
			delete $opt->{basename};
		}
	}

	if ($errs && @$errs)
	{	die "Error: please correct the following parameters to run the script:\n" . ( join("\n", map { " - " . $_ } @$errs ) ) . "\nThe help documentation can be accessed with the command\n\tgo-slimdown.pl --help\n";
	}

	return $opt;
}


=head1 NAME

go-slimdown.pl - select subset terms, slim 'em down, and output the results

=head1 SYNOPSIS

 go-slimdown.pl -i gene_ontology.obo -s goslim_generic -o slimmed.obo

=head1 DESCRIPTION

Given a file where certain terms are specified as being in subset S, this
script will 'slim down' the file by removing terms not in the subset.

Relationships between remaining terms are calculated by the inference engine.

If the root nodes are not already in the subset, they are added to the graph.

The slimming algorithm is 'relationship-aware', and finds the closest node
for each relation (rather than just the closest term). For example, if we
had the following relationships:

A -i- B -i- C -p- D

the slimmer would say that B was the closest node via the 'i' relation, and
D was the closest via the 'p' relation.

Note that there may be several different relationships between the same two
terms in the slimmed file.

=head2 Input parameters

=head3 Required

=over

=item -i || --ontology /path/to/file_name

input file (ontology file) with path

=item -s and -a: specifying subset(s)

go-slimdown.pl can extract a number of subsets from a file. You can specify
named subset(s) using the B<-s> or B<--subset> parameter:

 -s I<subset_name> (-s I<subset_2_name>, -s I<subset_3_name>, ...)

Alternatively, you can get all subsets using the B<-a> or B<--get_all_subsets>
option:

 go-slimdown.pl -i gene_ontology.obo --get_all_subsets -o slimmed.obo

See the B<-c> option below for combining subsets.

=item -o and -b: output file or files

If you are using a single subset, or wish to combine subsets, use the B<-o> or
B<--output> option:

 -o /path/to/file_name

If you are using several subsets and want to create separate files for each, use
the B<-b> or B<--basename> option to specify a base name for your output files:

 -b /path/to/file_base_name_containing_SLIM_NAME

The text "SLIM_NAME" will be replaced by the name of the subset. For example:

  go-slimdown.pl -i myfile.obo -s slim_goa -s slim_yeast
  -b /temp/slimmed.SLIM_NAME.obo

would produce two files, C</temp/slimmed.slim_goa.obo> and
C</temp/slimmed.slim_yeast.obo>.

=back

=head3 Optional switches

=over

=item  -c || --combined

If more than one subset is specified, create a slim using terms from all of
the subsets specified

=item -v || --verbose

prints various messages

=back

=head1 AUTHOR

Amelia Ireland

=head1 SEE ALSO

L<GOBO::InferenceEngine>, L<GOBO::Graph>, L<GOBO::Doc::FAQ>

=cut
