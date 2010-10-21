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

=item --show_tree

When in count mode, show the results as a denormalised tree

=item -k || --keep_orphans

Orphans are terms in the graph that, after slimming, are no longer connected to
graph root node. By default, these are removed from the graph. With this option
on, orphaned subset terms with annotations will be kept in the results. Note
that any non-subset orphan terms (and attached annotations) will be deleted
regardless of this setting.

Note that only subset

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
use Storable qw(dclone);
use Data::Dumper;
$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Indent = 1;

use GOBO::Graph;
use GOBO::Parsers::OBOParserDispatchHash;
use GOBO::Parsers::QuickGAFParser;
use GOBO::Writers::TreeWriter;
use GOBO::InferenceEngine::CustomEngine;
use GOBO::Util::GraphFunctions;
use GOBO::Util::Misc;
use GOBO::DataArray;
use GOBO::Writers::GAFWriter;
use GOBO::Writers::QuickGAFWriter;

my $options = parse_options(\@ARGV);

my $data;
my $subset_ids;
my $graph;

$ENV{VERBOSE} = 1;
$ENV{REMOVE_ROOT_ASSOCS} = 1;

# parse the input file and check we get a graph

$options->{closest_nodes_only} = 1;

if ($options->{mapping_file})
{
	die "Sorry, this option is not yet implemented";

=cut
	$graph = GOBO::Util::GraphFunctions::load_mapping_as_graph( mapping_file => $options->{mapping_file} );

	$data = GOBO::Util::GraphFunctions::get_subset_nodes( graph => $graph, options => { get_all_subsets => 1 } );


	# check we have enough subset terms
	test_subset( $data->{subset_term}, $options );
	map { $subset_ids->{$_} = 1 } keys %{$data->{subset_term}};

	my $p_options = {
		body => { parse_only => {
#			term => [ qw( name namespace ) ],
			typedef => '*',
	}, }, };

	## if we are either showing names or we want to generate
	## a new GAF file, get the name info from the ontology file.
	if ( $options->{show_names} || $options->{rewrite} )
	{	$p_options->{body}{parse_only}{term} = [ qw(name namespace) ];
	}

	print STDERR "Getting info from " . $options->{input} . "\n" if $options->{verbose};

	my $parser = new GOBO::Parsers::OBOParserDispatchHash(file=>$options->{input},
		options => $p_options);
	$parser->parse;

	## create a graph from the data we have
	my $new_graph = GOBO::Util::GraphFunctions::add_nodes_and_links_to_graph( old_g => $parser->graph, new_g => new GOBO::Graph, graph_data => $data->{graph}, options => $options );
	print STDERR "Done GOBO::Util::GraphFunctions::add_nodes_and_links_to_graph!\n" if $options->{verbose};

	$graph = GOBO::Util::GraphFunctions::add_extra_stuff_to_graph( old_g => $parser->graph, new_g => $new_graph, options => $options );
	print STDERR "Done GOBO::Util::GraphFunctions::add_extra_stuff_to_graph!\n" if $options->{verbose};
=cut

}
else
{	# before we check out the graph, let's see if the file actually contains any terms...
	if ($options->{termlist})
	{	$data->{to_find} = GOBO::Util::Misc::read_term_file(%$options);
		test_subset( [keys %{$data->{to_find}}], $options );
	}

	my $p_options = {
		body => { parse_only => {
			term => [ qw(name namespace subset is_a relationship is_obsolete )],
			typedef => '*',
	} } };

	## parse file and load the graph
	my $parser = new GOBO::Parsers::OBOParserDispatchHash(file=>$options->{input},
		options => $p_options, graph => new GOBO::Graph );
	$parser->parse;
	$graph = $parser->graph;

	print STDERR "Parsed the ontology file\n" if $options->{verbose};

	# get the nodes matching our subset criteria
	if ($options->{subset})
	{	$data = GOBO::Util::GraphFunctions::get_subset_nodes( graph => $graph, options => $options );
		print STDERR "Done GOBO::Util::GraphFunctions::get_subset_nodes!\n" if $options->{verbose};

		# move the subset to $subset_ids
		foreach my $s (keys %{$data->{subset}})
		{	map { $subset_ids->{$_} = 1 } keys %{$data->{subset}{$s}};
		}
		test_subset( [keys %$subset_ids], $options );
	}
	else
	{	my @fail;
		# we got the terms from a file. Check that they exist in our graph!
		foreach (keys %{$data->{to_find}})
		{	if ($graph->get_term($_) && ! $graph->get_term($_)->obsolete)
			{	$subset_ids->{$_} = 1;
			}
			else
			{	push @fail, $_;
			}
		}
		if (! $subset_ids || ! keys %$subset_ids)
		{	my $msg = "None of the terms specified in " . $options->{termlist} . " were found in the ontology file.";
			if ($options->{verbose})
			{	die "$msg go_slim_annotations was looking for the following terms:\n" . join(", ", sort keys %{$options->{termlist}}) . "\nPlease try again.\nDying";
			}
			else
			{	die "$msg Please try again.\nDying";
			}
		}

		test_subset( [keys %$subset_ids], $options );

		## make sure that we have the root nodes
		map { $subset_ids->{ $_->id }++ } @{$graph->get_roots};
	}
}

print STDERR "Got the graph and subset terms!\n" if $options->{verbose};

my $results = GOBO::Util::GraphFunctions::slim_annotations(options => $options, subset_ids => [keys %$subset_ids], graph => $graph );

print STDERR "Got our results!\n" if $options->{verbose};

$graph = $results->{graph};

## add the annotations, and check for any missing assocs
## if we are rewriting the file, we can get the annotations from the graph
## and then print them out
## we only need the closest assocs, not the whole lot

if ($options->{mode} eq 'rewrite')
{	my $writer = new GOBO::Writers::QuickGAFWriter( file => $options->{output} );
	$writer->header_data( [ "Gene Association file created by go-slim-annotations.pl from source file " . $options->{ga_input} ] );
	$writer->assoc_array( $graph->get_all_statements_in_ix('transitive_reduction') );
	$writer->write;

	exit(0);
}

# count or tree mode: print out the results
if ($options->{mode} eq 'count')
{	#print STDERR "statements:\n" . join("\n", @{$graph->statements}) . "\n";
#	print_counts( graph => $graph, options => $options );
	new_print_counts( graph => $graph, options => $options );
}
else
{	my $writer = new GOBO::Writers::TreeWriter( file => $options->{output}, graph => $graph, show_annotation_counts => 1, ontology_link_ix => 'direct_ontology_links', annotation_ix => 'all_annotations' );
	$writer->write;
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
#		### load the slim data from a mapping file
#		elsif ($o eq '-m' || $o eq '--mapping_file') {
#			$opt->{mapping_file} = shift @$args if @$args && $args->[0] !~ /^\-/;
#		}
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
			if ($opt->{mode} && $opt->{mode} eq 'tree')
			{	## ignore this
			}
			else
			{	$opt->{mode} = 'count';
			}
		}
		elsif ($o eq '--show_tree') {
			## override any existing mode
			$opt->{mode} = 'tree';
		}
		elsif ($o eq '-k' || $o eq '--keep_orphans') {
			$opt->{keep_orphans} = 1;
		}
#		elsif ($o eq '-b' || $o eq '--buckets') {
#			$opt->{buckets} = 1;
#		}
#		elsif ($o eq '-n' || $o eq '--show_names') {
#			$opt->{show_names} = 1;
#		}
#		elsif ($o eq '--show_gp_names') {
#			$opt->{show_gp_names} = 1;
#		}
		elsif ($o eq '-f' || $o eq '--force_continue') {
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
	{	die "Error: please ensure you have specified a gene association file, an ontology file, and a subset.\nThe help documentation can be accessed with the command 'go-slim-annotations.pl --help'\nDying";
	}

	if (!$opt->{ga_input})
	{	push @$errs, "specify a gene association file using -i /path/to/<file_name>";
	}
	elsif (! -e $opt->{ga_input})
	{	push @$errs, "the file " . $opt->{input} . " could not be found.\n";
	}

	if (!$opt->{input})
	{	push @$errs, "specify an input file using -i /path/to/<file_name>";
	}
	elsif (! -e $opt->{input})
	{	push @$errs, "the file " . $opt->{input} . " could not be found.\n";
	}

	if ($opt->{mapping_file})
	{	if (! -e $opt->{mapping_file})
		{	push @$errs, "the file " . $opt->{mapping_file} . " could not be found.\n";
		}

		if ($opt->{buckets})
		{	warn "Cannot use buckets in conjunction with a mapping file: turning buckets off.";
			delete $opt->{buckets};
		}

		if ($opt->{subset} || $opt->{termlist} || $opt->{term_regexp})
		{	warn "Found a mapping file: ignoring other options";
		}
	}
	else
	{

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
				$opt->{subset} = [ $ss ];
			}
		}
	}

	if (! $opt->{mode})
	{	$opt->{mode} = 'rewrite';
	}

	### output - if we're in rewrite mode, we MUST have an output file
	if (! $opt->{output} && $opt->{rewrite})
	{	push @$errs, "specify an output gene association file using -o /path/to/<file_name>";
	}
	elsif (! $opt->{output} && $opt->{mode} eq 'count')
	{	warn "No output file specified; results will be printed in the terminal window";
	}

	$opt->{verbose} = 1 if ! $opt->{verbose} && $ENV{VERBOSE};

	if ($errs && @$errs)
	{	die "Error: please correct the following parameters to run the script:\n" . ( join("\n", map { " - " . $_ } @$errs ) ) . "\nThe help documentation can be accessed with the command\n\tgo-slim-annotations.pl --help\n" . Dumper($opt);
	}

	if ($opt->{subset} && ref $opt->{subset} eq 'HASH')
	{	$opt->{subset} = [ keys %{$opt->{subset}} ];
	}

	return $opt;
}


sub new_print_counts {
	my %args = (@_);
	my $g = $args{graph};
	my $options = $args{options};

	if ($options->{output})
	{	open (STDOUT, ">" . $options->{output}) or confess "Could not create file " . $options->{output} . ": $! ";
	}

	print STDOUT "! Gene Association file created by go-slim-annotations.pl from source file " . $options->{ga_input} . "\n!\n";

#	print STDERR "annotated terms: " . join(", ", @{$g->get_annotated_terms('all_annotations')} ) ."\n\n";

#	sort { $a->id cmp $b->id } @{$g2->get_annotated_terms('annotations')}

	foreach my $t (sort { $a->id cmp $b->id } @{$g->get_annotated_terms_in_ix('all_annotations')})
	{	my $stts = $g->statements_in_ix_by_target_id('all_annotations', $t);
		my $direct = 0;
		my $all = 0;
		map {
			my $n_gp = scalar @{$_->node->data_arr};
			$all += $n_gp;
			$direct += $n_gp if ! $_->inferred;
		} @$stts;

		print STDOUT
		"$t: direct: " . $direct
		. "\tinferred: " . ( $all - $direct )
		. "\ttotal: " . $all . "\n";

	}
}


sub test_subset {
	my ($ss, $opt) = (@_);
	if (scalar @$ss < 5 && ! $opt->{force_continue})
	{	print STDERR "subset keys found:\n" . join("\n", @$ss) . "\n" if $opt->{verbose};
		die "Only " . (scalar @$ss) . " terms from the subset specified could be found. To continue anyway, please run the script again with the extra command line parameter -f\nDying";
	}
}

