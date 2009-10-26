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
use Storable qw(dclone);
use Data::Dumper;
$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Indent = 1;

use GOBO::Graph;
use GOBO::Parsers::OBOParserDispatchHash;
use GOBO::Parsers::QuickGAFParser;
use GOBO::InferenceEngine;
use GOBO::Util::GraphFunctions;
use GOBO::DataArray;
#use GOBO::Writers::TreeWriter;
my $options = parse_options(\@ARGV);

my $data;
my $subset;
my $graph;

# parse the input file and check we get a graph

if ($options->{mapping_file})
{
	die "Sorry, this option is not yet implemented";

=cut
	$data = GOBO::Util::GraphFunctions::load_mapping({ mapping_file => $options->{mapping_file} });
	# check we have enough subset terms
	test_subset( $data->{subset_term}, $options );
	map { $subset->{$_} = 1 } keys %{$data->{subset_term}};

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
	my $new_graph = GOBO::Util::GraphFunctions::add_nodes_and_links_to_graph({ old_g => $parser->graph, new_g => new GOBO::Graph, graph_data => $data->{graph}, options => $options });
	print STDERR "Done GOBO::Util::GraphFunctions::add_nodes_and_links_to_graph!\n" if $options->{verbose};

	$graph = GOBO::Util::GraphFunctions::add_extra_stuff_to_graph({ old_g => $parser->graph, new_g => $new_graph, options => $options });
	print STDERR "Done GOBO::Util::GraphFunctions::add_extra_stuff_to_graph!\n" if $options->{verbose};
=cut

}
else
{	# before we check out the graph, let's see if the file actually contains any terms...
	if ($options->{termlist})
	{	$data->{to_find} = read_term_file($options);
		test_subset( $data->{to_find}, $options );
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
	# get the nodes matching our subset criteria
	if ($options->{subset})
	{	$data = GOBO::Util::GraphFunctions::get_subset_nodes({ graph => $graph, options => $options });
		print STDERR "Done GOBO::Util::GraphFunctions::get_subset_nodes!\n" if $options->{verbose};

		# move the subset to $subset
		foreach my $s (keys %{$data->{subset}})
		{	map { $subset->{$_} = 1 } keys %{$data->{subset}{$s}};
		}
		test_subset( $subset, $options );
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
			{	die "$msg go_slim_annotations was looking for the following terms:\n" . join(", ", sort keys %{$options->{termlist}}) . "\nPlease try again.\nDying";
			}
			else
			{	die "$msg Please try again.\nDying";
			}
		}

		test_subset( $subset, $options );

		## make sure that we have the root nodes
		map { $subset->{ $_->id }++ } @{$graph->get_roots};
	}
}

## buckets: look at all GS terms and get their parent terms;
## we're looking for terms which slim to the parent terms but NOT the child

## foreach GO slim term
## get the parent term
##




## get the GA data
my $gaf_parser = GOBO::Parsers::QuickGAFParser->new(fh=>$options->{ga_input});

my $assoc_data = $gaf_parser->parse;
#print STDERR "assoc data: " . Dumper($assoc_data) . "\n\n";

if (! $assoc_data )
{	die "No annotations were found! Dying";
}

# we're going to do a slight hack here as the reasoner doesn't reason over term-
# annotation relations, so we'll add the annotation data as link objects and
# save the annotations elsewhere
my $annot = new GOBO::RelationNode( id => 'annotated_to', label => 'annotated to' );
my $not_annot = new GOBO::RelationNode( id => 'annotated_to_NOT', label => 'annotated to NOT' );
$graph->add_relation($annot);
$graph->add_relation($not_annot);
## should there be any other relationships for annotated_to?
$annot->transitive_over( $graph->relation_noderef('part_of') );

my @errs;
my $a_ids;
foreach my $a (keys %{$assoc_data->{by_a}})
{	foreach my $t (@{$assoc_data->{by_a}{$a}{terms}})
	{	## check the term exists in the graph
		if (! $graph->get_term($t))
		{	push @errs, $t;
			next;
		}
		$a_ids->{$a}++;
		my $link;
		# check for a NOT annotation
		if ($assoc_data->{by_a}{$a}{arr}[4] && $assoc_data->{by_a}{$a}{arr}[4] =~ /NOT/)
		{	$link = new GOBO::LinkStatement(node=>$graph->noderef($a), relation=>$not_annot, target=>$graph->get_term($t));
		}
		else
		{	$link = new GOBO::LinkStatement(node=>$graph->noderef($a), relation=>$annot, target=>$graph->get_term($t));
		}
		$graph->add_link($link);
	}
}

$graph->update_graph;

if (@errs)
{	if (scalar @errs == scalar keys %{$assoc_data->{by_t}})
	{	die "None of the terms in the annotation file matched those in the ontology file. Dying";
	}
	else
	{	warn "The following terms were not found in the ontology file: " . join(", ", @errs);
	}
}

## OK, we have our graph with the annotations attached. Let's get slimming!
# the input set is any terms with an annotation connected to them
my $input = [ keys %$subset, keys %$a_ids ];

# get the links between the nodes
my $node_data = GOBO::Util::GraphFunctions::get_graph_links({ subset => $subset, graph => $graph, options => $options, input => $input });
print STDERR "Done GOBO::Util::GraphFunctions::get_graph_links!\n" if $options->{verbose};

# slim down the graph...
# in these slims, the input set is all terms in the graph
my $graph_data = GOBO::Util::GraphFunctions::slim_graph({ graph => $graph, subset => $subset, options => $options, node_data => $node_data });

undef @errs;
my $n_annots = scalar keys %$a_ids;
## look for missing annotations
foreach my $a (keys %$a_ids)
{	# this annotation has been lost from the graph!
	if (! $graph_data->{graph}{$a})
	{	push @errs, $a;
		delete $a_ids->{$a};
		next;
	}
	## transfer this info into the a_ids hash
	$a_ids->{$a} = $graph_data->{graph}{$a};
## remove the link data that's supposed to represent annotations (we will replace
## it with annotation objects later on)
	delete $graph_data->{graph}{$a};
}

if (@errs)
{	if (scalar @errs == $n_annots)
	{	die "All annotations were lost during slimming! Dying";
	}
	else
	{	warn "The following annotations were lost during slimming: " . join(", ", @errs);
	}
}

## convert this into a graph object
my $new_graph = GOBO::Util::GraphFunctions::add_referenced_nodes_to_graph({ old_g => $graph, graph_data => $graph_data, options => $options });

# add the links
$new_graph = GOBO::Util::GraphFunctions::add_nodes_and_links_to_graph({ old_g => $graph, graph_data => $graph_data, options => $options });

$new_graph = GOBO::Util::GraphFunctions::add_extra_stuff_to_graph({ old_g => $graph, new_g => $new_graph, options => $options });

## add the annotations, and check for any missing assocs
## if we are rewriting the file, we don't need to add all assocs,
## only the closest ones

if ($options->{rewrite})
{	foreach my $a (keys %$a_ids)
	{	foreach my $r (keys %{$a_ids->{$a}})
		{	foreach my $t (keys %{$a_ids->{$a}{$r}})
			{	my $a_node = new GOBO::DataArray( id => $a, data => $assoc_data->{by_a}{$a}{arr} );
				my $annot = new GOBO::Annotation(
					node => $a_node,
					relation => $new_graph->get_node($r),
					target => $new_graph->get_node($t) );
				$new_graph->add_annotation($annot);
			}
		}
	}

	## and now we can rewrite the file!
	print_gaf_file({ graph => $new_graph, assoc_data => $assoc_data, options => $options });

	exit(0);
}

## otherwise, we'll build the graph with additional links
foreach my $a (keys %$a_ids)
{	## add the full data
	if ($node_data->{graph}{$a})
	{	foreach my $r (keys %{$node_data->{graph}{$a}})
		{	foreach my $t (keys %{$node_data->{graph}{$a}{$r}})
			{	my $a_node = new GOBO::DataArray( id => $a, data => $assoc_data->{by_a}{$a}{arr} );
				my $annot = new GOBO::Annotation(
					node => $a_node,
					relation => $new_graph->get_node($r),
					target => $new_graph->get_node($t),
				);
				## see if this combo exists in the a_ids hash, which contains
				## only the closest links
				if ($a_ids->{$a} && $a_ids->{$a}{$r} && $a_ids->{$a}{$r}{$t})
				{	## ok, this is a direct annotation
				}
				else
				{	$annot->inferred(1);
				}
				$new_graph->add_annotation($annot);
			}
		}
	}
}

## a few little statistical bits...
if ($options->{verbose})
{	my @roots = @{$new_graph->get_roots};
	print STDERR "roots: " . join("\n", @roots) . "\n\n";

	print STDERR "Annotations to root nodes:\n";
	foreach (@roots)
	{	my $a = $new_graph->get_annotations_by_target($_) || [];
		my @all = grep { $_->inferred } @$a;
		print STDERR "$_: direct: " . ( scalar @$a - scalar @all )
		. "\t"
		. "inferred: " . ( scalar @all ) . "\n";
	}
	print STDERR "\n\n";
}

# now print out the results
if ($options->{print_tree})
{	my $writer = new GOBO::Writers::TreeWriter( graph => $new_graph, show_annotation_counts => 1 );
	$writer->write_tree;
}
else
{	print_counts( $new_graph, $options );
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
		elsif ($o eq '--show_tree') {
			$opt->{print_tree} = 1;
		}
		elsif ($o eq '-b' || $o eq '--buckets') {
			$opt->{buckets} = 1;
		}
		elsif ($o eq '-n' || $o eq '--show_names') {
			$opt->{show_names} = 1;
		}
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


sub read_term_file {
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

	print STDERR "Found terms: " . join(", ", keys %$sub_h) . "\n" if $options->{verbose};

	return $sub_h;

}


sub print_counts {
	my $graph = shift;
	my $options = shift;

	if ($options->{output})
	{	open (STDOUT, ">" . $options->{output}) or confess "Could not create file " . $options->{output} . ": $! ";
	}

	print STDOUT "! Gene Association file created by go-slim-annotations.pl from source file " . $options->{ga_input} . "\n!\n";

#	my $print_term = sub {
#		return shift;
#	};

#	if ($options->{show_names})
#	{	$print_term = sub {
#			my ($id, $g) = @_;
#			my $t = $g->get_term($id);
#			return $t->id . ", " . $t->label;
#		};
#	}

	foreach my $t (sort { $a->id cmp $b->id } @{$graph->get_annotated_terms})
	{	my $annots = [ sort { $a->node->id cmp $b->node->id } @{$graph->get_annotations_by_target($t)} ];
		my @inf = grep { $_->inferred } @$annots;
		print STDOUT
		"$t: direct: " . ( scalar @$annots - scalar @inf )
		. "\t"
		. "inferred: " . ( scalar @inf ) . "total: " . ( scalar @$annots ) . "\n";

	}

#	foreach my $assoc (sort { $a->node->id cmp $b->node->id || $a->target->id cmp $b->target->id } @{$graph->annotations})
#	{	## retrieve the array data...

#	}
}


sub print_gaf_file {
	my $args = shift;
	my ($graph, $options) = ($args->{graph}, $args->{options});

	if ($options->{output})
	{	open (STDOUT, ">" . $options->{output}) or confess "Could not create file " . $options->{output} . ": $! ";
	}

	print STDOUT "! Gene Association file created by go-slim-annotations.pl from source file " . $options->{ga_input} . "\n!\n";

	foreach my $assoc (sort { $a->node->id cmp $b->node->id || $a->target->id cmp $b->target->id } @{$graph->annotations})
	{	## retrieve the array data...
		my $arr = $assoc->node->data;
		print STDOUT
			join("\t", @$arr[1..4])
			. "\t" . $assoc->target->id . "\t"
			. join("\t", @$arr[6..$#$arr])
			. "\n";
	}
}


sub test_subset {
	my ($ss, $opt) = (@_);
	if (scalar keys %$ss < 5 && ! $opt->{force_continue})
	{	print STDERR "subset keys found:\n" . join("\n", keys %$ss) . "\n" if $opt->{verbose};
		die "Only " . (scalar keys %$ss) . " terms from the subset specified could be found. To continue anyway, please run the script again with the extra command line parameter -f\nDying";
	}
}


