#!/usr/bin/perl -w
# find GO slim terms and generate a graph based on them, removing any terms not
# in the slim

use strict;
use FileHandle;
use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use GOBO::Graph;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::OBOParser;
use GOBO::InferenceEngine;
use GOBO::Writers::OBOWriter;

my $verbose = $ENV{GO_VERBOSE} || 0;

my $options;

while (@ARGV && $ARGV[0] =~ /^\-/) {
	my $opt = shift @ARGV;
	if ($opt eq '-i' || $opt eq '--ontology') {
		$options->{file} = shift;
		if (! -e $options->{file})
		{	die "Error: the file " . $options->{file} . " could not be found.\n";
		}
	}
	elsif ($opt eq '-s' || $opt eq '--subset') {
		my $s = shift;
		if ($options->{subset})
		{	push @{$options->{subset}}, $s if ! grep { $_ eq $s } @{$options->{subset}};
		}
		else
		{	push @{$options->{subset}}, $s;
		}
	}
	elsif ($opt eq '-o' || $opt eq '--output') {
		$options->{output} = shift;
	}
	elsif ($opt eq '-b' || $opt eq '--basename') {
		my $b = shift;
		if ($b !~ /SLIM_NAME/)
		{	die "Error: please supply a valid basename (containing SLIM_NAME) for the output files\nThe help documentation can be accessed with the command 'go-slimdown.pl --help'\n";
		}
		$options->{basename} = $b;
	}
	elsif ($opt eq '-c' || $opt eq '--combined') {
		## use a combination of more than one subset terms
		$options->{combined} = 1;
	}
	elsif ($opt eq '-h' || $opt eq '--help') {
		system("perldoc", $0);
		exit(0);
	}
	elsif ($opt eq '-v' || $opt eq '--verbose') {
		$verbose = 1;
	}
	else {
		die "Error: no such option: $opt\nThe help documentation can be accessed with the command 'go-slimdown.pl --help'\n";
	}
}

if (!$options || !$options->{file}|| !$options->{subset} || !@{$options->{subset}} || !($options->{output} || $options->{basename}))
{	die "Error: please ensure you have specified an input file, a subset, and an output file.\nThe help documentation can be accessed with the command 'go-slimdown.pl --help'\n";
}

if (scalar @{$options->{subset}} > 1)
{	## if we have more than one subset, make sure that we have specified a base name for the file
	if (!$options->{combined} && !$options->{basename})
	{	die "Error: please supply a valid basename (containing SLIM_NAME) for the output files\nThe help documentation can be accessed with the command 'go-slimdown.pl --help'\n";
	}
	elsif ($options->{combined})
	{	# only one output file if we're combining subsets
		if (!$options->{output})
		{	die "Error: there should only be a single output file specified if subsets are to be combined.\nThe help documentation can be accessed with the command 'go-slimdown.pl --help'\n";
		}
	}
}


if ($options->{output} && $options->{basename})
{	if (scalar @{$options->{subset}} > 1)
	{	warn "Using file path specified by the '-b' / '--basename' option\n";
	}
	else
	{	warn "Using file path specified by the '-o' / '--output' option\n";
		delete $options->{basename};
	}
}

my $data;
my $parser = new GOBO::Parsers::OBOParser(file=>$options->{file});
$parser->parse;

die "Error: parser could not find a graph in " . $options->{file} . "!\n" unless $parser->graph;

my $graph = $parser->graph;

my $ie = new GOBO::InferenceEngine(graph=>$graph);

## get all terms that are in a subset
foreach ( @{$graph->terms} )
{	next if $_->obsolete;
	## if it's in a subset, save the mofo.
	my $n = $_;
	if ($n->subsets)
	{	foreach my $s (@{$n->subsets})
		{	$data->{subset}{$s->id}{$n->id}++ if grep { $s->id eq $_ } @{$options->{subset}};
		}
	}
	# make sure that we have all the root nodes
	elsif (!@{$graph->get_outgoing_links($n)}) {
		$data->{roots}{$n->id}++;
	}
}

#	check that we have terms in our subsets
if (scalar @{$options->{subset}} > 1)
{	my $no_terms;
	foreach (@{$options->{subset}})
	{	if (! $data->{subset}{$_})
		{	push @$no_terms, $_;
		}
	}
	if ($no_terms && @$no_terms)
	{	if (scalar @$no_terms == scalar @{$options->{subset}})
		{	die "Error: no terms were found in any of the subsets specified. Dying";
		}
		else
		{	warn "Error: no terms were found for the following subset(s): " . join(", ", @$no_terms) . "\n";
		}
	}
}
else
{	# no terms in subset
	if (! $data->{subset}{ $options->{subset}[0] } )
	{	die "Error: no terms were found in subset " . $options->{subset}[0] . "! Dying";
	}
}

# get the relations and see how they relate to each other...
foreach (@{$graph->relations})
{	if ($graph->get_outgoing_links($_))
	{	foreach (@{$graph->get_outgoing_links($_)})
		{	$data->{relations}{graph}{$_->node->id}{$_->relation->id}{$_->target->id}++;
		}
	}
}

# merge the subsets into one if we want combined results
if ($options->{combined})
{	foreach my $s (keys %{$data->{subset}})
	{	map { $data->{combined}{$_} = 1 } keys %{$data->{subset}{$s}};
	}
	$data->{subset} = { combined => $data->{combined} };
	$options->{subset} = [ 'combined' ];
}

foreach my $subset (@{$options->{subset}})
{	
	# get rid of any existing data
	delete $data->{terms};
	
	# get all the links between terms in the subset or subset terms and root
	foreach my $t (sort keys %{$data->{subset}{$subset}})
	{	
		## asserted links
		foreach (@{ $graph->get_outgoing_links($t) })
		{	
			# skip it unless the target is a root or in the subset
			next unless $data->{subset}{$subset}{$_->target->id} || $data->{roots}{$_->target->id} ;
	
			$data->{terms}{graph}{$t}{$_->relation->id}{$_->target->id} = 1;
		}
	
		foreach (@{ $ie->get_inferred_target_links($t) })
		{	
			# skip it unless the target is a root or in the subset
			next unless $data->{subset}{$subset}{$_->target->id} || $data->{roots}{$_->target->id} ;
	
			# skip it if we already have this link
			next if defined #$data->{terms}{graph}{$t}{$_->target->id} &&
			$data->{terms}{graph}{$t}{$_->relation->id}{$_->target->id};
	
			## add to a list of inferred entries
			$data->{terms}{graph}{$t}{$_->relation->id}{$_->target->id} = 2;
		}
	}
	
	## populate the look up hashes
	populate_lookup_hashes($data->{terms});
	
	if (defined $data->{relations}{graph})
	{	populate_lookup_hashes($data->{relations});
	
		my $slimmed = go_slimmer($data->{relations});
	
		populate_lookup_hashes($slimmed);
	
		## slim down the relationships
		## get rid of redundant relations
		# these are the closest to the root
		foreach my $r (keys %{$slimmed->{target_node_rel}})
		{	foreach my $r2 (keys %{$slimmed->{target_node_rel}{$r}})
			{	# if both exist...
				if ($data->{terms}{rel_node_target}{$r} && $data->{terms}{rel_node_target}{$r2})
				{	
					# delete anything where we have the same term pairs with both relations
					foreach my $n (keys %{$data->{terms}{rel_node_target}{$r2}})
					{	if (defined $data->{terms}{graph}{$n}{$r})
					#	if ($data->{terms}{rel_node_target}{$r}{$n})
						{
							foreach my $t (keys %{$data->{terms}{rel_node_target}{$r2}{$n}})
							{	if (defined $data->{terms}{graph}{$n}{$r}{$t})
								{	
									delete $data->{terms}{graph}{$n}{$r}{$t};
									if (! values %{$data->{terms}{graph}{$n}{$r}})
									{	delete $data->{terms}{graph}{$n}{$r};
										if (! values %{$data->{terms}{graph}{$n}})
										{	delete $data->{terms}{graph}{$n};
										}
									}
	
								}
							}
						}
					}
				}
			}
		}
	}
	
	populate_lookup_hashes($data->{terms});
	
	my $slimmed = go_slimmer($data->{terms});
	
	my $new_graph = new GOBO::Graph;
	
	add_all_relations_to_graph($graph, $new_graph);
	add_extra_stuff_to_graph($graph, $new_graph);
	
	# add the terms to the graph
	
	foreach my $n ( keys %{$slimmed->{graph}} )
	{	# add the nodes to the graph
		$new_graph->add_term( $graph->noderef( $n ) ) if ! $new_graph->get_term($n);
	
		foreach my $r ( keys %{$slimmed->{graph}{$n}} )
		{	foreach my $t ( keys %{$slimmed->{graph}{$n}{$r}} )
			{	$new_graph->add_term( $graph->noderef( $t ) ) if ! $new_graph->get_term($t);
				$new_graph->add_link( new GOBO::LinkStatement(
					node => $new_graph->noderef($n),
					relation => $new_graph->noderef($r),
					target => $new_graph->noderef($t) 
				) );
			}
		}
	}
	
	if ($options->{basename})
	{	($options->{output} = $options->{basename}) =~ s/SLIM_NAME/$subset/;
	}

	my $writer = GOBO::Writers::OBOWriter->create(file=>$options->{output}, format=>'obo');
	$writer->graph($new_graph);
	$writer->write();

}

exit(0);


sub go_slimmer {
	my $d = shift;
	my $new_d;

	# for each node with a link to a 'target' (closer to root) node
	foreach my $id (keys %{$d->{node_target_rel}})
	{	
		# only connected to one node: must be the closest!
		if (scalar keys %{$d->{node_target_rel}{$id}} == 1)
		{	$new_d->{graph}{$id} = $d->{graph}{$id};
			next;
		}
		foreach my $rel (keys %{$d->{node_rel_target}{$id}})
		{	#	list_by_rel contains all the nodes between it and the root(s) of $id
			my @list_by_rel = keys %{$d->{node_rel_target}{$id}{$rel}};

			if (scalar @list_by_rel == 1)
			{	$new_d->{graph}{$id}{$rel} = $d->{node_rel_target}{$id}{$rel};
				next;
			}

			REL_SLIMDOWN_LOOP:
			while (@list_by_rel)
			{	my $a = pop @list_by_rel;
				my @list2_by_rel = ();
				while (@list_by_rel)
				{	my $b = pop @list_by_rel;
					if ($d->{target_node_rel}{$a}{$b})
					{	#	b is node, a is target
						#	forget about a, go on to the next list item
						push @list_by_rel, $b;
						push @list_by_rel, @list2_by_rel if @list2_by_rel;
						next REL_SLIMDOWN_LOOP;
					}
					elsif ($d->{node_target_rel}{$a}{$b})
					{	#	a is node, b is target
						#	forget about b, look at the next in the list
						next;
					}
					else
					{	#a and b aren't related
						#	keep b
						push @list2_by_rel, $b;
						next;
					}
				}
				#	if a is still around, it must be a descendent of
				#	all the terms we've looked at, so it can go on our
				#	descendent list
				$new_d->{graph}{$id}{$rel}{$a} = $d->{node_rel_target}{$id}{$rel}{$a};
	
				#	if we have a list2_by_rel, transfer it back to @list_by_rel
				push @list_by_rel, @list2_by_rel if @list2_by_rel;
			}
		}
	}
	return $new_d;
}


sub add_all_relations_to_graph {
	my $old_g = shift;
	my $new_g = shift;

	# add all the relations from the other graph
	foreach (@{$old_g->relations})
	{	$new_g->add_relation($old_g->noderef($_)) unless $_->id eq 'is_a';

		if ($old_g->get_outgoing_links($_))
		{	foreach (@{$old_g->get_outgoing_links($_)})
			{	
				$new_g->add_link( new GOBO::LinkStatement( 
					node => $old_g->noderef($_->node),
					relation => $old_g->noderef($_->relation),
					target => $old_g->noderef($_->target)
				) );
			}
		}
	}
}

sub add_extra_stuff_to_graph {
	my $old_g = shift;
	my $new_g = shift;
	foreach my $attrib qw( version source date comment declared_subsets property_value_map )
	{	$new_g->$attrib( $old_g->$attrib ) if $old_g->$attrib;
	}
}

sub populate_lookup_hashes {

	my $hash = shift;
	
	foreach my $k qw(node_target_rel target_node_rel node_rel_target target_rel_node rel_node_target rel_target_node)
	{	delete $hash->{$k};
	}
	
	foreach my $n (keys %{$hash->{graph}})
	{	foreach my $r (keys %{$hash->{graph}{$n}})
		{	foreach my $t (keys %{$hash->{graph}{$n}{$r}})
			{	$hash->{node_target_rel}{$n}{$t}{$r} = #1;
				$hash->{target_node_rel}{$t}{$n}{$r} = #1;
				$hash->{node_rel_target}{$n}{$r}{$t} = #1;
				$hash->{target_rel_node}{$t}{$r}{$n} = #1;
				$hash->{rel_node_target}{$r}{$n}{$t} = #1;
				$hash->{rel_target_node}{$r}{$t}{$n} = #1;
				$hash->{graph}{$n}{$r}{$t};
			}
		}
	}
}


=head1 NAME

go-slimdown.pl

=head1 SYNOPSIS

 go-slimdown.pl -i go/ontology/gene_ontology.obo -s goslim_generic -o slimmed.obo

=head1 DESCRIPTION

# must supply these arguments... or else!
 -i || --ontology /path/to/<file_name>   input file (ontology file) with path
 -s || --subset <subset_name>            name of the subset to extract

 -o || --output /path/to/<file_name>     output file with path
  or
 -b || --basename /path/to/<file_name_containing_SLIM_NAME>

      specify a file name containing the text "SLIM_NAME", which will be
      substituted with the name of the subset
      e.g. -s goslim_goa -s goslim_yeast -b /temp/gene_ontology.SLIM_NAME.obo 
      would produce two files,
      /temp/gene_ontology.goslim_goa.obo and /temp/gene_ontology.goslim_yeast.obo


# optional args
 -c || --combined                         if more than one subset is specified,
                                          create a slim using terms from all
                                          of the subsets specified
                                          
 -v || --verbose                          prints various messages

	Given a file where certain terms are specified as being in subset S, this
	script will 'slim down' the file by removing terms not in the subset.
	
	Relationships between remaining terms are calculated by the inference engine.
	
	If the root nodes are not already in the subset, they are added to the graph.
	
=cut
