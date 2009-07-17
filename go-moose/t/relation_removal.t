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

use Test::More;


my $verbose = $ENV{GO_VERBOSE} || 0;

my $parser = new GOBO::Parsers::OBOParser(file=>"t/data/obo_file.obo");
#my $parser = new GOBO::Parsers::OBOParser(file=>"/Users/gwg/old_go/ontology/gene_ontology.1_2.obo");
$parser->parse;
	
my $subset = 'goslim_test';
my $data;
my $graph = $parser->graph;
my $ie = new GOBO::InferenceEngine(graph=>$graph);

## get all terms that are in a subset
foreach ( @{$graph->terms} )
{	next if $_->obsolete;
	## if it's in a subset, save the mofo.
	my $n = $_;
	if ($n->subsets)
	{	foreach (@{$n->subsets})
		{	$data->{subset}{$_->id}{$n->id}++;
		}
		$data->{all_slim_terms}{$n->id}++;
	}
	# make sure that we have all the root nodes
	elsif (! $graph->get_outgoing_links($n) || scalar @{$graph->get_outgoing_links($n)} == 0)
	{
		$data->{terms}{roots}{$n->id}++;
	}
}


print STDERR "terms in subset: ".join(", ", keys %{$data->{subset}{goslim_test}})."\n" if $verbose;
print STDERR "root nodes: " . join(", ", keys %{$data->{terms}{roots}} ) . "\n" if $verbose;

# get all the links between terms in the subset or subset terms and root
foreach my $t (sort keys %{$data->{subset}{$subset}})
{	
	## asserted links
	foreach (@{ $graph->get_outgoing_links($t) })
	{	
		# skip it unless the target is a root or in the subset
		next unless $data->{subset}{$subset}{$_->target->id} || $data->{terms}{roots}{$_->target->id} ;

		$data->{terms}{graph}{$t}{$_->relation->id}{$_->target->id} = 1;

#		print STDERR "ASS relation: $t -- " . $_->relation->id . " -- " . $_->target->id . "\n" if $verbose;
	}

	foreach (@{ $ie->get_inferred_target_links($t) })
	{	
		# skip it unless the target is a root or in the subset
		next unless $data->{subset}{$subset}{$_->target->id} || $data->{terms}{roots}{$_->target->id} ;

		# skip it if we already have this link
		next if defined #$data->{terms}{graph}{$t}{$_->target->id} &&
		$data->{terms}{graph}{$t}{$_->relation->id}{$_->target->id};

		## add to a list of inferred entries
		$data->{terms}{graph}{$t}{$_->relation->id}{$_->target->id} = 2;
		#print STDERR "INF relation: $t -- " . $_->relation->id . " -- " . $_->target->id . "\n" if $verbose;
	}
}

## populate the look up hashes
#$data->{terms} = 
populate_lookup_hashes($data->{terms});


# get the relations and see how they relate to each other...
foreach (@{$graph->relations})
{	if ($graph->get_outgoing_links($_))
	{	foreach (@{$graph->get_outgoing_links($_)})
		{	$data->{relations}{graph}{$_->node->id}{$_->relation->id}{$_->target->id}++;
		}
	}
}

if (defined $data->{relations}{graph})
{	populate_lookup_hashes($data->{relations}) ;

	my $slimmed = go_slimmer($data->{relations});

#	print STDERR "slimmed: " . Dumper($slimmed);
#	print STDERR "data->{relations}: " . Dumper($data->{relations});
	populate_lookup_hashes($slimmed);

	## slim down the relationships
	## get rid of redundant relations
	# these are the closest to the root
	foreach my $r (keys %{$slimmed->{target_node_rel}})
	{	print STDERR "r: $r\n".Dumper($slimmed->{target_node_rel}{$r});
		foreach my $r2 (keys %{$slimmed->{target_node_rel}{$r}})
		{	print STDERR "r2: $r2\n";
		
			print STDERR "looking at $r and $r2...\n" if $verbose;
			# if both exist...
			if ($data->{terms}{rel_node_target}{$r} && $data->{terms}{rel_node_target}{$r2})
			{	
	#			print STDERR "Found both $r and $r2 in our graph!\n" if $verbose;
	#			print STDERR "rel_outgoing $r: " . Dumper($data->{rel_node_target}{$r}) .
	#			"rel_outgoing $r2: " . Dumper($data->{rel_node_target}{$r2})."\n" if $verbose;
	
				# delete anything where we have the same term pairs with both relations
				foreach my $n (keys %{$data->{terms}{rel_node_target}{$r2}})
				{	if (defined $data->{terms}{graph}{$n}{$r})
				#	if ($data->{terms}{rel_node_target}{$r}{$n})
					{
	#					print STDERR "Found $n...\n" if $verbose;
						foreach my $t (keys %{$data->{terms}{rel_node_target}{$r2}{$n}})
						{	if (defined $data->{terms}{graph}{$n}{$r}{$t})
							{	#$data->{terms} = 
#								delete_lookup_hashes($data->{terms}, { n=>$n, r=>$r, t=>$t });

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

print STDERR "data graph: " . Dumper($data->{terms}{graph});
$Data::Dumper::Maxdepth = 4;
$Data::Dumper::Indent = 1;
#	print STDERR "graph: " . Dumper($graph);
my $new_graph = new GOBO::Graph;


my $slimmed = go_slimmer($data->{terms});

print STDERR "slimmed: ".Dumper($slimmed);

#print STDERR "done: " . Dumper($data->{done});
#print STDERR "rel_done: " . Dumper($data->{rel_done});
$data->{done} = $slimmed;

#	print STDERR "old graph: " . Dumper($graph) if $verbose;

my $new_rel_graph = new GOBO::Graph;

add_all_relations_to_graph($graph, $new_graph);
add_extra_stuff_to_graph($graph, $new_graph);

# add the nodes and their relationships to the graph
foreach my $n (keys %{$data->{done}{graph}})
{	# add the nodes to the graph
	$new_graph->add_term( $graph->noderef( $n ) ) if ! $new_graph->get_term($n);

	foreach my $r (keys %{$data->{done}{graph}{$n}})
	{	foreach my $t (keys %{$data->{done}{graph}{$n}{$r}})
		{	$new_graph->add_term( $graph->noderef( $t ) ) if ! $new_graph->get_term($t);
			$new_graph->add_link( new GOBO::LinkStatement(
				node => $new_graph->noderef($n),
				relation => $new_graph->noderef($r),
				target => $new_graph->noderef($t) ) );
		}
	}
}


#	print STDERR "graph: " . Dumper($new_graph) if $verbose;

	undef $graph;

	testme($new_graph);
	
	printme($new_graph, 'slimfile.obo');

#	printme($new_rel_graph, 'relslimfile.obo');


exit(0);


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
#	return $hash;
}

sub go_slimmer {
	my $data = shift;
	my $new_data;

	# for each node with a link to a 'target' (closer to root) node
	foreach my $id (keys %{$data->{node_target_rel}})
	{	
		# only connected to one node: must be the closest!
		if (scalar keys %{$data->{node_target_rel}{$id}} == 1)
		{	#$data->{done}{$id} = $data->{node_target_rel}{$id};
			$data->{rel_done}{$id} = $data->{node_rel_target}{$id};
			$new_data->{graph}{$id} = $data->{graph}{$id};
			next;
		}
		foreach my $rel (keys %{$data->{node_rel_target}{$id}})
		{	#	list_by_rel contains all the nodes between it and the root(s) of $id
			my @list_by_rel = keys %{$data->{node_rel_target}{$id}{$rel}};

			if (scalar @list_by_rel == 1)
			{	$data->{rel_done}{$id}{$rel} = $data->{node_rel_target}{$id}{$rel};
				$new_data->{graph}{$id}{$rel} = $data->{node_rel_target}{$id}{$rel};
				next;
			}

			print STDERR "\n\n$id $rel ancestors: " . join(", ", @list_by_rel) . "\n" if $verbose;
			REL_SLIMDOWN_LOOP:
			while (@list_by_rel)
			{	my $a = pop @list_by_rel;
	
				print STDERR "  a: $a\n" if $verbose;
	
				my @list2_by_rel = ();
				while (@list_by_rel)
				{	my $b = pop @list_by_rel;
	
					print STDERR "  Looking at b: $b\n" if $verbose;
	
					if ($data->{target_node_rel}{$a}{$b})
					{	

						print STDERR "  TO LEAF: $b --x--> $a = ".join(", ", keys %{$data->{target_node_rel}{$a}{$b}})."\n" if $verbose;

						#	b is node, a is target
						#	forget about a, go on to the next list item
						push @list_by_rel, $b;
						push @list_by_rel, @list2_by_rel if @list2_by_rel;
						next REL_SLIMDOWN_LOOP;
					}
					elsif ($data->{node_target_rel}{$a}{$b})
					{	
						print STDERR "  TO ROOT: $a --x--> $b = ".join(", ", keys %{$data->{node_target_rel}{$a}{$b}})."\n" if $verbose;

						#	a is node, b is target
						#	forget about b, look at the next in the list
						next;
					}
					else
					{	print STDERR "  $a and $b are not connected by $rel.\n" if $verbose;
						#a and b aren't related
						#	keep b
						push @list2_by_rel, $b;
						next;
					}
				}
				#	if a is still around, it must be a descendent of
				#	all the terms we've looked at, so it can go on our
				#	descendent list
				print STDERR "  $a is a descendent of all the other terms!\n" if $verbose;
				$data->{rel_done}{$id}{$rel}{$a} = $data->{node_rel_target}{$id}{$rel}{$a};
				$new_data->{graph}{$id}{$rel}{$a} = $data->{node_rel_target}{$id}{$rel}{$a};
	
				#	if we have a list2_by_rel, transfer it back to @list_by_rel
				push @list_by_rel, @list2_by_rel if @list2_by_rel;
			}
		}

	}

	return $new_data;
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
#	return $new_g;
}

sub add_extra_stuff_to_graph {
	my $old_g = shift;
	my $new_g = shift;
	foreach my $attrib qw( version source date comment declared_subsets property_value_map )
	{	$new_g->$attrib( $old_g->$attrib ) if $old_g->$attrib;
	}
}


=cut
GO:0000001 is_a GO:0000008
GO:0000001 part_of GO:0000008
GO:0000001 regulates GO:0000008
GO:0000002 is_a GO:0000006
GO:0000002 is_a GO:0000007
GO:0000003 part_of GO:0000007
GO:0000004 is_a GO:0000012
GO:0000004 positively_regulates GO:0000015
GO:0000004 negatively_regulates GO:0000016
GO:0000005 regulates GO:0000008
GO:0000006 is_a GO:0000009
GO:0000007 part_of GO:0000011
GO:0000008 negatively_regulates GO:0000010
GO:0000009 is_a GO:0000010
GO:0000010 is_a GO:0000018
GO:0000011 is_a GO:0000010
GO:0000012 is_a GO:0000013
GO:0000013 is_a GO:0000014
GO:0000014 is_a GO:0000018
GO:0000015 part_of GO:0000014
GO:0000016 is_a GO:0000014
GO:0000017 is_a GO:0000019
GO:0000018 is_a GO:0000019
GO:0000021 is_a GO:0000019
GO:0000022 is_a GO:0000021
GO:0000023 is_a GO:0000022
GO:0000024 is_a GO:0000023
GO:0000024 part_of GO:0000025
GO:0000025 part_of GO:0000019

negatively_regulates is_a regulates
positively_regulates is_a regulates

GS terms: 
GO:0000001
GO:0000002
GO:0000003
GO:0000004
GO:0000005
GO:0000006
GO:0000007
GO:0000010
GO:0000014
GO:0000015
GO:0000019
GO:0000024
GO:0000025

rlns we should therefore have:
GO:0000001 is_a GO:0000008 negatively_regulates GO:0000010       neg regs
GO:0000001 part_of GO:0000008 negatively_regulates GO:0000010    regs
GO:0000001 regulates GO:0000008 negatively_regulates GO:0000010  no rln
GO:0000002 is_a GO:0000006                                       is a
GO:0000002 is_a GO:0000007                                       is a
GO:0000002 is_a GO:0000007 part_of GO:0000011 is_a GO:0000010    part of
GO:0000003 part_of GO:0000007                                    part of
GO:0000004 is_a GO:0000012 is_a GO:0000013 is_a GO:0000014       is a
GO:0000004 pos_regs GO:0000015                                   pos regs
GO:0000004 neg_regulates GO:0000016 is_a GO:0000014              neg regs
GO:0000005 regulates GO:0000008 negatively_regulates GO:0000010  no rln
GO:0000006 is_a GO:0000010                                       is a
GO:0000007 part_of GO:0000011 is_a GO:0000010                    part of
GO:0000010 is_a GO:0000018 is_a GO:0000019                       is a
GO:0000014 is_a GO:0000018 is_a GO:0000019                       is a
GO:0000015 part_of GO:0000014                                    part of
GO:0000024 is_a ... is_a GO:0000021 is_a GO:0000019              is a
GO:0000024 part_of GO:0000025                                    part of
GO:0000025 part_of GO:0000019                                    part of

negatively_regulates is_a regulates
positively_regulates is_a regulates

=cut

sub testme {
	my $g = shift;

my $answers;

$answers->{"GO:0000001"}{negatively_regulates}{"GO:0000010"} = 1,
#$answers->{"GO:0000001"}{regulates}{"GO:0000010"} = 1,
$answers->{"GO:0000002"}{is_a}{"GO:0000006"} = 1;
$answers->{"GO:0000002"}{is_a}{"GO:0000007"} = 1;
$answers->{"GO:0000002"}{part_of}{"GO:0000010"} = 1;
$answers->{"GO:0000003"}{part_of}{"GO:0000007"} = 1;
$answers->{"GO:0000004"}{is_a}{"GO:0000014"} = 1;
$answers->{"GO:0000004"}{positively_regulates}{"GO:0000015"} = 1;
# $answers->{"GO:0000004"}{regulates}{"GO:0000015"} = 1;
$answers->{"GO:0000004"}{negatively_regulates}{"GO:0000014"} = 1;
# $answers->{"GO:0000004"}{regulates}{"GO:0000014"} = 1;
$answers->{"GO:0000006"}{is_a}{"GO:0000010"} = 1;
$answers->{"GO:0000007"}{part_of}{"GO:0000010"} = 1;
$answers->{"GO:0000010"}{is_a}{"GO:0000019"} = 1;
$answers->{"GO:0000014"}{is_a}{"GO:0000019"} = 1;
$answers->{"GO:0000015"}{part_of}{"GO:0000014"} = 1;
$answers->{"GO:0000024"}{is_a}{"GO:0000019"} = 1;
$answers->{"GO:0000024"}{part_of}{"GO:0000025"} = 1;
$answers->{"GO:0000025"}{part_of}{"GO:0000019"} = 1;
#$answers->{negatively_regulates}{is_a}{regulates} = 1;
#$answers->{positively_regulates}{is_a}{regulates} = 1;

plan tests => 17;

	
	my $summary;
#	my $infeng = new GOBO::InferenceEngine(graph=>$g);
	
	foreach my $t (sort { $a->id cmp $b->id } @{$g->terms})
	{	#my @links = @{ $infeng->get_inferred_target_links($t) };
		my @links = @{ $g->get_outgoing_links($t) };
		
#		print STDERR "links for " . $t->id . ": " . Dumper( \@links );
		
		foreach (sort { $a->target->id cmp $b->target->id } @links)
		{	
			print STDERR "\nnode: " . $_->node->id . ", target: " . $_->target->id . "\n" if $verbose;
	
			if ($answers->{$_->node->id}
				&& $answers->{$_->node->id}{$_->relation->id}
				&& $answers->{$_->node->id}{$_->relation->id}{$_->target->id} )
			{	# found the correct answer :D
				ok(1, "Checking ". $_->node->id . " " . $_->relation->id . " " . $_->target->id);
	
				print STDERR $_->node->id .": looking for ". join(" or ", keys %{$answers->{$_->node->id}} ) . ", found " . $_->relation->id . "\n" if $verbose;

				delete $answers->{$_->node->id}{$_->relation->id}{$_->target->id};

				if (! keys %{$answers->{$_->node->id}{$_->relation->id}})
				{	delete $answers->{$_->node->id}{$_->relation->id};
				}

				if (! keys %{$answers->{$_->node->id}})
				{	delete $answers->{$_->node->id};
				}
			}
			else
			{	# shouldn't have found a relation
				print STDERR $_->node->id .": found " . $_->relation->id . " " . $_->target->id . ", incorrect!\n" if $verbose;
				ok(0, $_->node->id .": incorrectly inferred relation " . $_->relation->id . " (none expected)");
				$summary->{$_->node->id}{$_->relation->id}{$_->target->id}++;
			}
		}
	}
	
	ok(! keys %$answers, "Checking we have no results left");

#	if ($verbose)
#	{	
		if (keys %$answers)
		{	print STDERR "Missing the following inferences:\n" . Dumper($answers);
		}
		if (keys %$summary)
		{	print STDERR "Made the following incorrect inferences:\n" . Dumper($summary);
		}
#	}
}

sub printme {
	my $g = shift;
	my $file = shift || 'slimfile.obo';
	my $writer = GOBO::Writers::OBOWriter->create(file=>$file, format=>'obo');
	$writer->graph($g);
	$writer->write();
}
