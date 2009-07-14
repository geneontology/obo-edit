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

use Test;
plan tests => 1;


my $verbose = $ENV{GO_VERBOSE} || 1;

my $parser = new GOBO::Parsers::OBOParser(file=>"t/data/obo_file.obo");
$parser->parse;

my $data;
my $graph = $parser->graph;
my $ie = new GOBO::InferenceEngine(graph=>$graph);

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
	
}

#foreach ( sort { $a->node->id cmp $b->node->id 
#|| $a->relation->id cmp $b->relation->id
#|| $a->target->id cmp $b->target->id } @{$graph->links} )
#{	print STDERR "ASS: " . $_->node->id . " " . $_->relation->id . " " . $_->target->id . "\n";
#}
#my $acc;
#foreach my $t (keys %{$data->{all_slim_terms}})
#foreach my $subset (keys %{$data->{subset}})
#{
	my $subset = 'goslim_test';
	foreach my $t (sort keys %{$data->{subset}{$subset}})
	{	## keep links to GS terms.
		print STDERR "term: $t\n" if $verbose;
		foreach (@{ $graph->get_target_links($t) })
		{	print STDERR "ASS relation: $t -- " . $_->relation->id . " -- " . $_->target->id . "\n" if $verbose;
		#	$acc->{$_->target->id}++;
			next unless $data->{subset}{$subset}{$_->target->id};

			$data->{graph}{$t}{$_->target->id}{$_->relation->id} = 'ass'; # = $_->relation;
			$data->{to_root}{$t}{$_->target->id}{$_->relation->id} = 'ass';
			$data->{to_leaf}{$_->target->id}{$t}{$_->relation->id} = 'ass';
		}

		foreach (@{ $ie->get_inferred_target_links($t) })
		{#	$acc->{$_->target->id}++;
			print STDERR "INF relation: $t -- " . $_->relation->id . " -- " . $_->target->id . "\n" if $verbose;
	#		next unless $data->{all_slim_terms}{$_->target->id};
			next unless $data->{subset}{$subset}{$_->target->id};
			# skip it if we already have this link
			next if $data->{graph}{$t}{$_->target->id} &&
			$data->{graph}{$t}{$_->target->id}{$_->relation->id};

			## add to a list of inferred entries
			$data->{graph}{$t}{$_->target->id}{$_->relation->id} = 'inf'; # = $_->relation;
			$data->{to_root}{$t}{$_->target->id}{$_->relation->id} = 'inf';
			$data->{to_leaf}{$_->target->id}{$t}{$_->relation->id} = 'inf';
		}
	}




#	print STDERR "data graph: " . Dumper($data->{graph});
$Data::Dumper::Maxdepth = 4;
$Data::Dumper::Indent = 1;
#	print STDERR "graph: " . Dumper($graph);
	my $new_graph = new GOBO::Graph;

# for each term in with a connection to a parent term
	foreach my $id (keys %{$data->{to_root}})
	{	#	list contains all the parents of $id
		my @list = keys %{$data->{to_root}{$id}};
		print STDERR "id: $id\n" if $verbose;
		SLIMDOWN_LOOP:
		while (@list)
		{	my $a = pop @list;
			my @list2 = ();
			while (@list)
			{	my $b = pop @list;
				if ($data->{to_leaf}{$a}{$b})
				{	#	a is an ancestor of b
					#	forget about a, go on to the next list item
					push @list, $b;
					push @list, @list2 if @list2;
					next SLIMDOWN_LOOP;
				}
				elsif ($data->{to_root}{$a}{$b})
				{	#	b is an ancestor of a
					#	forget about b, look at the next in the list
					next;
				}
				else
				{	#	a and b aren't related
					#	keep b
					push @list2, $b;
					next;
				}
			}
			#	if a is still around, it must be a descendent of
			#	all the terms we've looked at, so it can go on our
			#	descendent list
			$data->{done}{$id}{$a} = $data->{to_root}{$id}{$a};
			#	if we have a list2, transfer it back to @list
			push @list, @list2 if @list2;
		}
	}


	$Data::Dumper::Maxdepth = 2;
#	print STDERR "old graph: " . Dumper($graph) if $verbose;



	foreach my $a (keys %{$data->{done}})
	{	foreach my $b (keys %{$data->{done}{$a}})
		{	map { $data->{new_graph}{rels}{$_}++ } keys %{$data->{done}{$a}{$b}};
			$data->{new_graph}{all_terms}{$b}++;
		}
		$data->{new_graph}{all_terms}{$a}++;
	}

	# add the GS terms
	foreach (keys %{$data->{new_graph}{all_terms}})
	{	$new_graph->add_term( $graph->noderef( $_ ) );
	}

	# add all the relations from the other graph
	foreach (@{$graph->relations})
	{	$new_graph->add_relation($graph->noderef($_)) unless $_->id eq 'is_a';
		if ($graph->get_target_links($_))
		{	foreach (@{$graph->get_target_links($_)})
			{	$new_graph->add_link( new GOBO::LinkStatement( node => $graph->noderef($_->node),
					relation => $graph->noderef($_->relation),
					target => $graph->noderef($_->target) ) );
			}
		}
	}



	foreach my $a (keys %{$data->{done}})
	{	foreach my $b (keys %{$data->{done}{$a}})
		{	foreach my $rel (keys %{$data->{done}{$a}{$b}})
			{	my $s = new GOBO::LinkStatement(
					node => $graph->noderef($a),
					relation => $graph->noderef($rel),
					target => $graph->noderef($b)
				);
				$new_graph->add_link($s);
			}
		}
	}


	## these are the relationships in the file:
=cut
GO:0000001 is_a GO:0000008
GO:0000001 part_of GO:0000008
GO:0000001 regulates GO:0000008
GO:0000002 is_a GO:0000006
GO:0000002 part_of GO:0000007
GO:0000003 part_of GO:0000007
GO:0000004 is_a GO:0000012
GO:0000004 part_of GO:0000015
GO:0000004 regulates GO:0000016
GO:0000005 regulates GO:0000008
GO:0000006 is_a GO:0000009
GO:0000007 is_a GO:0000011
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
GO:0000009
GO:0000010
GO:0000014
GO:0000019

rlns we should therefore have:
GO:0000001 is_a GO:0000008 negatively_regulates GO:0000010       neg_reg
GO:0000001 part_of GO:0000008 negatively_regulates GO:0000010    no rln
GO:0000001 regulates GO:0000008 negatively_regulates GO:0000010  no rln
GO:0000002 is_a GO:0000006                                       is a
GO:0000002 part_of GO:0000007                                    part of
GO:0000003 part_of GO:0000007                                    part of
GO:0000004 is_a GO:0000012 is_a GO:0000013 is_a GO:0000014       is_a
GO:0000004 part_of GO:0000015 part_of GO:0000014                 part_of
GO:0000004 regulates GO:0000016 is_a GO:0000014                  regulates
GO:0000005 regulates GO:0000008 negatively_regulates GO:0000010  no rln
GO:0000006 is_a GO:0000009                                       is_a
GO:0000007 is_a GO:0000011 is_a GO:0000010                       is_a
GO:0000009 is_a GO:0000010                                       is_a
GO:0000010 is_a GO:0000018 is_a GO:0000019                       is_a
GO:0000014 is_a GO:0000018 is_a GO:0000019                       is_a

negatively_regulates is_a regulates
positively_regulates is_a regulates

=cut

my $r_hash = {
'GO:0000001negatively_regulatesGO:0000010' => 1,
'GO:0000002is_aGO:0000006' => 1,
'GO:0000002part_ofGO:0000007' => 1,
'GO:0000003part_ofGO:0000007' => 1,
'GO:0000004is_aGO:0000014' => 1,
'GO:0000004part_ofGO:0000014' => 1,
'GO:0000004regulatesGO:0000014' => 1,
'GO:0000006is_aGO:0000009' => 1,
'GO:0000007is_aGO:0000010' => 1,
'GO:0000009is_aGO:0000010' => 1,
'GO:0000010is_aGO:0000019' => 1,
'GO:0000014is_aGO:0000019' => 1,
'negatively_regulatesis_aregulates' => 1,
'positively_regulatesis_aregulates' => 1,
};

	## query to check that we have all the relations we should have
foreach ( sort { $a->node->id cmp $b->node->id 
|| $a->relation->id cmp $b->relation->id
|| $a->target->id cmp $b->target->id } @{$new_graph->links} )
{	#print STDERR "INF: " . $_->node->id . " " . $_->relation->id . " " . $_->target->id . "\n";
	if ($r_hash->{ $_->node->id . $_->relation->id . $_->target->id })
	{	delete $r_hash->{ $_->node->id . $_->relation->id . $_->target->id };
	}
	else 
	{	print STDERR "unexpected relationship: " . $_->node->id . " " . $_->relation->id . " " . $_->target->id . "\n" if $verbose;
	}
}

if (keys %$r_hash)
{	print STDERR "rels not found: " . Dumper($r_hash) if $verbose;
}
else
{	ok(scalar keys %$r_hash == 0, 1, "Checking we have all relations" );
	print STDERR "Found all relations - HURRAH!\n\n" if $verbose;
}


#	foreach my $attrib qw( version source date comment declared_subsets property_value_map )
#	{	$new_graph->$attrib( $graph->$attrib ) if $graph->$attrib;
#	}
#	print STDERR "new graph: " . Dumper($new_graph) if $verbose;


#my $writer = GOBO::Writers::OBOWriter->create(file=>'slimfile.obo', format=>'obo');
#$writer->graph($new_graph);
#$writer->write();
