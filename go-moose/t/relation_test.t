use Test::More tests => 29;
use strict;
use GOBO::Parsers::OBOParserDispatchHash;
use GOBO::InferenceEngine;
#use GOBO::Util::GraphFunctions;
use FileHandle;

use Data::Dumper;

my $fh = new FileHandle("t/data/relation_test.obo");
my $parser = new GOBO::Parsers::OBOParserDispatchHash(fh=>$fh);
$parser->parse;
my $graph = $parser->graph;
my $ie = new GOBO::InferenceEngine(graph=>$graph);

my $verbose = 0;

# key: GO:00000XY
# X and Y encode the relations; graph structure is term1 X term2 Y GO:0000000
# 1 => is_a
# 2 => part_of
# 3 => regulates,
# 4 => negatively_regulates,
# 5 => positively_regulates,
# 6 => has_part
# so GO:0000052 in the graph would have this relation:
# GO:0000052 positively_regulates GO:0000002 part_of GO:0000000

my $answers = {
		"GO:0000001" => { "is_a" => 1, },
		"GO:0000002" => { "part_of" => 1, },
		"GO:0000003" => { "regulates" => 1, },
		"GO:0000004" => { "negatively_regulates" => 1, "regulates" => 1, },
		"GO:0000005" => { "positively_regulates" => 1, "regulates" => 1, },
		"GO:0000006" => { "has_part" => 1, },

		"GO:0000011" => { "is_a" => 1, },
		"GO:0000012" => { "part_of" => 1, },
		"GO:0000013" => { "regulates" => 1, },
		"GO:0000014" => { "negatively_regulates" => 1, "regulates" => 1, },
		"GO:0000015" => { "positively_regulates" => 1, "regulates" => 1, },
		"GO:0000016" => { "has_part" => 1, },

		"GO:0000021" => { "part_of" => 1, },
		"GO:0000022" => { "part_of" => 1, },
	#	"GO:0000023" => "?",
	#	"GO:0000024" => "?",
	#	"GO:0000025" => "?",
	#	"GO:0000026" => "?",

		"GO:0000031" => { "regulates" => 1, },
		"GO:0000032" => { "regulates" => 1, },
	#	"GO:0000033" => "?",
	#	"GO:0000034" => "?",
	#	"GO:0000035" => "?",
	#	"GO:0000036" => "?",

		"GO:0000041" => { "negatively_regulates" => 1, "regulates" => 1, },
		"GO:0000042" => { "regulates" => 1, },
	#	"GO:0000043" => "?",
	#	"GO:0000044" => "?",
	#	"GO:0000045" => "?",
	#	"GO:0000046" => "?",

		"GO:0000051" => { "positively_regulates" => 1, "regulates" => 1, },
		"GO:0000052" => { "regulates" => 1, },
	#	"GO:0000053" => "?",
	#	"GO:0000054" => "?",
	#	"GO:0000055" => "?",
	#	"GO:0000056" => "?",

		"GO:0000061" => { "has_part" => 1, },
	#	"GO:0000062" => "?",
	#	"GO:0000063" => "?",
	#	"GO:0000064" => "?",
	#	"GO:0000065" => "?",
		"GO:0000066" => { "has_part" => 1, },
};

# dump the relations
#if ($verbose)
#{	foreach my $r (@{$g->relations})
#	{	if ($r->id ne 'is_a')
#		{	print  $r->id . ": " . Dumper($r);
#		}
#	}
#}

my $summary;

foreach my $t (sort { $a->id cmp $b->id } @{$graph->terms})
{	next if $t->id =~ /GO:PAD/;
	my @links = @{ $ie->get_inferred_outgoing_edges(node_id => $t->id) };

	foreach (sort { $a->target->id cmp $b->target->id } @links)
	{	next unless $_->target->id eq 'GO:0000000';

		print  "\nnode: " . $_->node->id . ", target: " . $_->target->id . "\n" . join("\n", map { $_->[1] } sort { $a->[0] cmp $b->[0] } map { [ $_->as_string, $_ ] } @links ) . "\n\n" if $verbose;

		if ($answers->{$_->node->id})
		{	if ($answers->{$_->node->id}{$_->relation->id})
			{	# found the correct answer :D
				ok(1, "Checking ". $_->node->id . " " . $_->relation->id);

				print  $_->node->id .": looking for ". join(" or ", keys %{$answers->{$_->node->id}} ) . ", found " . $_->relation->id . "\n" if $verbose;

				delete $answers->{$_->node->id}{$_->relation->id};

			}
			elsif ($answers->{$_->node->id})
			{	# found a relation, but it was wrong. Sob!
				ok(0, "Checking ". $_->node->id . " " . $_->relation->id);

				print  $_->node->id .": looking for ". join(" or ", keys %{$answers->{$_->node->id}} ) . ", found " . $_->relation->id . "\n" if $verbose;
				$summary->{$_->node->id}{$_->relation->id}++;
			}
			if (! keys %{$answers->{$_->node->id}})
			{	delete $answers->{$_->node->id};
			}
		}
		else
		{	# shouldn't have found a relation
			print  $_->node->id .": found " . $_->relation->id . ", should not be one\n" if $verbose;
			ok(0, $_->node->id .": incorrectly inferred relation " . $_->relation->id . " (none expected)");
			$summary->{$_->node->id}{$_->relation->id}++;
		}
	}
}

ok(! keys %$answers, "Checking we have no results left");

if ($verbose)
{	if (keys %$answers)
	{	print  "Missing the following inferences:\n" . Dumper($answers);
	}
	if (keys %$summary)
	{	print  "Made the following incorrect inferences:\n" . Dumper($summary);
	}
}

print STDERR "subrelation_closure_h:\n";
foreach (keys %{$ie->subrelation_closure_h})
{	print STDERR "$_: " . join(", ", map { $_->id } @{$ie->subrelation_closure_h->{$_}} ) . "\n";
}
print STDERR "\n\n";

print STDERR "subrelation_reflexive_closure_h:\n";
foreach (keys %{$ie->subrelation_closure_h})
{	print STDERR "$_: " . join(", ", map { $_->id } @{$ie->subrelation_reflexive_closure_h->{$_}} ) . "\n";
}
print STDERR "\n\n";

#print STDERR "subrelation_reflexive_closure_h: " . Dumper($ie->subrelation_reflexive_closure_h) . "\n\n";

print STDERR "relation_composition_h:\n";
foreach my $r1 (keys %{$ie->relation_composition_h})
{	foreach my $r2 (keys %{$ie->relation_composition_h->{$r1}})
	{	print STDERR "$r1 + $r2 => " . join("; ", @{$ie->relation_composition_h->{$r1}{$r2}} ). "\n";
	}
}

#Dumper($ie->relation_composition_h) . "\n\n";

foreach ($graph->get_statement_ix_h_names)
{	print STDERR "key: $_; links:\n" . join("\n", map { $_->[1] }
	sort { $a->[0] cmp $b->[0] }
	map { [ $_->as_string, $_ ] } @{$graph->get_all_statements_in_ix($_)} ) . "\n\n";
}

