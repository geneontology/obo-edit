use Test::More tests => 19;
use strict;
use GOBO::Graph;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::OBOParser;
use GOBO::Writers::OBOWriter;
use GOBO::InferenceEngine;
use FileHandle;

use Data::Dumper;

my $fh = new FileHandle("t/data/relation_test.obo");
my $parser = new GOBO::Parsers::OBOParser(fh=>$fh);
$parser->parse;
my $g = $parser->graph;
my $ie = new GOBO::InferenceEngine(graph=>$g);

# key: GO:00000XY
# X and Y encode the relations; graph structure is term1 X term2 Y GO:0000000
# 1 => is_a
# 2 => part_of
# 3 => regulates,
# 4 => negatively_regulates,
# 5 => positively_regulates,
# so GO:0000052 in the graph would have this relation:
# GO:0000052 positively_regulates GO:0000002 part_of GO:0000000

my $answers = {
		"GO:0000001" => "is_a",
		"GO:0000002" => "part_of",
		"GO:0000003" => "regulates",
		"GO:0000004" => "negatively_regulates",
		"GO:0000005" => "positively_regulates",
	#	"GO:0000006" => "has_part",

		"GO:0000011" => "is_a",
		"GO:0000012" => "part_of",
		"GO:0000013" => "regulates",
		"GO:0000014" => "negatively_regulates",
		"GO:0000015" => "positively_regulates",
	#	"GO:0000016" => "has_part",

		"GO:0000021" => "part_of",
		"GO:0000022" => "part_of",
	#	"GO:0000023" => '?',
	#	"GO:0000024" => '?',
	#	"GO:0000025" => '?',
	#	"GO:0000026" => '?',

		"GO:0000031" => "regulates",
		"GO:0000032" => "regulates",
	#	"GO:0000033" => '?',
	#	"GO:0000034" => '?',
	#	"GO:0000035" => '?',
	#	"GO:0000036" => '?',

		"GO:0000041" => "negatively_regulates",
		"GO:0000042" => "regulates",
	#	"GO:0000043" => '?',
	#	"GO:0000044" => '?',
	#	"GO:0000045" => '?',
	#	"GO:0000046" => '?',

		"GO:0000051" => "positively_regulates",
		"GO:0000052" => "regulates",
	#	"GO:0000053" => '?',
	#	"GO:0000054" => '?',
	#	"GO:0000055" => '?',
	#	"GO:0000056" => '?',

	#	"GO:0000061" => "has_part",
	#	"GO:0000062" => '?',
	#	"GO:0000063" => '?',
	#	"GO:0000064" => '?',
	#	"GO:0000065" => '?',
	#	"GO:0000066" => "has_part",
};	

foreach my $t (@{$g->terms})
{	my @links = @{ $ie->get_inferred_target_links($t) };
        foreach (@links)
	{	next unless $_->target->id eq 'GO:0000000';
		if ($answers->{$_->node->id})
		{	if ($_->relation->id eq $answers->{$_->node->id}) 
                        {
                            delete $answers->{$_->node->id};
                            ok(1);
                        }
		}
	}
        if ($answers->{$t}) {
            printf "expected: $t => $answers->{$t}\n";
            ok(0);
        }
}

ok(! keys %$answers, "Checking we have no results left");
