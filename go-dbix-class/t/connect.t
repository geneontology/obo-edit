
#require Test::Harness;

use strict;
#use Test::More tests => 1;
use Test::More 'no_plan';

use GODBModel::Graph;


## BUG: This is just mine for now as I get this sorted with the AmiGO
## stuff... -SJC

## Constructor.
my $q = GODBModel::Query->new({
			       host => 'localhost',
			       #name => 'go_latest_lite',
			       name => 'go',
			       type => 'term_lazy',
			      });
ok( defined($q), "is defined");

## Trivial query.
my $all_terms = $q->get_all_results({'me.acc' => 'GO:0008150'});
is(scalar(@$all_terms), 1, "got one for 'GO:0008150'");
my $t = $$all_terms[0];
is($t->name, 'biological_process', "walk out 1");

# ## Trivial walk out.
# my $all_terms = $q->get_all_results({'me.acc' => 'GO:43473',
# 				     'graph_path.distance' => 1});
# print STDERR "_n_" . scalar($all_terms) . "\n";

#my @d = $t->descendents;
#my @d = $t->ancestors;
#print STDERR "_n_" . scalar(@d) . "\n";
#foreach my $grp (@d){
#  print STDERR "___s: " . $grp->subject->acc . ', o: ' . $grp->object->acc. "\n";
#}

## Apparently, this isn't in the wild yet...do I have an old package?
#done_testing();
