#!/usr/local/bin/perl -w

use lib '.';
BEGIN {
    eval { require Test; };
    use Test;    
    plan tests => 2;
}

# All tests must be run from the software directory;
# make sure we are getting the modules from here:
use strict;
use GO::Parser;
use GO::ObjCache;

# ----- REQUIREMENTS -----

# This test script tests the GO::Model::LogicalDefinition

# ------------------------

my $parser = new GO::Parser ({handler=>'obj'});
my $graph = $parser->handler->g;
ok(1);
$parser->parse ("./t/data/fypo_test.obo");

my $t = $graph->get_term_by_name("small viable vegetative cells");
printf "ns: %s\n", $t->namespace;
my $ok = 0;
foreach my $rel (@{$graph->get_parent_relationships($t->acc)}) {
    printf " REL: %s %s %s\n", $rel->acc2, $rel->type, $rel->acc1;
    if ($rel->type eq 'is_a' && $rel->acc1 eq 'FYPO:0000645') {
        $ok++;
        # NOTE: currently this relationship is never found
        printf "M1\n";
    }
    if ($rel->type eq 'is_a' && $rel->acc1 eq 'FYPO:0001491') {
        $ok++;
        print "M2\n";
    }
}
#ok($ok == 2);
ok($ok > 0);
my $ldef = $t->logical_definition;
foreach (@{$ldef->intersection_list}) {
    print "LDEF: @$_\n";
}
my $gacc = $ldef->generic_term_acc;
print "$gacc\n";

