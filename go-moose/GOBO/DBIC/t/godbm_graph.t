use strict;
use Test::More 'no_plan';

use GOBO::DBIC::GODBModel::Graph;

###
### BUG: This is just mine for now as I get this sorted with the AmiGO
### stuff... -SJC
###

## Constructor.
my $g = GOBO::DBIC::GODBModel::Graph->new({
					   host => 'localhost',
					   name => 'go'
					  });
ok( defined($g), "is defined");

## Check roots.
my $roots = $g->get_roots();
is(scalar(keys %$roots), 3, "got all roots");
foreach my $r (keys %$roots){
  ok($g->is_root_p($r), $r . " is a root");
}
