use strict;
use Test::More 'no_plan';
use Data::Dumper;

use GOBO::DBIC::GODBModel::Graph;


## Old: Get args from the environment now, but these would be preferred.
my $query_args = {};
#$query_args->{host} ='localhost'; # or: $ENV: GO_DBHOST=localhost
#$query_args->{name} ='go';        # or: $ENV: GO_DBNAME=go

# Only test in a defined environment.
if( ! $ENV{GO_DBNAME} || ! $ENV{GO_DBHOST} ){
  warn "Won't test without at least GO_DBNAME and GO_DBHOST set.";
  ok( 1, "throw a bone to pass warning");
}else{

  ## Constructor.
  my $g = GOBO::DBIC::GODBModel::Graph->new($query_args);
  ok( defined($g), "is defined");

  ## Check roots.
  my $roots = $g->get_roots();
  is(scalar(keys %$roots), 3, "got all roots");
  foreach my $r (keys %$roots){
    ok($g->is_root_p($r), $r . " is a root");
  }

  ## Check kids.
  my $numk1 = $g->get_children("GO:0022008");
  ok( scalar(@$numk1) eq 5, "GO:0022008 has 5 kids");
  my $numk2 = $g->get_children(["GO:0016787"]);
  ok( scalar(@$numk2) eq 19, "GO:0016787 has 19 kids");
  my $numk3 = $g->get_children(["GO:0016787", "GO:0022008"]);
  ok( scalar(@$numk3) eq 24, "GO:0016787 and GO:0022008 have 24 kids");

  ## Check kid rels.
  my $numkr1 = $g->get_child_relationships("GO:0022008");
  ok( scalar(@$numk1) eq 5, "GO:0022008 has 5 kid rels");
  my $numkr2 = $g->get_child_relationships(["GO:0016787"]);
  ok( scalar(@$numk2) eq 19, "GO:0016787 has 19 kid rels");
  my $numkr3 = $g->get_child_relationships(["GO:0016787", "GO:0022008"]);
  ok( scalar(@$numk3) eq 24, "GO:0016787 and GO:0022008 have 24 kid rels");

  ## Get a couple lineage sets.
  my($lnodes1, $lnode_rel1, $lnode_rel_inf1, $lnode_depth1, $max_ldepth1) =
    $g->lineage('GO:0022008');
  my($lnodes2, $lnode_rel2, $lnode_rel_inf2, $lnode_depth2, $max_ldepth2) =
    $g->lineage(['GO:0007399']);
  # $g->kvetch('lnode_depth1: ' . Dumper($lnode_depth1));
  # $g->kvetch('lnode_depth2: ' . Dumper($lnode_depth2));

  ## Get a list of depths for each term.
  my $cache = {};
  my $fold_in = sub {
    my $inhash = shift || {};
    foreach my $k (keys %$inhash){
      if( ! $cache->{$k} ){ $cache->{$k} = []; }
      push @{$cache->{$k}}, $inhash->{$k};
    }
  };
  &$fold_in($lnode_depth1);
  &$fold_in($lnode_depth2);
  # $g->kvetch('folded: ' . Dumper($cache));

  ## Examine lists of length two, and see who has the highest
  ## collected depth.
  my $sums = {};
  foreach my $k (keys %$cache){
    if( scalar(@{$cache->{$k}}) == 2 ){
      $sums->{$k} = $cache->{$k}[0] + $cache->{$k}[1];
    }
  }
  # $g->kvetch('sums: ' . Dumper($sums));

  ## Sort according to accumulated depth.
  my @done = sort { $sums->{$b} <=> $sums->{$a} } keys(%$sums);
  # $g->kvetch('done: ' . Dumper(\@done));

  ## Ancestor deepest sums.
  ok( $done[0] eq 'GO:0007399', "GO:0007399 is accu highest");
  ok( $done[1] eq 'GO:0048731', "GO:0048731 is accu next");
}


