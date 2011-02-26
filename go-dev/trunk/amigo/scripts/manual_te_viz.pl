#!/usr/local/bin/perl
####    
#### Takes file like:
####    GO:0019222 3.91e-20
####    GO:0016070 1.96e-16
####    GO:0044424 2.96e-16
####    GO:0006139 4.95e-16
####    GO:0043283 4.97e-16
####    GO:0050789 4.97e-16
####    GO:0005515 2.46e-15
####    GO:0031323 4.06e-15
####    GO:0043231 5.58e-15
####
#### And produces vizualize-usable JSON output.
####
#### Usage:
####    sjcarbon@moiraine:~/local/src/svn/geneontology/go-dev/trunk/amigo$:) perl ./scripts/manual_te_viz.pl /tmp/test1.txt
####
#### Cut and paste output into visualize web tool.
####

BEGIN { require "config.pl" if -f "config.pl" ; }
use lib "$ENV{GO_DEV_ROOT}/amigo/perl";

use strict;
use AmiGO::Aid;

## Kvetcher.
sub ll {
  my $s = shift || '';
  #print STDERR $s . "\n";
  print $s . "\n";
}

## All values in.
my $idpvals = {};
while(<>){
  my($id, $pval) = split;
  $idpvals->{$id} = $pval;
}

## Emitter.
my $core = AmiGO::Aid->new();
my $str = $core->pvals_to_json($idpvals);
ll($str);
