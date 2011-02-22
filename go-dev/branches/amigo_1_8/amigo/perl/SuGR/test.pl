#!/usr/bin/perl -w
####
#### Try:
####   reset; perl -I /home/sjcarbon/local/src/cvs/go-dev/amigo/perl ./test
####

use strict;
use utf8;

use Data::Dumper;

use SuGR::Render;
use SuGR::Partition;
use Graph::Directed;
use SuGR::BaryMatrix;
use SuGR::Sugiyama;

# {
#   ## Test run for partitioner.
#   my $g = Graph::Directed->new;
#   $g->add_vertex('a');
#   $g->add_vertex('b');
#   $g->add_vertex('c');
#   $g->add_edge('b', 'a');
#   $g->add_edge('c', 'b');
#   $g->add_edge('c', 'a');

#   my $parter = SuGR::Partition->new();
#   $parter->partition($g);
#   $parter->pdump();

#   ## Should move these to test code:
#   print "Max width: " . $parter->max_partition_width() . "\n";
#   print "Vertex paritions: " . $parter->number_of_vertex_partitions() . "\n";
#   print "Edge paritions: " . $parter->number_of_edge_partitions() . "\n";

#   ## Test run for bm.
#   my $bm = SuGR::BaryMatrix->new($parter->get_vertex_partition(0),
# 				 $parter->get_vertex_partition(1),
# 				 $parter->get_edge_partition(0));
#   #$bm->bdump();
#   $bm->barycentric_object_reorder();
#   $bm->barycentric_subject_reorder();
#   $bm->bdump();
# }

## Simulate slightly complicated graph.
{
  ## Test run for partitioner.
  my $g = Graph::Directed->new;
  $g->add_vertex('a');
  $g->add_vertex('b');
  $g->add_vertex('c');
  $g->add_edge('b', 'a');
  $g->add_edge('c', 'b');
  $g->add_edge('c', 'a');

  $g->add_vertex('x');
  $g->add_edge('x', 'a');

  my $sg = SuGR::Sugiyama->new();
  my $layout = $sg->layout($g);

  ## Quick render check.
  my $monkey = SuGR::Render->new();
  $monkey->render($layout);
  $monkey->to_png('test1.output.png');

  print STDERR Dumper($monkey->to_world());
}

## Simulate top roots.
{
  ## Test run for partitioner.
  my $g = Graph::Directed->new;
  $g->add_vertex('a');
  $g->add_vertex('b');
  $g->add_vertex('c');

  my $sg = SuGR::Sugiyama->new();
  my $layout = $sg->layout($g);

  ## Quick render check.
  my $monkey = SuGR::Render->new();
  $monkey->render($layout);
  $monkey->to_png('test2.output.png');

  print STDERR Dumper($monkey->to_world());
}

# {
#   ## Test run for partitioner.
#   my $g = Graph::Directed->new;
#   $g->add_vertex('a1');
#   $g->add_vertex('b1');
#   $g->add_vertex('c1');
#   $g->add_vertex('a2');
#   $g->add_vertex('b2');
#   $g->add_vertex('c2');
#   $g->add_edge('a2', 'a1');
#   $g->add_edge('b2', 'b1');
#   $g->add_edge('c2', 'c1');

#   my $sg = SuGR::Sugiyama->new();
#   $sg->layout($g);
# }
