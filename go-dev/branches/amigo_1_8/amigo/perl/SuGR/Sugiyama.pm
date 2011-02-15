=head1 SuGR::Sugiyama

Translate a graph into a discrete data representation (which will be
then passed to an actual rendering object). This should contain all of
the heavy lifting for the layout.

Trying to translate the JS sugiyama stuff directly into perl.

TODO: Matrix implementation and partition->matrix step need to be
tightened.

BUG: need to check if there are no edges.

=cut

package SuGR::Sugiyama;

use strict;
use utf8;
use POSIX qw(floor);
use base 'SuGR';
use SuGR::Partition;
use SuGR::BaryMatrix;

use Data::Dumper;

#use foo;
my $tg = SuGR::TrivialGraph->new();


=item new

Constr.

=cut
sub new {

  ## SHIFT IN (graph, rel)

  my $class = shift;
  my $self = {
	      LOG => Log::Log4perl->get_logger('SuGR::Sugiyama'),

	      ## Core variables.
	      ITERATIONS => 10,
	     };

  bless $self, $class;
  return $self;
}


## TODO/BUG: half done JS -> perl
# =item dump

# Dump partition.

# =cut
# sub dump {

#   my $self = shift;

#   ## Dump vertex partitions.
#   my $num_parts = 0;
#   foreach my $key (keys %{$self->{vertex_partition_set}}){
#     $num_parts++;
#   }
#   for( var i = 0; i < num_parts; i++ ){
#     print STDERR 'Vertex Partition ' . i . ':'
#       if( $self->{DEBUG} );
#     my $curr_part = vertex_partition_set[ i ];
#     my $out = [];
#     for( var j = 0; j < curr_part.length; j++ ){
#       out.push('[' + curr_part[j].id() + ']');
#     }
#     if( GLOBAL_SUGIYAMA_JS_DEBUG ) print(out.join(''));
#   }

#   // Dump edge partitions.
#     num_parts = 0;
#   for( var key in edge_partition_set ){
#     num_parts++; }
#   for( var i = 0; i < num_parts; i++ ){
#     if( GLOBAL_SUGIYAMA_JS_DEBUG ) print('Edge Partition ' + i + ':');
#     var curr_part = edge_partition_set[ i ];
#     var out = new Array;
#     for( var j = 0; j < curr_part.length; j++ ){
#       out.push('[' + curr_part[j].id() + ']');
#     }
#     if( GLOBAL_SUGIYAMA_JS_DEBUG ) print(out.join(''));
#   }

#   // Dump paths list.
#     for( var i = 0; i < logical_paths.length; i++ ){
#       if( GLOBAL_SUGIYAMA_JS_DEBUG ) print('Path ' + i + ':')
# 	var out = new Array;
#       for( var l = 0; l < logical_paths[i].length; l++ ){
# 	out.push( logical_paths[i][l]);
#       }
#       if( GLOBAL_SUGIYAMA_JS_DEBUG ) print(out.join(', '));
#     }
# }


=item layout

Takes a graph.
Can be queried for the position of every node and edge.

=cut
sub layout {

  my $self = shift;

  my $graph = shift;
  #my $relation_id = shift;

  ###
  ### Step I: Make a proper hierarchy; partition the graph over
  ### 'is_a'. $partitioner is the takeaway result of this section.
  ###

  ##my $partitions = new SugiPartition(g, 'is_a');
  #my $partitions = $partitioner->partition($graph, $relation_id);
  my $partitioner = SuGR::Partition->new();
  $partitioner->partition($graph);

  ## DEBUG:
  #$partitions->pdump();

  ###
  ### Step II: Reduce number of crossings by vertex permutation.
  ###

  ## Get the edge and vertex partitions.
  ## BUG: Need to catch num_partitions < 2. Create an instantiation of
  ## all of the matrix representations of the partitions.
  my @edge_partitions = ();
  my @vertex_partitions = ();
  for( my $i = 0; $i < $partitioner->number_of_edge_partitions(); $i++ ){
    push @edge_partitions, $partitioner->get_edge_partition($i);
  }
  for( my $i = 0; $i < $partitioner->number_of_vertex_partitions(); $i++ ){
    push @vertex_partitions, $partitioner->get_vertex_partition($i);
  }

  ## Step through the edge partitions (and the associated vertex
  ## partitions), rearranging them as we go.
  for( my $k = 0; $k < $self->{ITERATIONS}; $k++ ){

    for( my $i = 0; $i < scalar(@edge_partitions); $i++ ){

      $self->{LOG}->debug('Vertex (over) partition set:');
      $self->{LOG}->debug(Dumper($vertex_partitions[$i]));
      $self->{LOG}->debug('Edge partition set: ');
      $self->{LOG}->debug(Dumper($edge_partitions[$i]));
      $self->{LOG}->debug('Vertex (under) partition set:');
      $self->{LOG}->debug(Dumper($vertex_partitions[$i+1]));

      ## BUG?: I hope that it's destructive on the other end...
      ## TODO: This seems to be working correctly now...
      my $m = SuGR::BaryMatrix->new($vertex_partitions[$i],
				    $vertex_partitions[$i +1],
				    $edge_partitions[$i]);

      $self->{LOG}->debug('Barymatrix (before):');
      #$self->{LOG}->debug($m);
      $m->bdump();

      ## TODO: Can increase the number of iterations--the paper doesn't
      ## really explain this. Perhaps this loop should encompass the
      ## edge stepper above instead?
      $self->{LOG}->debug("Now reordering...");
      $m->barycentric_object_reorder();
      $m->barycentric_subject_reorder();

      #sleep 2;

      $self->{LOG}->debug('Barymatrix (after):');
      #$self->{LOG}->debug($m);
      $m->bdump();

      ##
      $vertex_partitions[$i] = $m->reordered_object_partition;
      $vertex_partitions[$i+1] = $m->reordered_subject_partition;
    }
  }

  ###
  ### Step III: give proper integer X and Y positions: suspend them in
  ### a matrix.
  ###

  ## Populate matrix and register final locations of nodes for later.
  ## TODO: Sugiyama method. Temporarily did naive method.
  my %vertex_locations = ();
  #my $vertex_registry = {};
  my $m = $partitioner->max_partition_width();
  for( my $i = 0; $i < scalar(@vertex_partitions); $i++ ){

    my $l = scalar(@{$vertex_partitions[$i]});

    for( my $v = 0; $v < $l; $v++ ){

      my $vert = $vertex_partitions[$i][$v];
      my $vid = $vert->{id};
      #my $locale = floor( ($v+1) * ($m/$l/2) );

      my $position =($v+1);
      my $proportion = ($position / $l);
      my $scale = $m;
      my $real = $proportion * $scale;
      my $locale = floor($real) -1;

      $self->{LOG}->debug('Looking at:' . $vid );
      $self->{LOG}->debug("\tposition is: " . $position);
      $self->{LOG}->debug("\tproportion is: " . $proportion);
      $self->{LOG}->debug("\tscale is: " . $scale);
      $self->{LOG}->debug("\treal is: " . $real);
      $self->{LOG}->debug("\tfloor is: " . $locale);

      #$vertex_registry->{$vid} = {x => $locale, y => $i};
      #if( ! $vertex_partitions[$i][$v]->{virtual_p} ){
      my $vp = $vertex_partitions[$i][$v]->{virtual_p};
      $vertex_locations{$vid} = {
				 x => $locale,
				 y => $i,
				 id => $vid,
				 virtual_p => $vp,
				};
      #}
    }
  }

  ## Now that we have all of the nodes locations, convert logical
  ## paths to actual paths.
  my $logical_paths = $partitioner->get_logical_paths();
  ## TODO: it looks like the loop here can just be removed...
  #my @actual_paths = ();
  foreach my $logical_path (@$logical_paths){
    $self->{LOG}->debug('New path...');
    for( my $i = 0; $i < scalar(@{$logical_path}); $i++ ){
      $self->{LOG}->debug("\tlogical_path[" . $i . "]..." . $$logical_path[$i]);
      #my $x = $$logical_paths[$i][$j]->{x};
      #my $y = $$logical_paths[$i][$j]->{y};
      #push @actual_paths, { x => $x, y => $y };
    }
  }

  ## Return this baddy to the world.
  return
    {
     locations => \%vertex_locations,
     #paths => \@actual_paths,
     paths => $logical_paths,
     width => $partitioner->max_partition_width(),
     height => $partitioner->number_of_vertex_partitions(),
    };
}
