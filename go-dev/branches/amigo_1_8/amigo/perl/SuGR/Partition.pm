=head1 SuGR::Partition

Cut the graph into levels.

BUG/TODO: grr...let's just go ahead and wrap the perl Graph.pm module
for the time being (this will mean that all relations are treated the
same).

=cut

package SuGR::Partition;

use strict;
use utf8;
use base 'SuGR';

use SuGR::TrivialGraph;
use Graph::Directed;


##
my $trivial_graph = SuGR::TrivialGraph->new();


=item new

Constr.

=cut
sub new {

  my $class = shift;
  my $self = {
	      LOG => Log::Log4perl->get_logger('SuGR::Partition'),

	      ## Common information.
	      first_seen_reference => {},
	      last_seen_reference => {},

	      vertex_set => {},
	      edge_set => {},

	      vertex_partition_set => {},
	      edge_partition_set => {},

	      logical_paths => [],

	      maximum_partition_width => 0,
	      #number_of_partitions => 0,
	     };

  bless $self, $class;
  return $self;
}


=item max_partition_width

Return the maximum partition width.

=cut
sub max_partition_width {

  my $self = shift;
  return $self->{maximum_partition_width};
}


=item number_of_vertex_partitions

Return the number of vertex partitions.


=cut
sub number_of_vertex_partitions {

  my $self = shift;
  return scalar (keys %{$self->{vertex_partition_set}});
}


=item get_vertex_partition

Get the ith vertex partition.

=cut
sub get_vertex_partition {

  my $self = shift;
  my $index = shift || 0;

  return $self->{vertex_partition_set}{$index};
}


=item number_of_edge_partitions

Return the number of edge partitions.

=cut
sub number_of_edge_partitions {

  my $self = shift;
  return scalar (keys %{$self->{edge_partition_set}});
}


=item get_edge_partition

Return a partition.

=cut
sub get_edge_partition {

  my $self = shift;
  my $integer = shift || 0;

  return $self->{edge_partition_set}{$integer};
}


=item number_of_logical_paths

Return the number of paths.

=cut
sub number_of_logical_paths {

  my $self = shift;
  return scalar(@{$self->{logical_paths}});
}


=item get_logical_paths

Return the paths list.

=cut
sub get_logical_paths {

  my $self = shift;
  return $self->{logical_paths};
}


=item recursive_partitioner

Define the partitioner. Recursively walk the graph.

=cut
sub recursive_partitioner {

  my $self = shift;

  my $graph = shift;
  my $node_id = shift; # a real abstract node (not a trivial made one_
  #my $node = shift; # a real abstract node (not a trivial made one_
  #my $relation = shift;
  my $level = shift;

  ###
  ### ...
  ###

  ## Have we seen it before or is it new?
  if( ! defined $self->{vertex_set}{$node_id} ){

    $self->{LOG}->debug("Saw (new) " . $node_id . " at level " . $level . "!");

    ## Create new vertex and add to set.
    my $new_vertex = $trivial_graph->make_simple_vertex($node_id);
    $new_vertex->{level} = $level;
    $self->{vertex_set}{$node_id} = $new_vertex;

    ## Check the node in to the 'seen' references.
    $self->{first_seen_reference}{$node_id} = $level;
    $self->{last_seen_reference}{$node_id} = $level;

  }else{

    ## No worries, these were already defined in the if part...
    if( $self->{first_seen_reference}{$node_id} > $level ){
      $self->{first_seen_reference}{$node_id} = $level;
    }
    if( $self->{last_seen_reference}{$node_id} < $level ){
      $self->{last_seen_reference}{$node_id} = $level;
    }
    $self->{vertex_set}{$node_id}{level} =
      $self->{last_seen_reference}{$node_id};

    $self->{LOG}->debug("Saw (old) " . $node_id . " at level " . $level . "!");
    $self->{LOG}->debug("\t" . $self->{first_seen_reference}{$node_id} . ' ' .
			$self->{last_seen_reference}{$node_id} . ' ' .
			$self->{vertex_set}{$node_id}{level} );
  }

  ###
  ### ...
  ###

  ## Find all the child nodes and go down...
  #my $child_nodes = graph.getExtantChildren(node.id(), relation);
  my @child_node_ids = $graph->predecessors($node_id);
  ## TODO: Better way?
  ##var child_nodes = graph.getChildren(node.id(), relation);
  foreach my $child_id (@child_node_ids){
    ## Add edge and descend.
    my $new_edge = $trivial_graph->make_simple_edge($child_id,$node_id,'is_a');
    $self->{edge_set}{$new_edge->{id}} = $new_edge;
    $self->recursive_partitioner($graph, $child_id, $level +1);
  }
}


=item partition

Define the partitioner. Recursively walk the graph.

TODO/BUG?: In the future, I'll want to re-add the graph-over-relation
features that were available in the JavaScript version.

=cut
sub partition {

  my $self = shift;
  my $graph = shift;
  #my $relation_id = shift || '';

  $self->{LOG}->debug("Start partitioning with graph: " . $graph);

  ###
  ### Run the partitioner.
  ###
  #my $roots = graph.getRootNodes(rel);

  ## First get the roots, which should look remarkably like the union
  ## of sink and isolated nodes.
  my @sink_roots = $graph->sink_vertices();
  my @isolated_roots = $graph->isolated_vertices();
  my @roots = (@sink_roots, @isolated_roots);
  $self->{LOG}->debug("Found " . scalar(@roots) . " root(s)");
  foreach my $root (@roots){
    $self->{LOG}->debug("Descend on root: " . $root);
    # #$self->{last_seen_reference}{$root->id()} = 0;
    #$self->{last_seen_reference}{$root} = 0; # TODO: is this necessary?
    # #recursive_partitioner($graph, $root, 'is_a', 0);
    $self->recursive_partitioner($graph, $root, 0);
  }

  ## Now we have a listing of the first and last level that a node
  ## appears at. We'll go through and make a proper ordering. We know
  ## that the last seen reference is where the actual node will
  ## appear. If there is a difference with the listing in the first
  ## node reference, the difference will be made in virtual nodes.
  my $v_id = 0;
  foreach my $key (keys %{$self->{edge_set}} ){
    my $edge = $self->{edge_set}{$key};

    ## Get difference count.
    my $tvert_sub = $self->{vertex_set}{$edge->{subject_id}};
    my $tvert_obj = $self->{vertex_set}{$edge->{object_id}};
    my $difference = $tvert_sub->{level} - $tvert_obj->{level};
    #my $difference = $self->{last_seen_reference}{$edge->{subject_id}}
    #  - $self->{last_seen_reference}{$edge->{object_id}};

    $self->{LOG}->debug("Looking at: " . $tvert_sub->{id} . ' and ' .
			$tvert_obj->{id} . ' w/difference ' . $difference);

    ###
    ### If there is a difference, create virtual nodes and
    ### paths. Deleted used edges.
    ###

    my $new_path = [];
    if( $difference > 1 ){

      $self->{LOG}->debug("VNodes needed: " . ($difference -1));

      ## Create a new chain of virtual nodes.
      my $current_subject_id = $edge->{object_id};
      my $current_object_id = undef;
      my $edge_object = $self->{vertex_set}{$edge->{object_id}};
      my $current_level = $edge_object->{level};
      push @$new_path, $edge->{object_id}; ## TODO: check this bit against JS
      for( my $i = 1; $i <= $difference; $i++ ){

	$current_object_id = $current_subject_id;
	$current_level++;

	if( $i != $difference ){
	  ## Make a virtual node.
	  my $v_node_id = '_VN_' . $v_id . '_';
	  $v_id++;
	  my $new_v_node = $trivial_graph->make_simple_vertex($v_node_id, 1);
	  $new_v_node->{level} = $current_level;
	  $self->{vertex_set}{$new_v_node->{id}} = $new_v_node;
	  $current_subject_id = $new_v_node->{id};
	  push @$new_path, $new_v_node->{id};

	  $self->{LOG}->debug("New vnode id: " . $v_node_id);

	}else{
	  ## Last link and path step.
	  $current_subject_id = $edge->{subject_id};
	  push @$new_path, $edge->{subject_id};
	}

	## Make edge to virtual node.
	my $new_edge = $trivial_graph->make_simple_edge($current_subject_id,
							$current_object_id,
							'is_a',
							1);
	$self->{edge_set}{$new_edge->{id}} = $new_edge;

	$self->{LOG}->debug("New vedge between " . $current_subject_id .
			    " and: " . $current_object_id);
      }

      ## Since the node generator goes in reverse order.
      @$new_path = reverse(@$new_path);

      ## Finally, delete the edge connecting these two--no longer needed.
      delete( $self->{edge_set}{$key} );

    }else{
      ## Add the trival path.
      $self->{LOG}->debug("VNodes not needed: " .
			  $edge->{subject_id} . ', '. $edge->{object_id});
      push @$new_path, $edge->{subject_id};
      push @$new_path, $edge->{object_id};
    }
    ## Add our new path to the group.
    push @{$self->{logical_paths}}, $new_path;
  }

  ## Sort the vertices into different partitions and count them.
  foreach my $key (keys %{$self->{vertex_set}}){

    my $vert = $self->{vertex_set}{$key};
    my $lvl = $vert->{level};

    $self->{LOG}->debug("Check: have vertex ". $key . " at level " . $lvl);

    if( ! $self->{vertex_partition_set}{$lvl} ){
      $self->{vertex_partition_set}{$lvl} = [];
      #$self->{number_of_partitions}++; # count the number of partitions
    }
    push @{$self->{vertex_partition_set}{$lvl}}, $vert;
    ## Count max width.
    if( scalar(@{$self->{vertex_partition_set}{$lvl}}) >
	$self->{maximum_partition_width} ){
      $self->{maximum_partition_width} =
	scalar(@{$self->{vertex_partition_set}{$lvl}});
    }
    #$self->{LOG}->debug("Current max width: ".$self->{maximum_partition_width});
  }

  ## Sort the edges into different partitions. Made easier since the
  ## vertices have already been sorted.
  foreach my $key (keys %{$self->{edge_set}}){

    my $edge = $self->{edge_set}{$key};
    my $edge_object = $self->{vertex_set}{$edge->{object_id}};
    my $lvl = $edge_object->{level};

    $self->{LOG}->debug("Check: have edge ". $key . " at level " . $lvl);

    if( ! defined $self->{edge_partition_set}{$lvl} ){
      $self->{edge_partition_set}{$lvl} = [];
    }
    push @{$self->{edge_partition_set}{$lvl}}, $edge;
  }
}



=item pdump

Dump the contents of the partitioner. Take a look around, It'll be
fun!

=cut
sub pdump {

  my $self = shift;

  #print STDERR "Dumping partitions...\n";
  if( scalar(@{$self->{vertex_partition_set}{0}}) ){
    my @first_set = map { $_->{id} } @{$self->{vertex_partition_set}{0}};
    #print STDERR join " ", @first_set;
    #print STDERR "\n";

    ##
    for( my $key = 1; $key < scalar(keys %{$self->{vertex_partition_set}}); $key++ ){
      my @current_set =
	map { '(' . $_->{subject_id} . ', ' . $_->{object_id} . ')' }
	  @{$self->{edge_partition_set}{($key -1)}};
      #print STDERR join " ", @current_set;
      #print STDERR "\n";
      @current_set = map { $_->{id} } @{$self->{vertex_partition_set}{$key}};
      #print STDERR join " ", @current_set;
      #print STDERR "\n";
    }
  }else{
    #print STDERR "...looks empty.\n";
  }
}



1;
