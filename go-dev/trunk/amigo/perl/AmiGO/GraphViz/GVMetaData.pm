=head1 AmiGO::GraphViz::GVMetaData

A lower-level handle for GraphViz (extract layout information and
image).

=cut

package AmiGO::GraphViz::GVMetaData;

use base 'AmiGO::GraphViz';

#use Data::UUID;
#use AmiGO::GraphViz;
use Data::Dumper;
#use Data::Types qw(:all);


=item new


=cut
sub new {

  ##
  my $class = shift;
  my $arg_hash = shift || {};
  my $self = $class->SUPER::new($arg_hash);

  $self->{GVR_GRAPH} = undef;
  $self->{GVR_NODES} = undef;
  $self->{GVR_EDGES} = undef;

  #my $session = shift || undef;
  #my $bitmap = $arg_hash->{bitmap} || 0;

  ## Figure out the unique identifier for this map by the leaves. Sort
  ## and cat.
  #my $uid = join('_', sort(keys(%$leaves)));
  ## Or not...
#   my $ug = Data::UUID->new();
#   my $uid = $ug->to_string( $ug->create() );
#   my $graph_map_name = 'graph_map.' . $uid . '.png';
#   my $graph_map_url = $self->amigo_env('AMIGO_TEMP_IMAGE_URL') .
#     '/' . $graph_map_name;

  bless $self, $class;
  return $self;
}


## Convert the text description of a the GraphViz graph into a data
## structure that contains pixel and lat/lon information for all
## interesting objects.
sub generate_projection_data {

  my $self = shift;
  my $png_pixel_width = shift || die "need height argument: $!";
  my $png_pixel_height = shift || die "need height argument: $!";

#   my $png_image_location = shift || die "Image location is required: $!";

#   ## Get the real physical dimensions from the PNG (GraphViz has been
#   ## known to lie: see the differences between PNG and SVG images).
#   ## TODO
#   my ($png_pixel_width, $png_pixel_height) = imgsize($png_image_location);
#   $self->kvetch('graph size: ' . $png_pixel_width . ', ' . $png_pixel_height);

  #$self->kvetch($self->{GV_INTERNAL_TEXT});

  ## Extract GV dimensions and locations.
  my $gv_edges = {};
  my $gv_nodes = {};
  my $gv_graph = {};
  my @lines = split /\n+/, $self->{GV_INTERNAL_TEXT};
  my $edge_id = 0;
  foreach my $l (@lines){
    if( $l =~ /^graph/){

      my @pieces = split /\s+/, $l;
      $self->kvetch('graph like: ' . $l);
      $gv_graph->{width} = $pieces[2] + 0.0;
      $gv_graph->{height} = $pieces[3] + 0.0;

    }elsif( $l =~ /^node/){

      $self->kvetch('node like: ' . $l);
      my @pieces = split /\s+/, $l;

      ## Hacky, but pry the node id out of number 9...
      my $rough = $pieces[9];
      $rough =~ /\<TD\>(.*)\<br\/\>/;
      $self->kvetch("\tacc like: " . $1);

      ##
      my $gv_id = $pieces[1];
      $gv_nodes->{$gv_id} = {
			     acc => $1,
			     x => $pieces[2] + 0.0,
			     y => $pieces[3] + 0.0,
			     w => $pieces[4] + 0.0,
			     h => $pieces[5] + 0.0,
			    };

    }elsif( $l =~ /^edge/){

      $self->kvetch('edge like: ' . $l);
      my @pieces = split /\s+/, $l;

      ## Rip out the basic information.
      my $rel =  $pieces[-6] . '_' . $pieces[-5];
      $rel =~ s/\"//g;
      $gv_edges->{$edge_id} =
	{
	 start => $pieces[1],
	 end => $pieces[2],
	 chain_length => $pieces[3] + 0,
	 points => [],
	 relation => $rel,
	};

      ## Rip out the waypoint chain.
      foreach my $i (0..$gv_edges->{$edge_id}{chain_length} -1){
	my $cindex = ($i * 2) + 4;
	push @{$gv_edges->{$edge_id}{points}},
	  {
	   x => $pieces[$cindex] + 0.0,
	   y => $pieces[$cindex + 1] + 0.0,
	  };
      }

      $edge_id++;

    }else{
      ## Noise.
    }
  }

  ## Now that we have full information, figure out the conversions.
  my $x_conv_k = $png_pixel_width / $gv_graph->{width};
  my $y_conv_k = $png_pixel_height / $gv_graph->{height};
  my $height_index = $png_pixel_height;
  $self->kvetch('new graph width conv: ' . $x_conv_k );
  $self->kvetch('new graph height conv: ' . $y_conv_k );
  $self->kvetch('new graph width: ' . $x_conv_k * $gv_graph->{width} );
  $self->kvetch('new graph height: ' . $y_conv_k * $gv_graph->{height} );
  $self->kvetch('new graph height index: ' . $height_index );

  ## Pixel location conversion utilites.
  my $_xconv = sub {
    my $in = shift || 1;
    return $x_conv_k * $in;
  };
  my $_yconv = sub {
    my $in = shift || 1;
    return $y_conv_k * $in;
  };
  my $_hconv = sub {
    my $in = shift || 1;
    return $height_index - ($y_conv_k * $in);
  };

  ###
  ### Count order to make sure we always fit into the same "flat"
  ### projection.
  ###

  #my $BASE_SCALE = 200;
  #my $BASE_SCALE = 1;

  ## Calculate zoom scaling for our garden--we always want to fit in
  ## under 1.0, -1.0 so that we get minimal distortion.
  #my $RENDER_WORLD_X = $png_pixel_width * $BASE_SCALE;
  #my $RENDER_WORLD_Y = $png_pixel_height * $BASE_SCALE;
  my $RENDER_WORLD_X = $png_pixel_width;
  my $RENDER_WORLD_Y = $png_pixel_height;
  my $x_order = length(sprintf("%u",$RENDER_WORLD_X));
  my $y_order = length(sprintf("%u",$RENDER_WORLD_Y));
  my $order = $x_order;
  $order = $y_order if $y_order > $x_order;
  #$order = $order + 1;

  ##
  my $x_scale = 1 / (10 ** $order);
  my $y_scale = -1 / (10 ** $order);
  ##my $lat_conv_k = $RENDER_WORLD_Y * $y_scale;
  ##my $lon_conv_k = $RENDER_WORLD_X * $x_scale;
  #my $lat_conv_k = $BASE_SCALE * $x_scale;
  #my $lon_conv_k = $BASE_SCALE * $y_scale;
  my $lon_conv_k = $x_scale;
  my $lat_conv_k = $y_scale;

  ## Lat/lon location conversion utilites.
  my $_lonconv = sub {
    my $in = shift || 1;
    return $lon_conv_k * $in;
  };
  my $_latconv = sub {
    my $in = shift || 1;
    return $lat_conv_k * $in;
  };

  ###
  ### Now redo the objects with acc instead of nodes and the properly
  ### dimensioned measurments into pixels (TODO: lat/lon conversion).
  ###

  ## Convert nodes to pixels and acc.
  my $pixel_nodes = {};
  my $pixel_node_max_width = 0.0;
  foreach $node_id (keys %$gv_nodes){

    my $acc = $gv_nodes->{$node_id}{acc};
    $pixel_nodes->{$acc} =
      {
       lon => &$_lonconv( &$_xconv( $gv_nodes->{$node_id}{x}) ),
       lat => &$_latconv( &$_hconv( $gv_nodes->{$node_id}{y}) ),
       x => &$_xconv( $gv_nodes->{$node_id}{x}),
       y => &$_hconv( $gv_nodes->{$node_id}{y}),
       w => &$_xconv( $gv_nodes->{$node_id}{w}),
       h => &$_yconv( $gv_nodes->{$node_id}{h}),
      };

    ## Let's get the widest node for halo sizing.
    $pixel_nodes_max_width = $pixel_nodes->{$acc}{w}
      if $pixel_nodes->{$acc}{w} > $pixel_nodes_max_width;
  }

  ##
  my $pixel_graph =
    {
     lon => &$_lonconv($png_pixel_width),
     lat => &$_latconv($png_pixel_height),
     x => &$_xconv($gv_graph->{width}),
     y => &$_yconv($gv_graph->{height}),
     scale => &$_lonconv($pixel_nodes_max_width),
    };


  ## Convert edges to acc and pixel.
  my $pixel_edges = {};
  foreach my $edge_id (keys %$gv_edges){

    #$self->kvetch('_in_' . $edge_id);

    ## Extract old general information.
    my $edge = $gv_edges->{$edge_id};
    my $start_acc = $gv_nodes->{$edge->{start}}{acc};
    my $end_acc = $gv_nodes->{$edge->{end}}{acc};

    ## Generate new general information
    my $edge_acc = $end_acc . '_' . $edge->{relation} . '_' . $start_acc;
    #$self->kvetch('_on_' . $edge_acc);
    $pixel_edges->{$edge_acc} =
      {
       subject_acc => $end_acc,
       object_acc => $start_acc,
       relation => $edge->{relation},
       number_of_waypoints => $edge->{chain_length},
       waypoints => [],
      };

    ## Generate new waypoint information from old. From subject to
    ## object.
    foreach my $p (@{$gv_edges->{$edge_id}{points}}){
      unshift @{$pixel_edges->{$edge_acc}{waypoints}},
	{
	 lon => &$_lonconv( &$_xconv($p->{x}) ),
	 lat => &$_latconv( &$_hconv($p->{y}) ),
	 x => &$_xconv($p->{x}),
	 y => &$_hconv($p->{y}),
	};
    }
    #$self->kvetch('_done_' . Dumper($pixel_edges->{$edge_acc}));
  }

  ## DEBUG
  #$self->kvetch('Graph:');
  #$self->kvetch(Dumper($pixel_graph));
  #$self->kvetch($pixel_graph->{x});
  #$self->kvetch('Nodes:');
  #$self->kvetch(Dumper($pixel_nodes));
  #$self->kvetch('Edges:');
  #$self->kvetch(Dumper($pixel_edges));

  #die;

  $self->{GVR_GRAPH} = $pixel_graph;
  $self->{GVR_NODES} = $pixel_nodes;
  $self->{GVR_EDGES} = $pixel_edges;
}

##
sub get_world {
  my $self = shift;
  return $self->{GVR_GRAPH};
}

##
sub get_world_nodes {
  my $self = shift;
  return $self->{GVR_NODES};
}

##
sub get_world_edges {
  my $self = shift;
  return $self->{GVR_EDGES};
}



1;
