=head1 SuGR::Render

Render testing for now.

The idea to to take a layout object (in this case, a degenerate
Sugiyama for now) and turn it into a rendered graphic.

Part of this job will be to convert the discrete matrix locations to a
(very small) projected world map.

TODO: this should probably just emit a JS API file.

=cut

package SuGR::Render;

use strict;
use utf8;
use base 'SuGR';
use Data::Dumper;

#use Log::Log4perl;
use Cairo;
use Text::WrapI18N;
use Graphics::ColorNames;


###
###
###

my $BASE_SCALE = 200;
my $LINE_WIDTH = 5;
my $BORDER_WIDTH = 5;
my $FONT_SIZE = 12;
my $WRAP_COLUMNS = 12;
my @BLACK = (0,0,0);

my $COLORS = Graphics::ColorNames->new('X');


=item new

Constr.

=cut
sub new {

  my $class = shift;
  my $self = {
	      LOG => Log::Log4perl->get_logger('SuGR::Render'),

	      ##
	      IN_NODES => {},
	      IN_EDGES => {},

	      ## Drawing objects (generated in render).
	      SURFACE => undef,
	      CAIRO => undef,

	      ## Keepers for things that we'll need to translate to ll
	      ## when we're done drawing.
	      RENDER_WORLD_X => 99999,
	      RENDER_WORLD_Y => 99999,
	      RENDER_WORLD_LON => 99999.99999,
	      RENDER_WORLD_LAT => 99999.99999,
	      RENDER_NODES => {}, # non-virtual nodes
	      ALL_NODES => {}, # all nodes
	      RENDER_PATHS => {},

	     };

  bless $self, $class;
  return $self;
}


=item add_node_information

...

=cut
sub add_node_information {

  my $self = shift;
  my $id = shift || '';
  my $body = shift || '';

  $self->{IN_NODES}{$id} = {
			    body => $body,
			   };
}


=item add_edge_information

...

=cut
sub add_edge_information {

  my $self = shift;
  my $start_id = shift || '';
  my $end_id = shift || '';
  my $label = shift || '';
  my $color = shift || 'black';

  my $edge_id = $self->_make_edge_id($start_id, $end_id);
  $self->{IN_EDGES}{$edge_id} = {
				label => $label,
				color => $color,
			       };
}


=item render

...

=cut
sub render {

  my $self = shift;
  my $layout_object = shift;

  $self->{RENDER_WORLD_X} = $layout_object->{width} * $BASE_SCALE;
  $self->{RENDER_WORLD_Y} = $layout_object->{height} * $BASE_SCALE;

  my $w = $self->{RENDER_WORLD_X};
  my $h = $self->{RENDER_WORLD_Y};
  my $verts = $layout_object->{locations};
  my $paths = $layout_object->{paths};

  $self->{LOG}->debug('Create...');

  $self->{LOG}->debug('Width/height: ' . $w . ' x ' . $h);
  $self->{SURFACE} = Cairo::ImageSurface->create ('argb32', $w, $h);
  $self->{CAIRO} = Cairo::Context->create ($self->{SURFACE});

  ###
  ### Draw all of the vertices...
  ###

  $self->{LOG}->debug('Need to add ' . scalar(keys %$verts) . ' vertices.');
  $self->{CAIRO}->set_line_width($LINE_WIDTH);
  foreach my $vert (keys %$verts){

    my $v = $verts->{$vert};

    my $x = $v->{x};
    my $y = $v->{y};
    my $real_x = ($x * $BASE_SCALE) + ($BASE_SCALE / 2);
    my $real_y = ($y * $BASE_SCALE) + ($BASE_SCALE / 2);

    $self->{LOG}->debug('Vertex ' . $v->{id} . ' centered at: ('.
			$real_x .', '. $real_y .')');

    $self->{CAIRO}->rectangle($real_x - ($BASE_SCALE / 4),
			      $real_y - ($BASE_SCALE / 4),
			      ($BASE_SCALE / 2),
			      ($BASE_SCALE / 2));

    $self->{ALL_NODES}{$v->{id}} = { X => $real_x, Y => $real_y };

    ## Draw nodes.
    if( ! $v->{virtual_p} ){
      $self->{ALL_NODES}{$v->{id}}{VIRTUAL_P} = 0;
      $self->{CAIRO}->set_source_rgb(@BLACK);
    }else{
      $self->{ALL_NODES}{$v->{id}}{VIRTUAL_P} = 1;
      $self->{CAIRO}->set_source_rgb(1, 1, 1);
    }
    #$self->{CAIRO}->fill;
    $self->{CAIRO}->stroke;

    ## Write text for non-virtual nodes. This is where we use the node
    ## information objects. TODO: the information object could just be
    ## an HTML string that we work with...
    if( ! $v->{virtual_p} &&
	$self->{IN_NODES}{$v->{id}} ){

      my $info = $self->{IN_NODES}{$v->{id}};
      my $text = $info->{body};

      ## Figure out the best text offset for the node id.
      my $text_extents = $self->{CAIRO}->text_extents($text);
      #  #$self->{CAIRO}->set_font_face('bold');
      #  print STDERR "\n___" . $self->{CAIRO}->get_font_face() . "\n\n";
      #  print STDERR "\n___" . ref($self->{CAIRO}->get_font_face()) . "\n\n";
      $self->{CAIRO}->set_font_size($FONT_SIZE);
      my $font_extents = $self->{CAIRO}->font_extents();
      my $text_x = $real_x
	- ($BASE_SCALE / 4)
	  - $text_extents->{x_bearing}
	    + $font_extents->{height};
      my $text_y = $real_y
	- ($BASE_SCALE / 4)
	  - $text_extents->{y_bearing}
	    + $font_extents->{height};

      ## Draw the text.
      $self->{LOG}->debug('Need to add text: ' . $text .
			  ' at: (' . $text_x . ',' . $text_y . ')');
      ## Wrap the text and draw it using some of the calced data
      ## above.
      $Text::WrapI18N::columns = $WRAP_COLUMNS;
      #$Text::WrapI18N::separator = "|";
      my $wrapped_text = Text::WrapI18N::wrap('', '', $text);
      my @wrapped_lines = split(/\s+/, $wrapped_text);
      my $line_index = 0;
      foreach my $line (@wrapped_lines){

	my $adjusted_y = $text_y +
	  ($font_extents->{height} * $line_index);

	$self->{LOG}->debug('Need to add body text: '. $line .
			    ' at: ('. $text_x .','. $adjusted_y .')');
	$self->{CAIRO}->move_to($text_x, $adjusted_y);
	$self->{CAIRO}->set_source_rgb(@BLACK);
	$self->{CAIRO}->show_text($line);

	$line_index++;
      }
    }
  }

  ###
  ### Put out all of the edges.
  ### TODO: pretty much everything to make it look decent.
  ###

  $self->{LOG}->debug(Dumper($self->{IN_EDGES}));
  $self->{LOG}->debug('Need to add ' . scalar(@$paths) . ' paths.');
  foreach my $path_node_set (@$paths){

    my $unique_path_key = join '', @$path_node_set;
    $self->{RENDER_PATHS}{$unique_path_key} = [];
    $self->{LOG}->debug('Path ('. $unique_path_key . ')');

    ## TODO: identify the path and grab the meta-data for color and
    ## label.
    my $default_label_color = 'black';
    my $edge_id = $self->_make_edge_id($$path_node_set[0],
				       $$path_node_set[-1]);
    my $edge_info = $self->{IN_EDGES}{$edge_id};
    my $edge_label = $edge_info->{label};
    my $edge_color = $edge_info->{color};
    my @edge_rgb = (@BLACK);
    if( $edge_color ){
      my @int_colors = $COLORS->rgb($edge_color);
      @edge_rgb = $self->_rgb_ints_to_floats(@int_colors);
    }

    $self->{LOG}->debug('Path looks like: ' . $edge_id . ' ' . $edge_color);

    ## Decompose a single path and draw it.
    for( my $i = 0; $i < scalar(@$path_node_set); $i++ ){

      my $node_id = $$path_node_set[$i];

      my $real_x = $self->{ALL_NODES}{$node_id}{X};
      my $real_y = $self->{ALL_NODES}{$node_id}{Y};
      push @{$self->{RENDER_PATHS}{$unique_path_key}},
	{
	 X => $real_x,
	 Y => $real_y,
	};
      $self->{LOG}->debug("\tpoint at: " . $real_x . ' ' . $real_y .
			  ' ' . $unique_path_key);

      ## If we are on the first or last step, consider the halo around
      ## the node.
      my $bs = $BASE_SCALE / 4;
      $self->{CAIRO}->set_line_width($BORDER_WIDTH);

      ## i == 0 means that we're just moving to the end of the path,
      ## drawing is done in the else.
      if( $i == 0 ){
	$self->{CAIRO}->move_to($real_x, $real_y - $bs);
      }else{

	my $prev_node_id = $$path_node_set[$i-1];
	my $real_prev_x = $self->{ALL_NODES}{$prev_node_id}{X};
	my $real_prev_y = $self->{ALL_NODES}{$prev_node_id}{Y};
	if( $i == 1 ){
	  $real_prev_y -= $bs;
	}

	my $real_mid_x = ($real_x + $real_prev_x) / 2.0;
	if( $i+1 == scalar(@$path_node_set) ){
	  #$self->{CAIRO}->line_to($real_x, $real_y + $bs);

	  my $real_mid_y = ($real_y + $bs + $real_prev_y)/2.0;
	  #	  my $real_mid2_y = ($real_y + $bs + $real_prev_y)/2.0;
	  $self->{CAIRO}->curve_to($real_prev_x, $real_mid_y,
				   $real_x, $real_mid_y,
				   $real_x, $real_y + $bs);

	  $self->_write_label_at($edge_label, $default_label_color,
				 $real_mid_x, $real_mid_y);
	}else{
	  #$self->{CAIRO}->line_to($real_x, $real_y);

	  my $real_mid_y = ($real_y + $real_prev_y) / 2.0;
	  $self->{CAIRO}->curve_to($real_prev_x, $real_mid_y,
				   $real_x, $real_mid_y,
				   $real_x, $real_y);

	  $self->_write_label_at($edge_label, $default_label_color,
				 $real_mid_x, $real_mid_y);
	}
      }
    }
    ## TODO: color string to rgb...
    #$self->{CAIRO}->set_source_rgb(0, 0, 1);
    $self->{CAIRO}->set_source_rgb(@edge_rgb);
    $self->{CAIRO}->stroke;
  }
}


=item to_png

...

=cut
sub to_png {

  my $self = shift;
  my $location = shift;

  $self->{LOG}->debug('Write out...');
  $self->{CAIRO}->show_page;
  $self->{SURFACE}->write_to_png($location);
}


=item to_world

TODO: Convert the interesting points in the rendered image into
projected world coordinates.

TODO: turn into helper function and put with actual renderers.

=cut
sub to_world {

  my $self = shift;

  my $x_order = length(sprintf("%u",$self->{RENDER_WORLD_X}));
  my $y_order = length(sprintf("%u",$self->{RENDER_WORLD_Y}));
  my $order = $x_order;
  $order = $y_order if $y_order < $x_order;
  #my $x_scale = 1 / (10 ** $x_order);
  #my $y_scale = -1 / (10 ** $y_order);
  my $x_scale = 1 / (10 ** $order);
  my $y_scale = -1 / (10 ** $order);

  ## How big is a box? Don't worry, $x_scale is calced off of
  ## $BASE_SCALE.
  $self->{RENDER_WORLD_SCALE} = $x_scale * $BASE_SCALE;

  ## Changes the coord system, change the world.
  $self->{RENDER_WORLD_LON} = $x_scale * $self->{RENDER_WORLD_X};
  $self->{RENDER_WORLD_LAT} = $y_scale * $self->{RENDER_WORLD_Y};

  ## Run over the nodes and give them LL as well.
  foreach my $node_id (keys %{$self->{ALL_NODES}}){
    $self->{ALL_NODES}{$node_id}{LON} =
      $x_scale * $self->{ALL_NODES}{$node_id}{X};
    $self->{ALL_NODES}{$node_id}{LAT} =
      $y_scale * $self->{ALL_NODES}{$node_id}{Y};
  }

  ## DEBUG/TODO: This is here just for current debugging purposes.
  return {
	  ## Extents of the graph's world.
	  world => {
		    X => $self->{RENDER_WORLD_X},
		    Y => $self->{RENDER_WORLD_Y},
		    LON => $self->{RENDER_WORLD_LON},
		    LAT => $self->{RENDER_WORLD_LAT},
		    SCALE => $self->{RENDER_WORLD_SCALE},
		   },
	  nodes=> $self->{ALL_NODES}, # the center location of each node
	  paths=>{}, # ll paths between heir-connected nodes
	 };
}


=item world_xy

Return the world's x/y bounds from (0,0).

=cut
sub world_xy {
  my $self = shift;
  return { X => $self->{RENDER_WORLD_X}, Y => $self->{RENDER_WORLD_Y} };
}


=item world_ll

Return the world's long/lat bounds from (0,0).

=cut
sub world_ll {
  my $self = shift;
  return { LON=>$self->{RENDER_WORLD_LON}, LAT=>$self->{RENDER_WORLD_LAT} };
}


=item world_nodes

Return the world's nodes.

=cut
sub world_nodes {
  my $self = shift;

  my $render_nodes = {};
  foreach my $key (keys %{$self->{ALL_NODES}}){
    my $node = $self->{ALL_NODES}{$key};
    if( ! $node->{VIRTUAL_P} ){
      $render_nodes->{$key} =
	{
	 X => $node->{X},
	 Y => $node->{Y},
	 LON => $node->{LON},
	 LAT => $node->{LAT},
	};
    }
  }

  return $render_nodes;
}


=item world_scale

Return the world's general scale ().

=cut
sub world_scale {
  my $self = shift;
  return $self->{RENDER_WORLD_SCALE};
}


## TODO: Draw an edge label.
sub _write_label_at {

  my $self = shift;
  my $label = shift || '';
  my $color = shift || '';
  my $x = shift || 0.0;
  my $y = shift || 0.0;

  ## Fiddle with the colors.
  my @rgb = (0, 0, 0); # default to black
  if( $color ){
    my @int_colors = $COLORS->rgb($color);
    @rgb = $self->_rgb_ints_to_floats(@int_colors);
  }

  ## Save our current state.
  $self->{CAIRO}->save();
  my($curr_x, $curr_y) = $self->{CAIRO}->get_current_point();
  #my($r,$g,$b,$a) = $self->{CAIRO}->get_source_rgb();

  ## Draw out label.
  $self->{CAIRO}->move_to($x + $LINE_WIDTH, $y);
  $self->{CAIRO}->set_source_rgb(@rgb);
  $self->{CAIRO}->show_text($label);

  ## Undo our evil.
  $self->{CAIRO}->restore();
  #$self->{CAIRO}->set_source_rgb($r, $g, $b);
  $self->{CAIRO}->move_to($curr_x, $curr_y);
}


## Naughty generator.
sub _make_edge_id {

  my $self = shift;
  my $token_a = shift || '';
  my $token_b = shift || '';
  my $binder = '_^_';

  return $token_a . $binder . $token_b;
}


## rgb ints floats
sub _rgb_ints_to_floats {

  my $self = shift;
  my @ints = @_;
  return map { $_/255 } @ints;
}



1;
