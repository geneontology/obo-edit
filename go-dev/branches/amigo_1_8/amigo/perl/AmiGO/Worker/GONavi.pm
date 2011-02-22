=head1 AmiGO::Worker::GONavi

...

=cut

package AmiGO::Worker::GONavi;

use base ("AmiGO::JavaScript");

use GOBO::DBIC::GODBModel::Graph;
use GD;
use Graph::Directed;
use IO::Scalar;
use Image::Size;


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();
  my $args = shift || {};

  bless $self, $class;
  return $self;
}


=item generate

...

=cut
sub generate {

  my $self = shift;
  my $input_term_list = shift || [];

  my $retstruct = {};

  my $graph = GOBO::DBIC::GODBModel::Graph->new();
  # $graph->verbose(1); # Aux verbose needed this graph isn't AmiGO
  my $g = Graph::Directed->new();
  my $gvrip = AmiGO::GraphViz::GVMetaData->new({
						bitmap => 1,
						fontsize => 16, # more legible
					       });

  ## Something should be in the graph.
  if( scalar(@$input_term_list) == 0 ){
    #$self->kvetch("using roots as seeds...");
    @$input_term_list = keys %{$graph->get_roots()};
  }

  ## Convert input terms to AmiGO terms.
  my $terms = [];
  foreach my $acc (@$input_term_list){
    $self->kvetch("AmiGO::Worker::GONavi::generate: graph seed: " . $acc);
    my $term = $graph->get_term($acc);
    if( defined $term ){
      push @$terms, $term;
    }
  }

  #$self->kvetch("AmiGO::Worker::GONavi::generate: pre climb");

  ## Convert the terms to nodes and edges.
  my($nodes, $edges) = $graph->climb($terms);

  #$self->kvetch("AmiGO::Worker::GONavi::generate: post climb");

  ## Add edges to graph for layout and edge information.
  foreach my $key (keys %$edges){
    my $t2t = $edges->{$key};
    my $s = $t2t->subject->acc;
    my $o = $t2t->object->acc;
    my $r = $t2t->relationship->name;
    $gvrip->add_edge($s, $r, $o);
    #     $monkey->add_edge_information($s, $o,
    # 				  $aid->readable($r),
    # 				  $aid->relationship_color($r));
    $g->add_edge($s, $o);
  }

  ## Add nodes to graph for layout and node information.  While we're
  ## at, collect the leaves for the next step.
  my $leaves = {};
  my $child_info = {};
  foreach my $acc (keys %$nodes){
    my $term = $graph->get_term($acc);
    # $monkey->add_node_information($acc, $acc . "\n" . $term->name);
    $gvrip->add_node($acc, $acc, $term->name,
		     {
		      color => '', # $border,
		      fillcolor => '', # $fill,
		      fontcolor => '', # $font,
		     });

    $g->add_vertex($acc);

    ## Collect leaves.
    if( $g->in_degree($acc) == 0 ){
      $leaves->{$acc} = 1;
    }

    ## Get basic neighborhood info--children.
    $self->kvetch("Kids for: " . $acc);
    my @child_cache = ();
    my $children = $graph->get_children($acc);
    foreach my $kid (@$children){
      push @child_cache, {acc => $kid->acc, name => $kid->name};
    }
    ## Order children.
    my @foo = sort {
      return $a->{name} cmp $b->{name};
    } @child_cache;
    $child_info->{$acc} = \@foo;
  }

  ## Get main representations.
  my $png_data = $gvrip->get_png();
  my $mini_png_data = '';
  my $plain_data = $gvrip->get_plain(); # TODO: Why did I get this again...?

  ## Soldier on and get the projection data for further calculations.
  my ($png_pixel_width, $png_pixel_height) = imgsize(\$png_data);
  $self->kvetch('graph size: ' . $png_pixel_width . ', ' . $png_pixel_height);
  $gvrip->generate_projection_data($png_pixel_width, $png_pixel_height);

  ## Transform standard file data into to mini data.
  my $src_img = GD::Image->newFromPngData($png_data)
    || die "image impossible: $!";
  my $small_x = int($gvrip->get_world()->{x} / 32.0);
  my $small_y = int($gvrip->get_world()->{'y'} / 32.0); # y oh y
  my $dest_img = GD::Image->new($small_x, $small_y)
    || die "mini image impossible: $!";
  $dest_img->copyResampled($src_img, 0, 0, 0, 0, $small_x, $small_y,
			   $png_pixel_width, $png_pixel_height);
  $mini_png_data = $dest_img->png();

  ##
  $retstruct->{map_world_information} =
    {
     x => $gvrip->get_world()->{x},
     y => $gvrip->get_world()->{'y'}, # y oh y
     lon => $gvrip->get_world()->{lon},
     lat => $gvrip->get_world()->{lat},
     scale => $gvrip->get_world()->{scale},
    };
  $retstruct->{map_term_information} = $gvrip->get_world_nodes();
  $retstruct->{map_edge_information} = $gvrip->get_world_edges();

  ## Pick basic GO term information.
  $retstruct->{go_term_information} =
    { map{ $_ => { name => $nodes->{$_}->name(),
		   children => $child_info->{$_} } } keys %$nodes };

  return
    {
     image => $png_data,
     mini_image => $mini_png_data,
     meta_data => $retstruct,
    };
}



1;
