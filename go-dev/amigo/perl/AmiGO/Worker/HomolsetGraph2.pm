=head1 AmiGO::Worker::HomolsetGraph2

Defines a few constants and helpers for the sub classes. Should the
actual live database stuff (as opposed to statics from installation
time) be from here on down?

In the end, these SVGs should have more or less the following structure:

<svg onload="init()">
 <!-- JS definitions. -->
 <script>// Lotsa this.</script>
 <defs>
  <!-- often used primitives -->
 </defs>
 <g id="graph_context"> <!-- contains zoomer/etc. -->
  <g id="underlay" />
  <g id="graph" />
  <g id="overlay" />
 </g>
 <g id="control_context" />
</svg>

=cut

use utf8;
#use diagnostics;
use strict;

package AmiGO::Worker::HomolsetGraph2;

use base ("AmiGO::Worker");

#use GOBO::DBIC::GODBModel::Schema;
use GOBO::DBIC::GODBModel::Graph;
use AmiGO::GraphViz;
#use AmiGO::JavaScript;
use AmiGO::Worker::GPInformation::HomolsetInformation;
use AmiGO::Aid::ReferenceGenome;
use AmiGO::SVGRewrite;
use Time::HiRes qw(gettimeofday tv_interval);


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  ## We'll borrow SUCCESS and ERROR_MESSAGE from AmiGO.

  ## Start time.
  my $t = [gettimeofday];

  $self->{AID} = AmiGO::Aid::ReferenceGenome->new();
  $self->{INFO} =
    #AmiGO::Worker::GPInformation::HomolsetInformation->new({skip_roots=>0});
    AmiGO::Worker::GPInformation::HomolsetInformation->new({skip_roots=>1,
							    graph_roots=>1});

  ##
  $self->kvetch('_0_time: ' . tv_interval($t));

  $self->{AMIGO_SPECIES} = {};# This is going to be a complicated struct.
  $self->{AMIGO_TERMS} = {};  # This is going to be a complicated struct.

  ##
  $self->{AMIGO_ORDER_VALUE} = {};
  my $value = 0;
  foreach my $spec (@{$self->{AID}->species_list({safely=>0})}){
    $self->{AMIGO_ORDER_VALUE}{$spec} = $value;
    $self->kvetch("ORDER VAL: " . $spec . ' ' . $value);
    $value++;
  }

  bless $self, $class;
  return $self;
}


=item make_graph

Create an SVG graph for great honor.

=cut
sub make_graph {

  my $self = shift;
  my $set = shift || 1; ## BUG: silly failsafe.
  my $gv_out_type = shift || 'svg'; ## TODO/BUG: This should not be
                                    ## here in the final version, just
                                    ## now for graphviz testing.

  ## Init system.
  my $info = $self->{INFO};
  $info->calculate($set);

  ## Start time.
  my $t = [gettimeofday];

  ## Time.
  $self->kvetch('_1_time: ' . tv_interval($t));
  $t = [gettimeofday];

  ###
  ### Build display graph.
  ###

  ## Make PNG images large, otherwise SVG.
  my $gv = undef;
  if( $gv_out_type eq 'png' ){
    $gv = AmiGO::GraphViz->new({bitmap => 1});
  }else{
    $gv = AmiGO::GraphViz->new();
  }

  ## Add edges.
  my $edges = $info->get_edges();
  foreach my $key (keys %$edges){

    my $t2t = $edges->{$key};
    my $s = $t2t->subject->acc;
    my $o = $t2t->object->acc;
    my $r = $t2t->relationship->name;
    $gv->add_edge($s, $r, $o);
    $self->kvetch("_r_ Render edge ($key): " . $s . " $r " . $o);
    #$self->kvetch("^^^^^^^^^^^: not found")
    #  if ! defined($self->{AMIGO_TERMS}{$obj}) ||
    #	! defined($self->{AMIGO_TERMS}{$sub});
  }

  ## Add nodes.
  #foreach $key (keys %$nodes){
  my $nodes = $info->get_nodes();
  my $assocs = $info->get_matrix();
  foreach my $acc (keys %$assocs){
  #foreach my $acc (keys %$nodes){

    my $t = $nodes->{$acc};
    $self->kvetch("_r_ Render node: " . $t->acc);

    ## Declare the needed structure if we haven't seen this one
    ## before.
    if( ! $self->{AMIGO_TERMS}{$acc} ){
      $self->{AMIGO_TERMS}{$acc} = {
				    name => $t->name,
				    gene_products => {},
				   };
    }

    $self->kvetch("On term: " . $acc);

    ## Items that will be added under the term in the graph.
    my %stack_info = (); # {spec_name, spec_color, gp_sym }

    foreach my $ncbi_taxa_id (keys %{$assocs->{$acc}}){

      my $spec_name = $self->{AID}->taxid2readable({spec_id=>$ncbi_taxa_id});
      my $spec_color = $self->{AID}->taxid2color($ncbi_taxa_id);

      foreach my $a_id (keys %{$assocs->{$acc}{$ncbi_taxa_id}}){

	my $assoc_data = $assocs->{$acc}{$ncbi_taxa_id}{$a_id};
	#my $gp_acc = $assoc_data->{gene_product_id};
	my $gp_acc = $assoc_data->{gene_product_id} . $assoc_data->{direct_p};
	#my $gp_acc = $a_id;

	my $direct_p = $assoc_data->{direct_p};

	$self->kvetch("\tOn GP: " . $gp_acc . ' (' . $spec_color. ')');

	## Add to our seen terms and add all of the GPs with
	## additional info to it. This will be for the JS variable
	## amigo_terms.
	my $aterm_gp_data = $self->{AMIGO_TERMS}{$acc}{gene_products};
	if( ! defined $aterm_gp_data->{$gp_acc} ){

	  my $has_exp_p = 0;
	  my $has_good_iss_p = 0;
	  my $has_odd_iss_p = 0;
	  my $has_bad_iss_p = 0;
	  #if( $direct_p ){
	    $has_exp_p = $assoc_data->{has_exp_p};
	    $has_good_iss_p = $assoc_data->{has_good_iss_p};
	    $has_odd_iss_p = $assoc_data->{has_odd_iss_p};
	    $has_bad_iss_p = $assoc_data->{has_bad_iss_p};
	  #}

	  ##
	  $aterm_gp_data->{$gp_acc} =
	    {
	     acc => $gp_acc,
	     symbol => $assoc_data->{gene_product_symbol},
	     color => $spec_color,
	     species => $spec_name,
	     direct_p => $direct_p,
	     has_exp_p => $has_exp_p,
	     has_good_iss_p => $has_good_iss_p,
	     has_odd_iss_p => $has_odd_iss_p,
	     has_bad_iss_p => $has_bad_iss_p,
	     #evcode => {},
	    };
	}else{

	  ## We've seen this GP before, so just increment the
	  ## changing values.
	  #my $aterm_gp_data = $self->{AMIGO_TERMS}{$acc}{gene_products};
	  $aterm_gp_data->{$gp_acc}{direct_p}++
	    if $assoc_data->{direct_p};
	  #if( $direct_p ){
	    $aterm_gp_data->{$gp_acc}{has_exp_p}++
	      if $assoc_data->{has_exp_p};
	    $aterm_gp_data->{$gp_acc}{has_good_iss_p}++
	      if $assoc_data->{has_good_iss_p};
	    $aterm_gp_data->{$gp_acc}{has_odd_iss_p}++
	      if $assoc_data->{has_odd_iss_p};
	    $aterm_gp_data->{$gp_acc}{has_bad_iss_p}++
	      if $assoc_data->{has_bad_iss_p};
	  #}
	}

	## Information to add to the GV term box. We're progressively
	## adding has_* info...
	if( ! defined $stack_info{$gp_acc} ){

	  $self->kvetch("\t\toriginal direct: " . $assoc_data->{direct_p} .
			' exp ' . $assoc_data->{has_exp_p} );

	  $stack_info{$gp_acc} =
	    {
	     gp_symbol => $assoc_data->{gene_product_symbol},
	     color => $spec_color,
	     name => $spec_name,
	     direct_p => $assoc_data->{direct_p},
	     has_exp_p => $assoc_data->{has_exp_p},
	     has_good_iss_p => $assoc_data->{has_good_iss_p},
	     has_odd_iss_p => $assoc_data->{has_odd_iss_p},
	     #has_exp_p => $aterm_gp_data->{$gp_acc}{has_exp_p},
	     #has_good_iss_p => $aterm_gp_data->{$gp_acc}{has_good_iss_p},
	     #has_odd_iss_p => $aterm_gp_data->{$gp_acc}{has_odd_iss_p},
	    };
	}else{
	  $stack_info{$gp_acc}{has_exp_p} += $assoc_data->{has_exp_p};
	  $stack_info{$gp_acc}{has_good_iss_p} += $assoc_data->{has_good_iss_p};
	  $stack_info{$gp_acc}{has_odd_iss_p} += $assoc_data->{has_odd_iss_p};

	  $self->kvetch("\t\tadditive direct: " . $assoc_data->{direct_p} .
			' exp ' . $stack_info{$gp_acc}{has_exp_p} );
	}
      }
    }

    ## Give the order by
    ## Add the stacked node.
    #$gv->add_stacked_node($nodes{$t}, \@stack_info);
    ## Reduce into an array.
    my @stack_info = map { $stack_info{$_} } keys %stack_info;
    my @ordered_info = sort {
      #$a->{name} cmp $b->{name};
      #$self->kvetch("__>>__" . $a->{name});
      #sleep 2;
      $self->{AMIGO_ORDER_VALUE}{$a->{name}} <=>
	$self->{AMIGO_ORDER_VALUE}{$b->{name}};
    } @stack_info;

    $gv->add_stacked_node($nodes->{$acc}, \@ordered_info);
  }

  ###
  ### Generate JS variable amigo_species for RefGenome.js from
  ### AMIGO_TERMS.
  ###

  foreach my $acc (keys %{$self->{AMIGO_TERMS}}){

    foreach my $gp_acc (keys %{$self->{AMIGO_TERMS}{$acc}{gene_products}} ){

      my $aterm_gp_data = $self->{AMIGO_TERMS}{$acc}{gene_products}{$gp_acc};
      my $spec_name = $aterm_gp_data->{species};
      my $color = $aterm_gp_data->{color};
      my $direct_p = $aterm_gp_data->{direct_p};

      ## Add to our seen list and tally it up.
      if( ! defined $self->{AMIGO_SPECIES}{$spec_name} ){
	$self->{AMIGO_SPECIES}{$spec_name} =
	  {
	   ## New things.
	   name => $spec_name,
	   color => $color,
	   indirect_count => 0,
	   direct_count => 0,
	   direct_terms => {},
	   indirect_terms => {},
	  };
      }

      ## Direct and indirect caching.
      $self->{AMIGO_SPECIES}{$spec_name}{indirect_count}++;
      $self->{AMIGO_SPECIES}{$spec_name}{indirect_terms}{$acc} = 1;
      if( $direct_p ){
	$self->{AMIGO_SPECIES}{$spec_name}{direct_terms}{$acc} = 1;
	#$self->kvetch("___" . $acc, 1) if $spec_name eq 'yeast';
      }
    }
  }

  ## Just get the direct count by species from the term hash.
  foreach my $spc (keys %{$self->{AMIGO_SPECIES}}){
    $self->{AMIGO_SPECIES}{$spc}{direct_count} = 
      scalar( keys %{$self->{AMIGO_SPECIES}{$spc}{direct_terms}} );
  }

  ## Time.
  $self->kvetch('_3_time: ' . tv_interval($t));
  $t = [gettimeofday];

  ###
  ### The reduced SVG rewrite section.
  ###
  ### From here on, we're just going to try and tame the nasty output
  ### from Graphviz on the SVG.
  ###

  ## Get what GV regards as SVG...and work with it.
  ## TODO/BUG: This little section right here is the major slowdown.
  ## TODO: Note that this if section should not appear in the final
  ## version--it's only here to facilitate graphviz testing.
  my $svg_file = '';
  if( $gv_out_type eq 'dot' ){
    my $file = $gv->get_dot(0);
    $self->kvetch('_5_time: ' . tv_interval($t));
    return $file;
  }elsif( $gv_out_type eq 'png' ){
    my $file = $gv->get_png(0);
    $self->kvetch('_5_time: ' . tv_interval($t));
    return $file;
  }elsif( $gv_out_type eq 'svg_raw' ){
    my $file = $gv->get_svg(0);
    $self->kvetch('_5_time: ' . tv_interval($t));
    return $file;
  }else{
    $svg_file = $gv->get_svg(0);
  }

  ## Time.
  $self->kvetch('_5_time: ' . tv_interval($t));
  $t = [gettimeofday];

  my $svg_rewriter = AmiGO::SVGRewrite->new();
  $svg_rewriter->add_svg_title($info->get_symbol() . ': interactive SVG');

  ## Add links (couldn't do them dynamically).
  $svg_rewriter->add_svg_segment('<a xlink:href="http://wiki.geneontology.org/index.php/AmiGO_Manual:_RG_Graphical_View" xlink:title="View documentation for the interactive graphs."><text style="font-size:10px;fill:blue;" font-size="10" x="57px" y="580px">(view graph help pages)</text></a>');
  $svg_rewriter->add_svg_segment('<a xlink:href="' . $info->get_link() . '" xlink:title="Return to the homolset details page for ' . $info->get_symbol() . '."><text style="font-size:10px;fill:blue;" font-size="10px" x="17px" y="565px">(return to homolog set details page)</text></a>');

  ##
  my $tmp_spec_order = $self->{AID}->species_list({safely=>0});
  my $spec_order = [];
  foreach my $spec (@$tmp_spec_order){
    if( defined $self->{AMIGO_SPECIES}{$spec} ){
      push @$spec_order, $spec;
    }
  }
  $self->kvetch('amigo_species_order');
  $svg_rewriter->add_js_variable('amigo_species_order', $spec_order);

  ##
  my @tmp_spec_info = keys %{$self->{AMIGO_SPECIES}};
  $svg_rewriter->add_species(\@tmp_spec_info);
  $svg_rewriter->add_js_variable('amigo_species', $self->{AMIGO_SPECIES});
  $svg_rewriter->add_js_variable('amigo_terms', $self->{AMIGO_TERMS});
  $svg_rewriter->add_js_variable('homolset_symbol', $info->get_symbol());
  $svg_rewriter->add_js_variable('homolset_link', $info->get_link());

  $svg_rewriter->add_js_library('org.bbop.NodeDetails');
  $svg_rewriter->add_js_initializer("org.bbop.NodeDetails('detail_context');");

  $svg_rewriter->add_js_library('org.bbop.Viewer');
  $svg_rewriter->add_js_initializer("org.bbop.Viewer('rgsvg','tform_matrix');");

  $svg_rewriter->add_js_library('RefGenome');
  $svg_rewriter->add_js_initializer("RefGenomeInit('underlay_context_1', 'underlay_context_2', 'underlay_context_3', 'overlay_context','control_context');");

  $svg_file = $svg_rewriter->rewrite($svg_file);

  $self->kvetch('_6_time: ' . tv_interval($t));

  return $svg_file;
}



1;
