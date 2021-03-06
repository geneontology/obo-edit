# $Id: Dotty.pm,v 1.10 2010/02/08 04:27:37 sjcarbon Exp $

package GO::Dotty::Dotty;

=head1 NAME

  Dotty::Dotty;

=head1 SYNOPSIS

  Utility functions for producing Dotty diagrams from GO graphs

  Contributed to the go-database list by Philip Lord

=head1 REQUIREMENTS

http://www.research.att.com/sw/tools/graphviz/
http://search.cpan.org/search?dist=GraphViz

=cut

use strict;
use Exporter;
use GraphViz;
my @ISA=qw{Exporter};
use GO::CGI::Utilities qw(:all);
my $verbose = get_environment_param('verbose');

my %labellednodes;

#	not used!
sub graphviz_to_dotty{
	my $graphviz = shift;
	
	open( DOTTY, "|dotty -" );
	print DOTTY $graphviz->as_text;
	close( DOTTY );
}

sub go_graph_to_graphviz {
	my $graph = shift;
	my $gopts = shift;
	my $opts = shift;
	my $it = $graph->create_iterator;
	$it->compact(1);
	my %relationships;
	my %labellednodes;
	
	if ($gopts->{layout})
	{	#	change layout values to 0 or 1
		if ($gopts->{layout} eq 'horizontal')
		{	$gopts->{layout} = 1;
		}
		else
		{	delete $gopts->{layout};
		}
	}
	my %relcolors = (
		is_a => 'red',
		part_of => 'blue',
		develops_from => 'green',
		regulates => 'brown',
		negatively_regulates => 'tan',
		positively_regulates => 'black'
	);

	my %default_graph =
	(	rankdir => $gopts->{layout} || 0,
		directed => 1,
		node => {
			shape=>'box',
			color=>'black',
			style=>'filled',
			fontsize => 10,
			fillcolor => $gopts->{fillcolor} || 'white',
			fontcolor => $gopts->{fontcolor} || 'blue',
		},
		edge => {
			fontsize => 8
		},
	);

	my $path_to_dot = get_environment_param('dot_path') || '/dot';
	$path_to_dot =~ s/\/dot$//;
	if ($path_to_dot)
	{	$ENV{PATH} .= ':' . $path_to_dot;
	}
	print STDERR "creating new GraphViz graph...\n" if $verbose;
	my $graphviz = GraphViz->new(%default_graph);
	print STDERR "done!\n" if $verbose;
	#new
	my %term_h;
	my $terms = $graph->get_all_nodes;
	map { $term_h{$_->acc} = $_ } @$terms;

	while (my $ni = $it->next_node_instance ) {
		my $term = $ni->term;
		_graphviz_label( $term, $graph,
				 \%labellednodes, $graphviz, $opts );
		#new
		my $parent_rels = $graph->get_parent_relationships($term->acc);
		foreach my $rel (@$parent_rels){
		  my $p = $term_h{$rel->acc1};
		  _graphviz_label( $p, $graph->is_focus_node( $p ),
				   \%labellednodes, $graphviz, $opts );
		  #	my $parents = $graph->get_parent_terms($term->acc);
		  #	foreach my $p (@$parents) {
		  #	 _graphviz_label( $p, $graph->is_focus_node( $p ),
		  #         \%labellednodes, $graphviz, \%opts );

		  my $identifier = $p->acc . " " . $term->acc;
		  unless( $relationships{ $identifier } ){
		    ## for some reason graphviz assumes that a number is only a
		    ## label, and not a node name which is irritating.
		    my $node = "acc" . $term->acc;
		    my $pnode = "acc" . $p->acc;
		    my $relation = $rel->type;
		    #	print STDERR "Adding edge: $node $relation $pnode\n";
		    #	my $apph = $term->apph;
		    #	my $relation =
		    #    $apph->get_relationships( {child=>$term, parent=>$p} );

		    $graphviz->add_edge($node => $pnode, label => $relation,
					color=>$relcolors{$relation} ||'black');
		    $relationships{ $identifier } = 1;
		  }
		}

		if ($opts->{selected_assocs}) {
			my @prs = map {$_->gene_product } @{$term->selected_association_list || []};
			my $node = "acc" . $term->acc;
			foreach my $pr (@prs) {
				my $nid = $pr->xref->as_str;
				unless($labellednodes{$nid}) {
					$graphviz->add_node($nid, label=>$pr->symbol, color=>'red');
					$labellednodes{$nid} = 1;
				}
				$graphviz->add_edge( $nid=>$node, label=>"annotated_to");
			}
		}
	}
	return $graphviz;
}

sub _graphviz_label{
	my $term = shift;
	my $graph = shift;
	my $labellednodes = shift;
	my $graphviz = shift;
	my $opts = shift;
	my $acc = $term->acc;
	my $node = "acc" . $term->acc;

	unless( $labellednodes->{$acc} ){
		if ($opts->{'sub'}) {
			 $graphviz->add_node($opts->{'sub'}->($node, $term));
		}
		elsif ($opts->{base_url}) {
			$graphviz->add_node
				(	$node, 
					label => $term->name . "\n" . $term->acc,
					URL => $opts->{'base_url'}.$term->acc,
					fontname => 'Courier'
				);
		}
		else {
			$graphviz->add_node
				(	$node, 
					label => $term->name . "\n" . $term->acc,
					fontname => 'Courier'
				);
		}
		$labellednodes->{$acc} = 1;
	}
}

sub label_nodes_with_colour{
	my $graphviz = shift;
	my $terms = shift;
	my $colour = shift;
	
	foreach my $term (@$terms){
		my $node = "acc" . $term->acc;
		$graphviz->add_node
		( $node,
			style=>"filled",
			color=>$colour,
			fontname=>'Courier'
		);
	}
}

# support US spelling
*label_nodes_with_color = \&label_nodes_with_colour;

1;
