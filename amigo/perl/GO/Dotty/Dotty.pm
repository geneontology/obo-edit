# $Id: Dotty.pm,v 1.7 2006/12/12 15:44:43 girlwithglasses Exp $

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



my %labellednodes;

sub graphviz_to_dotty{
	my $graphviz = shift;
	
	open( DOTTY, "|dotty -" );
	print DOTTY $graphviz->as_text;
	close( DOTTY );
}

sub go_graph_to_graphviz {
	my $graph = shift;
	my %gopts = %{shift || {}};
	my %opts = %{shift || {}};
	my $it = $graph->create_iterator;
	my %relationships;
	my %labellednodes;
	
	my %relcolors = (
		is_a => 'red',
		part_of => 'blue',
		develops_from => 'green',
	);

	my %default_graph =
	(	rankdir => $gopts{layout}-- || 0,
		directed => 1,
		node => {
			shape=>'box',
			color=>'black',
			style=>'filled',
			fontsize => 10,
			fillcolor => $gopts{fillcolor} || 'white',
			fontcolor => $gopts{fontcolor} || 'blue',
		},
		edge => {
			fontsize => 8
		},
	);
	
	my $graphviz = GraphViz->new(%default_graph);
	
	#new
	my %term_h;
	my $terms = $graph->get_all_nodes;
	map { $term_h{$_->acc} = $_ } @$terms;
	
	while (my $ni = $it->next_node_instance ) {
		my $term = $ni->term;
	#	print STDERR "\nterm: ".$term->acc."\n";
		_graphviz_label( $term, $graph, \%labellednodes, $graphviz, \%opts );
		
		#new
		my $parent_rels = $graph->get_parent_relationships($term->acc);
		use Data::Dumper;
		$Data::Dumper::Indent = 1;
	#	print STDERR "parent_rels: ".Dumper($parent_rels)."\n";
		foreach my $rel (@$parent_rels)
		{	my $p = $term_h{$rel->acc1};
		#	print STDERR "parent term: ".$p->acc."\n";
			_graphviz_label( $p, $graph->is_focus_node( $p ), \%labellednodes, $graphviz, \%opts );
	#	my $parents = $graph->get_parent_terms($term->acc);
	#	foreach my $p (@$parents) {
	#		_graphviz_label( $p, $graph->is_focus_node( $p ), \%labellednodes, $graphviz, \%opts );
	
			my $identifier = $p->acc . " " . $term->acc;
			unless( $relationships{ $identifier } ){
			## for some reason graphviz assumes that a number is only a
			## label, and not a node name which is irritating.
				my $node = "acc" . $term->acc;
				my $pnode = "acc" . $p->acc;
				my $relation = $rel->type;
			#	print STDERR "Adding edge: $node $relation $pnode\n";
			#	my $apph = $term->apph;
			#	my $relation = $apph->get_relationships( {child=>$term, parent=>$p} );
		
				$graphviz->add_edge($node => $pnode, label => $relation, color => $relcolors{$relation} || 'black');
		#		{	$graphviz->add_edge($node => $pnode, label => $relation->[0]->type, color => $relcolors{$relation->[0]->type} || 'black');
				$relationships{ $identifier } = 1;
			}
		}
	
		if ($opts{selected_assocs}) {
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
	my %opts = %{ shift || {}};
	my $acc = $term->acc;
	my $node = "acc" . $term->acc;

	unless( $labellednodes->{$acc} ){
		if ($opts{sub}) {
			 $graphviz->add_node($opts{sub}->($node, $term));
		}
		elsif ($opts{base_url}) {
			$graphviz->add_node
				(	$node, 
					label => $term->name . "\n" . $term->acc,
					URL => $opts{'base_url'}.$term->acc,
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
