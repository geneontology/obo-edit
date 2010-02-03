=head1 GOBO::DBIC::GODBModel::Graph

This graph should probably be sub-classed as an ontology, and the
connecting and abstract bits should be shifted around.

NOTE: This uses graph_path as the primary engine.

=cut

use utf8;
use strict;

package GOBO::DBIC::GODBModel::Graph;

use base 'GOBO::DBIC::GODBModel';
use utf8;
use strict;
use GOBO::DBIC::GODBModel::Schema;
use GOBO::DBIC::GODBModel::Query;
use Graph::Directed;
use Graph::TransitiveClosure;

=item new

=cut
sub new {

  ##
  my $class = shift;
  my $args = shift || {};
  my $self  = $class->SUPER::new($args);

  ## Defined in super now...
  #   $self->{SCHEMA} =
  #     GOBO::DBIC::GODBModel::Schema->connect($self->{CONNECT_INFO});
  $args->{type} = 'term2term_over_graph_path';
  $self->{GRAPH_Q} = GOBO::DBIC::GODBModel::Query->new($args);

  ## We'll borrow SUCCESS and ERROR_MESSAGE from GOBO::DBIC::GODBModel.

  ### Nodes are defined as terms (keyed by acc) and edges are defined
  ### as two terms, a relationship, and a completeness (keyed
  ### accordingly).
  #$self->{NODES} = {};
  #$self->{EDGES} = {};

  ## TODO/BUG: the below would be preferable if the GO wasn't borked.
  #my $rrs = $schema->resultset('Term')->search({is_root => 1});

  ## Try and guess which term id the "bad" root.
  my $all_db_id = 1;
  my $tmp_rs = $self->{SCHEMA}->resultset('Term')->search({ is_root => 1 });
  my $possible_root_term = $tmp_rs->next;
  if( $possible_root_term && $possible_root_term->acc eq 'all' ){
    $all_db_id = $possible_root_term->id;
  }

  $self->{ROOTS} = {};
  my $rrs =
    $self->{SCHEMA}->resultset('Term2Term')->search({ term1_id => $all_db_id });
  while( my $possible_root_rel = $rrs->next ){
    my $term = $possible_root_rel->subject;
    if( ! $term->is_obsolete && $term->name ne 'all' ){
      $self->{ROOTS}{$term->acc} = $term;
    }
  }

  bless $self, $class;
  return $self;
}


## Internal convenience function.
sub _convert_term_or_acc_to_acc {

  my $self = shift;
  my $term = shift || '';

  ## Convert string or obj to acc string.
  my $term_acc = $term; # assume as string
  if( $term && (ref $term eq 'GOBO::DBIC::GODBModel::Schema::Term' ) ){
    $term_acc = $term->acc;
  }

  return $term_acc;
}


=item get_roots

Returns the root nodes.

=cut
sub get_roots {
  my $self = shift;

  ## We don't want to actually pass this thing...makes Continuity.pm
  ## cry.
  my $copy = {};
  foreach my $key (keys %{$self->{ROOTS}}){
    $copy->{$key} = $self->{ROOTS}{$key};
  }
  return $copy;
}


=item is_root_p

Boolean on acc string or DBIx::Class Term.

=cut
sub is_root_p {
  my $self = shift;
  my $thing = shift || '';

  ##
  my $retval = 0;
  my $acc = $self->_convert_term_or_acc_to_acc($thing);
  #$self->kvetch('_root_p_acc_: ' . $acc);
  if( defined $self->{ROOTS}{$acc} ){
    $retval = 1;
  }
  #$self->kvetch('_root_p_ret_: ' . $retval);
  return $retval;
}


=item is_leaf_p

Boolean on acc string or DBIx::Class Term.

=cut
sub is_leaf_p {
  my $self = shift;
  my $thing = shift || '';
  # $self->kvetch('>>><<<');

  ## Assume leafiness until proven otherwise.
  my $retval = 1;
  my $res = $self->get_child_relationships($thing);
  if( $res && scalar(@$res) ){
    $retval = 0;
  }
  return $retval;
}


=item get_term

Gets a term from an acc string or DBIx::Class Term.
A Term just gets passed through (little overhead).

=cut
sub get_term {

  my $self = shift;
  my $reterm = shift || undef;

  #$self->kvetch('GOBO::DBIC::GODBModel::Graph::get_term in: '.$reterm);
  #$self->kvetch('GOBO::DBIC::GODBModel::Graph::get_term ref: '.ref($reterm));

  ## Convert to Term object if it looks like a (acc) string.
  if( defined $reterm && ref $reterm ne 'GOBO::DBIC::GODBModel::Schema::Term' ){
    my $term_rs = $self->{SCHEMA}->resultset('Term')->search({acc => $reterm});
    $reterm = $term_rs->first || undef;
  }

  return $reterm;
}


=item get_children

In: acc string or Term.
Out: Children term (object) list ref.

=cut
sub get_children {

  my $self = shift;
  #my $term = shift || undef;
  my $thing = shift || '';
  my $acc = $self->_convert_term_or_acc_to_acc($thing);

  my $all = $self->{GRAPH_Q}->get_all_results({'graph_object.acc' => $acc,
					       'graph_path.distance' => 1});

  my $ret = [];
  foreach my $t2t (@$all){
    if( ! $t2t->subject->is_obsolete ){
      push @$ret, $t2t->subject;
      # $self->kvetch("GOBO::DBIC::GODBModel::Graph::get_children: kid: " .
      # 		    $t2t->subject->acc);
    }
  }
  return $ret;
}


=item get_relationship

In: term, term; take string or object.
Out: int.

=cut
sub get_relationship {

  my $self = shift;
  my $obj_thing = shift || '';
  my $sub_thing= shift || '';
  my $obj_acc = $self->_convert_term_or_acc_to_acc($obj_thing);
  my $sub_acc = $self->_convert_term_or_acc_to_acc($sub_thing);

  my $all = $self->{GRAPH_Q}->get_all_results({'object.acc' => $obj_acc,
					       'subject.acc' => $sub_acc});

  ## Should be one.
  my $ret = undef;
  foreach my $t2t (@$all){
    #$ret = $t2t->relationship_type_id;
    $ret = $t2t->relationship->name;
    last;
  }
  return $ret;
}


=item get_child_relationships

Takes DBIx::Class Term or acc string.
Gets the term2term links from a term.

=cut
sub get_child_relationships {

  my $self = shift;
  my $term = shift || undef;
  my $term_acc = $self->_convert_term_or_acc_to_acc($term);

  return $self->{GRAPH_Q}->get_all_results({'graph_object.acc' => $term_acc,
					    'graph_path.distance' => 1});
}


=item get_parent_relationships

Takes DBIx::Class Term or acc string.
Gets the term2term links from a term.

=cut
sub get_parent_relationships {

  my $self = shift;
  my $term = shift || undef;
  my $term_acc = $self->_convert_term_or_acc_to_acc($term);

  return
    $self->{GRAPH_Q}->get_all_results({'graph_subject.acc' => $term_acc,
				       'graph_path.distance' => 1});
}


=item climb

TODO: acc or str, singular or listref

With an array ref of terms, will climb to the top of the ontology
(with an added 'all' stopper for GO).

This returns an array of five things:
   (\%nodes, \%edges, \%tc_desc, \%tc_anc, \%tc_depth);
   *) a link list
   *) a term (node)
   *) a hashref of of nodes in terms of in-graph descendants

TODO: should also be able to take array ref of strings...

=cut
sub climb {

  my $self = shift;
  my $in_thing = shift || [];

  ## Whatever it is, arrayify it.
  if( ref $in_thing ne 'ARRAY' ){
    $in_thing = [$in_thing];
  }

  ## Whatever is in there, make sure that they're all Terms. Use
  ## Graph::get_term pass-through.
  my @seed_terms = map {
  #my @blah = map {
    $self->get_term($_);
  } @$in_thing;
  #my $seed_terms = \@blah;

  ## For doing transitive closure on the graph to help with
  ## association transfer.
  my $booking_graph = Graph::Directed->new();

  # $self->kvetch("Climb: IN");

  ## Pre-seed the nodes list.
  my %edges = ();
  my %nodes = ();
  foreach my $seed ( @seed_terms ){
    $nodes{$seed->acc} = $seed;
    # $self->kvetch("Climb: added seed: " . $seed->acc);
  }

  ##
  my %in_graph_by_acc = ();
  while( @seed_terms ){

    my $current_term = pop @seed_terms;

    ## BUG: Prevent super root (not really our bug though).
    my $current_acc = $current_term->acc;
    if( $current_acc ne 'all' ){

      ## Add node to hash if not already there.
      if( ! $nodes{$current_acc} ){
	$nodes{$current_acc} = $current_term;
	# $self->kvetch("Climb: adding node: " . $current_acc);
	$booking_graph->add_vertex($current_acc);
      }

      ## Find parent relations each time.
      #my $parent_rs = $current_term->parent_relations;
      #my @all_parents = $parent_rs->all;
      #foreach my $parent_rel (@all_parents){
      #$self->kvetch('new code', 1);
      my $all_parents = $self->get_parent_relationships($current_term);
      foreach my $parent_rel (@$all_parents){

	my $id = $parent_rel->id;

	my $obj = $parent_rel->object;
	my $obj_acc = $obj->acc;

	## BUG: Prevent links to super root (not really our bug though).
	if( $obj_acc ne 'all' ){

	  my $sub = $parent_rel->subject;
	  my $sub_acc = $sub->acc;
	  #my $rel_id = $parent_rel->relationship_type_id;
	  my $rel_id = $parent_rel->relationship->name;

	  ## Add edge to hash if not already there.
	  if( ! defined $edges{$id} ){
	    $edges{$id} = $parent_rel;
	    # $self->kvetch("Climb adding edge: $sub_acc $rel_id $obj_acc");
	    $booking_graph->add_edge($sub_acc, $obj_acc);
	  }

	  ## Make sure that there is a space in the indirect hash
	  ## if it is not already there.
	  if( ! defined $in_graph_by_acc{$obj_acc} ){
	    $in_graph_by_acc{$obj_acc} = {};
	  }

	  ## TODO: double check the correctness of this...
	  ## If we haven't seen it, mark it and climb.
	  if( ! defined $in_graph_by_acc{$obj_acc}{$sub_acc} ){

	    $in_graph_by_acc{$obj_acc}{$sub_acc} = 1;
	    push @seed_terms, $obj;
	  }
	}
      }
    }
  }

  ###
  ### From here on is just reworking everything using the Graph module
  ### to pull out useful information.
  ###

  ## Calculate the transitive closure to help with figuring out the
  ## association transitivity in other components.
  my $tc_graph = Graph::TransitiveClosure->new($booking_graph,
					       reflexive => 0,
					       path_length => 1);
  my %tc_desc = ();
  my %tc_anc = ();

  ## Iterate through the combinations making the anc and desc hashes.
  foreach my $obj (keys %nodes){

    $tc_desc{$obj} = {} if ! defined $tc_desc{$obj};
    $tc_anc{$obj} = {} if ! defined $tc_anc{$obj};

    foreach my $sub (keys %nodes){

      if( $tc_graph->is_reachable($obj, $sub) ){
	$tc_anc{$obj}{$sub} = 1;
      }
      if( $tc_graph->is_reachable($sub, $obj) ){
	$tc_desc{$obj}{$sub} = 1;
      }
    }
  }

  ## Down here, we're doing something separate--we're going to get
  ## the depth of the node.
  #TODO
  my %tc_depth = ();
  foreach my $sub (keys %nodes){
    foreach my $root (keys %{$self->{ROOTS}}){
      my $len = $tc_graph->path_length($sub, $root);
      if( defined $len ){
	$tc_depth{$sub} = $len;
	# $self->kvetch('Depth of ' . $sub . ' is ' . $len);
      }
    }
  }

  #return (\%nodes, \%edges, \%in_graph_by_acc);
  #return (\%nodes, \%edges, \%tc_desc);
  return (\%nodes, \%edges, \%tc_desc, \%tc_anc, \%tc_depth);
}


# =item ancestors

# Takes a model term, returns an array ref of model term ancestors

# =cut
# sub ancestors {

#   my $self = shift;
#   my $term = shift;
#   my $ancestors = [];

#   ##
  

#   return $ancestors;
# }



1;
