=head1 AmiGO::Worker::Term

DEPRECATED

Generates consumable static information about ontology terms.
This is not a search tool, but a (hopefully efficient) data retrieval tool.

=cut

use utf8;
use strict;

package AmiGO::Worker::Term;

use base ("AmiGO::Worker");

use Data::Dumper;
use AmiGO::Aid;
use GOBO::DBIC::GODBModel::Graph;

=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self = $class->SUPER::new();
  #  my $excessive = shift || undef; # TODO: document

  $self->{AW_TQ} = GOBO::DBIC::GODBModel::Query->new({type=>'term_lazy'});
  $self->{AW_TG} = GOBO::DBIC::GODBModel::Graph->new();
  $self->{AW_AID} = AmiGO::Aid->new();
  #  $self->{AWT_LOTS} = $excessive;

  bless $self, $class;
  return $self;
}


=item get_info

Args: term acc string or arrayref of term acc strings.
    : takes optional arg {lite => (0|1)}, default 0
Returns: hash containing various term infomation, keyed by acc

=cut
sub get_info {

  my $self = shift;
  my $arg = shift || die "need an argument";
  my $opt_arg = shift || {};

  ## Only array refs.
  if( ref $arg ne 'ARRAY' ){ $arg = [$arg]; }

  my $is_lite_p = 0;
  $is_lite_p = 1 if $opt_arg && $opt_arg->{lite};

  ## Get term information for display.
  my $query_results =
    $self->{AW_TQ}->get_all_results({'me.acc' => {-in => $arg}});
  my $term_info = undef;

  if( $is_lite_p ){
    $term_info = $self->{AW_AID}->lite_term_information($query_results);
  }else{
    $term_info = $self->{AW_AID}->term_information($query_results);
  }

  return $term_info;
}


=item get_child_info

Args: term acc string or arrayref of term acc strings.
Returns: hash containing various term infomation, keyed by (int) order

=cut
sub get_child_info {

  my $self = shift;
  my $arg = shift || die "need an argument";

  ## Only array refs.
  if( ref $arg ne 'ARRAY' ){ $arg = [$arg]; }

  ###
  ### Get neighborhood below term(s).
  ###

  ## We're capable of getting multiple child relations from the
  ## graph_path table, so we are going to filter for the "strongest"
  ## one and use that as the single representative child.
  my $the_single_child = {};
  my $child_rels = $self->{AW_TG}->get_child_relationships($arg);
  #$self->kvetch('_a_: ' . $child_rels);
  #$self->kvetch('_b_: ' . scalar(@$child_rels));
  foreach my $child_rel (@$child_rels){

    my $rel = $child_rel->relationship; #->name;
    my $sub = $child_rel->subject;

    #my $rel_name = $rel->name;
    my $rel_acc = $rel->acc;
    my $sub_acc = $sub->acc;
    my $sub_name = $sub->name;

    # $self->kvetch('_c.r_: ' . $rel_acc);
    # $self->kvetch('_c.s_: ' . $sub_acc);
    # $self->kvetch('_c.n_: ' . $sub_name);

    my $add_it_p = 1;

    ## If the item is already in, check weight.
    if( defined $the_single_child->{$sub_acc} ){
      if( $self->{AW_TG}->relation_weight($rel_acc, 1000) <
	  $self->{AW_TG}->relation_weight($the_single_child->{$sub_acc}{rel},
					  1000) ){
	$add_it_p = 0;
      }
    }

    ## If the item acc smells obsolete, prevent it from being
    ## displayed/getting into the mix.
    if( $sub_acc =~ /^obsolete_/ ){
      $add_it_p = 0;
    }

    ## If it passed that above tests, add it.
    if( $add_it_p ){

      $the_single_child->{$sub_acc} =
	{
	 acc => $sub_acc,
	 name => $sub_name,
	 rel => $rel_acc,
	 link => $self->get_interlink({mode => 'term_details',
				       arg => {acc => $sub_acc},
				      }),
	 #optional => {frag => 'lineage'}}),
	};
    }
  }

  ## Unwind hash key for gpc info list and child chunks.
  my $child_chunks = [];
  foreach my $sub_acc (keys %$the_single_child){
    #push @$acc_list_for_gpc_info, $sub_acc;
    push @$child_chunks, $the_single_child->{$sub_acc};
  }

  ## Name ordering.
  my @sorted_child_chunks = sort {
    lc($a->{name}) cmp lc($b->{name})
  } @$child_chunks;


  return \@sorted_child_chunks;
}


=item get_ancestor_info

Args: term acc string or arrayref of term acc strings.
    : takes optional arg {reflexive => (0|1)}
Returns: hash containing various term infomation, keyed by (string) type

=cut
sub get_ancestor_info {

  my $self = shift;
  my $arg = shift || die "need an argument";
  my $opt_arg = shift || {};

  my $is_reflexive_p = 0;
  $is_reflexive_p = 1 if $opt_arg && $opt_arg->{reflexive};

  ## Only array refs.
  if( ref $arg ne 'ARRAY' ){ $arg = [$arg]; }

  ###
  ### Get neighborhood above term(s).
  ###

  $self->{AW_TG}->verbose(1);

  #$self->kvetch("Start lineage arg: " . Dumper($arg));
  my($lnodes, $lnode_rel, $lnode_rel_inf, $lnode_depth, $max_ldepth) =
    $self->{AW_TG}->lineage($arg);
  #$self->kvetch('lnodes: ' . Dumper($lnodes));
  # $self->kvetch('lnode_rel: ' . Dumper($lnode_rel));
  # $self->kvetch('lnode_depth: ' . Dumper($lnode_depth));
  # $self->kvetch('max_depth: ' . Dumper($max_ldepth));
  #$self->kvetch("Stop lineage");
  #die;

  ## Adjust if we want depth done to reflexive levels.
  if( $is_reflexive_p ){
    $max_ldepth += 1;
  }

  ## We'll want to know if later input is in the incoming arg list.
  my %in_arg_list = map { $_ => 1 } @$arg;

  ## Sort into buckets depending on reported depth.
  my $acc_list_for_gpc_info = [];
  my $nodes_by_depth = {};
  # my $max_depth = 0;
  foreach my $acc (keys %$lnodes){

    ## Only continue if not self, don't want reflexive input.
    #if( $acc ne $input_term_id ){
    #$self->kvetch("looking at1: " . $acc);
    if( ! $in_arg_list{$acc} || $is_reflexive_p ){

      ## 
      my $depth = $lnode_depth->{$acc};
      $self->kvetch("looking at: " . $acc . ', depth: ' . $depth);
      if( ! defined $nodes_by_depth->{$depth} ){
	$nodes_by_depth->{$depth} = [];
	$self->kvetch('made level: ' . $depth);
      }

      ## Add manufactured struct.
      my $term = $lnodes->{$acc};
      my $rel = $lnode_rel->{$acc};
      my $inf = $lnode_rel_inf->{$acc};

      ## Looks like it's not a member of this "reduced" graph.
      if( ! defined $rel ){ $rel = 'fatal'; }

      #$self->kvetch("_rel: " . $rel);
      push @{$nodes_by_depth->{$depth}},
	{
	 acc => $acc,
	 inferred_p => $inf,
	 name => $term->name,
	 rel => $rel,
	 link => $self->get_interlink({
				       mode => 'term_details',
				       arg => {acc => $acc},
				      }),
	};
      push @$acc_list_for_gpc_info, $acc;
    }
  }
  #$self->kvetch("nodes_by_depth:\n" . Dumper($nodes_by_depth));
  ##$self->kvetch("adepth:\n" . Dumper($adepth));
  $self->kvetch("_max_depth: " . $max_ldepth);
  my $nodes_sorted_by_depth = {};
  for( my $depth = 0; $depth < $max_ldepth; $depth++ ){
    #$self->kvetch("_depth: " . $depth);
    if( defined $nodes_by_depth->{$depth} ){
      my @blah = sort {
	lc($a->{name}) cmp lc($b->{name})
      } @{$nodes_by_depth->{$depth}};
      $nodes_sorted_by_depth->{$depth} = \@blah;
    }
    #$self->kvetch("nbd:\n" .Dumper($nodes_by_depth->{$depth}));
    $self->kvetch("nsbd $depth:\n" .Dumper($nodes_sorted_by_depth->{$depth}));
  }

  ## Out.
  return
    {
     'seen_acc_list' => $acc_list_for_gpc_info,
     'max_depth' => $max_ldepth,
     'max_displacement' => $max_ldepth + 2,
     'parent_chunks_by_depth' => $nodes_sorted_by_depth,
    };
}



1;
