=head1 SuGR::BaryMatrix

TODO/BUG: redo doc to reflect perliness.

Takes arrays of vertices and edges as an argument. Edges must have
the methods '.object()' and '.subject()' and Vertices must have
method '.id()'.

Smallest error set with current barymatrix:
   GO:0003674 GO:0005575 GO:0008150 GO:0000150 GO:0048046

=cut

package SuGR::BaryMatrix;

use strict;
use utf8;
use base 'SuGR';
use Data::Dumper;


###
###
###

=item new

Constr.

object, subject, and edge partitions

This function really just creates the relationship matrix from the
above components.

=cut
sub new {

  my $class = shift;

  my $object_vertex_partition = shift || [];
  my $subject_vertex_partition = shift || [];
  my $edge_partition = shift || [];

  my $self = {
	      ##
	      LOG => Log::Log4perl->get_logger('SuGR::BaryMatrix'),

	      ##
	      object_vector => $object_vertex_partition,
	      subject_vector => $subject_vertex_partition,
	      edge_partition => $edge_partition,

	      ##
	      relation_matrix => {},

	      ##
	      #BC_CONST => 0.0,
	      #BC_CONST => 100.0,
	      BC_CONST => -100.0,
	     };

  #$self->{LOG}->debug(scalar(@{$self->{edge_partition}}));
  #sleep 2;

  ## Build up the relation matrix from the edge partition. Go over
  ## Each edge in the edge partition.
  foreach my $edge (@{$self->{edge_partition}}){

    #my $obj = $self->{edge_partition}[$i];
    #my $sub = $self->{edge_partition}[$i];
    my $obj_id = $edge->{object_id};
    my $sub_id = $edge->{subject_id};

    ## Add if not defined.
    #if( ! $self->{relation_matrix}{sub_id} ){
    #  $self->{relation_matrix}{sub_id} = {}; }
    if( ! defined $self->{relation_matrix}{$obj_id} ){
      $self->{relation_matrix}{$obj_id} = {};
    }

    ## Now that it's defined, add the subject.
    ##$self->{relation_matrix}[ sub_id ][ obj_id ] = false;
    $self->{relation_matrix}{$obj_id}{$sub_id} = 1;
  }

#   ## DEBUG: relation matrix:
#   for( my $m = 0; $m <= scalar(@{$self->{object_vector}}) -1; $m++ ){
#     my $ov_id_m = _vector_id_at_index($self->{object_vector}, $m);
#     $self->{LOG}->debug("obj: <<o: " . $ov_id_m . "\>\>");
#   }
#   for( my $n = 0; $n <= scalar(@{$self->{subject_vector}}) -1; $n++ ){
#     my $sv_id_n = _vector_id_at_index($self->{subject_vector}, $n);
#     $self->{LOG}->debug("sub: <<o: " . $sv_id_n . ">>");
#   }
#   foreach my $oid (keys %{$self->{relation_matrix}}){
#     foreach my $sid (keys %{$self->{relation_matrix}{$oid}}){
#       $self->{LOG}->debug("edge: <<o: " . $oid . ", s: " . $sid . ">>");
#     }
#   }

  $self->{LOG}->debug("Relationship matrix:");
  $self->{LOG}->debug(Dumper($self->{relation_matrix}));
  #sleep 2;

  bless $self, $class;
  return $self;
}


## BUG: These damn things seem to reoder on equal--want no reorder on
## equal. Reorder objects given B1 <= B2, where Bi is the barycenter
## weight.
sub barycentric_object_reorder {

  my $self = shift;

  #$self->{LOG}->debug("HERE2 ... " . scalar(@{$self->{object_vector}}));
  my @sorted_vector = sort {

    my $a_id = $a->{id};
    my $b_id = $b->{id};
    my $a_val = $self->_get_object_barycenter($a_id);
    my $b_val = $self->_get_object_barycenter($b_id);
    #return $a_val - $b_val;
    $a_val <=> $b_val;
#     #     return $self->get_object_barycenter($a) -
#     #       $self->get_object_barycenter($b);
#     $self->{LOG}->debug("HERE2.5 ... $a <=> $b");
#     #print STDERR Dumper($a);
#     $self->_get_object_barycenter($a) <=> $self->_get_object_barycenter($b);
  } @{$self->{object_vector}};

  #$self->_permutation_check($self->{object_vector}, \@sorted_vector);

  $self->{object_vector} = \@sorted_vector;
}


## BUG: These damn things seem to reoder on equal--want no reorder on
## equal. Reorder subjects given B1 <= B2, where Bi is the barycenter
## weight.
sub barycentric_subject_reorder {

  my $self = shift;

  my @sorted_vector = sort {

    my $a_id = $a->{id};
    my $b_id = $b->{id};
    my $a_val = $self->_get_subject_barycenter($a_id);
    my $b_val = $self->_get_subject_barycenter($b_id);
    #return $a_val - $b_val;
    $a_val <=> $b_val;
    ##return $self->get_subject_barycenter($a) -
    ##  $self->get_subject_barycenter($b);
    #$self->_get_subject_barycenter($a) <=> $self->_get_subject_barycenter($b);
  } @{$self->{subject_vector}};

  #$self->_permutation_check($self->{subject_vector}, \@sorted_vector);

  $self->{subject_vector} = \@sorted_vector;
}


##

=item _get_object_barycenter

Arg: object id (string)
Return: barycenter value (float)

=cut
sub _get_object_barycenter {

  my $self = shift;

  my $object_id = shift | '';
  my $weighted_number_of_edges = 0;
  my $number_of_edges = 0;

  #$self->{LOG}->debug("HERE 3, with object_id: " . $object_id);

  ## For each subject (starting at 1 for reasons that will soon be
  ## obvious)...
  for( my $i = 1; $i <= scalar(@{$self->{subject_vector}}); $i++ ){

    ## Calculate subject id for the previous one.
    my $prev_sub_id = _vector_id_at_index($self->{subject_vector}, $i -1);
    #$self->{LOG}->debug("HERE 3.5, with prev_sub_id: " . $prev_sub_id);

    ## If the object and previous subject exist, 
    if( defined $self->{relation_matrix}{$object_id} &&
	defined $self->{relation_matrix}{$object_id}{$prev_sub_id} ){
      $weighted_number_of_edges += $i;
      $number_of_edges++;
    }
  }

  ## The '-1' is to offset the indexing.
  ## TODO: Is 0 , 100, -100 OK for unconnected objects?
  my $barycenter = $self->{BC_CONST};
  if( $number_of_edges > 0 ){
    $barycenter = ( $weighted_number_of_edges / $number_of_edges ) -1.0
  }
  $self->{LOG}->debug('Object ('.$object_id.') barycenter is: ' . $barycenter);
  #sleep 1 if( $number_of_edges != 0 );
  return $barycenter;
}


=item _get_subject_barycenter

Gets barycenter for column s.
Arg: subject id (string)
Return: barycenter value (float)

=cut
sub _get_subject_barycenter {

  my $self = shift;

  my $subject_id = shift || '';
  my $weighted_number_of_edges = 0;
  my $number_of_edges = 0;

  ##
  for( my $i = 1; $i <= scalar(@{$self->{object_vector}}); $i++ ){

    ##
    my $prev_obj_id = _vector_id_at_index($self->{object_vector}, $i -1);

    ##
    if( defined $self->{relation_matrix}{$prev_obj_id} &&
	defined $self->{relation_matrix}{$prev_obj_id}{$subject_id} ){
      $weighted_number_of_edges += $i;
      $number_of_edges++;
    }
  }

  ## The '-1' is to offset the indexing.
  ## TODO: Is 0 , 100, -100 OK for unconnected objects?
  my $barycenter = $self->{BC_CONST};
  if( $number_of_edges > 0 ){
    $barycenter = ( $weighted_number_of_edges / $number_of_edges ) -1.0;
  }
  $self->{LOG}->debug('Subject ('.$subject_id.') barycenter is: '. $barycenter);
  return $barycenter;
}


#
sub reordered_object_partition {
  my $self = shift;
  return $self->{object_vector};
}
sub reordered_subject_partition {
  my $self = shift;
  return $self->{subject_vector};
}


## Display the stored matrix.
sub bdump {

  my $self = shift;

  #print('o:' . $self->{object_vector});
  #print('s:' . $self->{subject_vector});

  $self->{LOG}->debug("Dumping barymatrix...");

  ## Print top row.
  my $top_queue = [];
  for( my $i = 0; $i < scalar(@{$self->{subject_vector}}); $i++ ){
    push @$top_queue, _vector_id_at_index($self->{subject_vector} , $i);
  }
  my $string = join "\t", @$top_queue;
  $self->{LOG}->debug("o\\s\t" . $string );

  ## Print remainder.
  for( my $j = 0; $j < scalar(@{$self->{object_vector}}); $j++ ){

    my $queue = [];
    my $ov_id_j = _vector_id_at_index($self->{object_vector} , $j);
    push @$queue, $ov_id_j;

    #$self->{LOG}->debug("\t" . $ov_id_j);

    for( my $k = 0; $k < scalar(@{$self->{subject_vector}}); $k++ ){

      my $sv_id_k = _vector_id_at_index($self->{subject_vector} , $k);

      #$self->{LOG}->debug("(o: " . $ov_id_j . ", s: " . $sv_id_k . ")");
      #$self->{LOG}->debug("(j: " . $j . " k: " . $k . ")");

      ##
      if( defined $self->{relation_matrix}{$ov_id_j} &&
	  defined $self->{relation_matrix}{$ov_id_j}{$sv_id_k} ){
	push @$queue, '(1)';
      }else{
	push @$queue, '(0)';
      }
    }
    $self->{LOG}->debug(join "\t", @$queue);
  }
}


##
sub _vector_id_at_index {

  my $vector_ref = shift || undef;
  my $index = shift || 0;

  my $thing_obj = $$vector_ref[$index];
  my $thing_obj_id = $thing_obj->{id};

  return $thing_obj_id;
}


##
sub _permutation_check {

  my $self = shift;

  my $a = shift || [];
  my $b = shift || [];

  die "non-matching array size"
    if scalar(@$a) != scalar(@$b);

  for( my $i = 0; $i < scalar(@$a); $i++ ){
    my $oa = $$a[$i];
    my $ob = $$b[$i];
    my $oa_id = $oa->{id};
    my $ob_id = $ob->{id};

    if( $oa_id ne $ob_id ){
      $self->{LOG}->debug("Permutation found: slot $i from $oa_id to $ob_id");
      sleep 1;
      last;
    }
  }
}



1;
