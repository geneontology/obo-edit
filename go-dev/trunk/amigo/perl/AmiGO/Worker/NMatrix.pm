=head1 AmiGO::Worker::NMatrix

Generates the n-axis species table.

=cut

use utf8;
use strict;

package AmiGO::Worker::NMatrix;

use base ("AmiGO");

use Data::Dumper;
use Set::Scalar;
use Algorithm::Permute;

use AmiGO::Worker::Intersection;

=item new

Constructor.

Args: spec_taxa_id, aref of term ids
Returns: |terms|x|terms| matrix of string values

=cut
sub new {

  ##
  my $class = shift;
  my $self = $class->SUPER::new();

  my $spec = shift || die "need a species here";
  my $card = shift || die "need an integer cardinality here";
  my $graph_type = shift || 'all';

  ###
  ### Classy things.
  ###

  $self->{MATRIX_SPEC} = $spec;
  $self->{MATRIX_CARD} = $card;
  $self->{MATRIX_GTYPE} = $graph_type;

  ##
  if( $self->{MATRIX_GTYPE} eq 'change_me_later' ){
    die "change later means change later";
  }else{
    $self->{MATRIX_INSECT} =
      AmiGO::Worker::Intersection->new($self->{MATRIX_SPEC},
				       $self->{MATRIX_CARD},
				       $self->{MATRIX_GTYPE});
  }

  ##
  $self->{MATRIX_FOUND_TERMS} = {};
  $self->{MATRIX_FOUND_GPS} = {};

  bless $self, $class;
  return $self;
}


=item get_matrix

Args: n/a
Returns: {term_acc_1}{term_acc_2}{gene_products => [ids...], link=>'http://...'}

=cut
sub get_matrix {

  my $self = shift;
  my $term_sets = shift || [];

  ### Make it something a little more easy to use.
  if( ! $self->{MATRIX_SPEC} ||
      ! scalar(@$term_sets) ||
      scalar(@$term_sets) < 2 ){
    die "Wrong number of arguments to make matrix: $!";
  }

  ## Setify and arrayify the term sets. Cache seen terms as well.
  my @sets = ();
  foreach my $term_set (@$term_sets){
    push @sets, Set::Scalar->new(@$term_set);
    map { $self->{MATRIX_FOUND_TERMS}{$_} = 1 } @$term_set;
  }

  ## Answer and term information caching.
  my $answer_cache = {};

  ## Iterate over the product and collect the intersection information
  ## as we go (cache it yes).
  my $done_cache = {};
  my $iter = Set::Scalar->cartesian_product_iterator(@sets);
  while (my @pset = $iter->()) {

    #$self->kvetch("_pset_: " . Dumper(\@pset));
    my @ordered_set = sort {$a cmp $b} @pset;
    #$self->kvetch("_ordered_set_: " . join ' ', @ordered_set);

    ## Make sure we're not doing more than necessary with repeats.
    my $done_id = join '::', @ordered_set;
    if( defined $done_cache->{$done_id} ){
      ## Seen before.
    }else{

      ## Mark as done.
      $done_cache->{$done_id} = 1;

      ## Retrieve the information.
      my $info = $self->{MATRIX_INSECT}->get_information(\@ordered_set);
      #$self->kvetch("_info_: " . Dumper($info));

      ## Pull the gp accs out of the answer and cache them for later.
      foreach my $gpacc (@{$info->{gene_products}}){
	$self->{MATRIX_FOUND_GPS}{$gpacc} = 1;
      }

      ###
      ### In all of its permutations, stuff it into the answer
      ### cache. Since we know we haven't donoe this general answer
      ### before (see above), we're not going to worry about trivial
      ### dupes (e.g. a key of x x x) as they will be fast to enter
      ### and not occur very often.
      ###

      ## For each permutation.
      my $perm_iter = Algorithm::Permute->new(\@pset);
      while( my @perm = $perm_iter->next() ){
	#$self->kvetch("_perm_: " . join(" ", @perm));

	## For each variable in the permutation, drill in and add to
	## the answer cache.
	my $current = $answer_cache;
	for( my $v = 0;  $v < scalar(@perm); $v++ ){

	  my $var = $perm[$v];
	  if( $v + 1 == scalar(@perm) ){ # last iteration
	    #$self->kvetch("(drill_a lvl " . $v . "): " . $var);
	    $current->{$var} = $info;
	  }elsif( ! defined $current->{$var} ){ # drill down
	    #$self->kvetch("(drill_b lvl " . $v . "): " . $var);
	    $current->{$var} = {};
	    $current = $current->{$var};
	  }else{
	    #$self->kvetch("(drill_c lvl " . $v . "): " . $var);
	    $current = $current->{$var};
	  }
	}
      }
    }
  }

  #$self->kvetch("_answer_cache_: " . Dumper($answer_cache));

  return $answer_cache;
}


=item get_terms

Args: n/a
Returns: a hashref of unique terms (key is acc and value is 1)

=cut
sub get_terms {
  my $self = shift;
  return $self->{MATRIX_FOUND_TERMS};
}


=item get_gene_products

Args: n/a
Returns: a hashref of unique gp acces (key is acc and value is 1)

=cut
sub get_gene_products {
  my $self = shift;
  return $self->{MATRIX_FOUND_GPS};
}



1;
