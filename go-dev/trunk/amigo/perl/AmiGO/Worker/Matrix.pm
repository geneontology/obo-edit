=head1 AmiGO::Worker::Matrix

Generates the n-axis species table.

=cut

use utf8;
#use diagnostics;
use strict;

package AmiGO::Worker::Matrix;

use base ("AmiGO");

use GOBO::DBIC::GODBModel::Schema;
#use GOBO::DBIC::GODBModel::Graph;


=item new

Constructor.

Args: spec_taxa_id, aref of term ids
Returns: |terms|x|terms| matrix of string values

=cut
sub new {

  ##
  my $class = shift;
  my $self = $class->SUPER::new();

  my $spec = shift || '';
  my $terms = shift || [];
  my $graph_type = shift || 'all';

  ###
  ### Classy things.
  ###

  ##
  $self->{MATRIX_INFO} = {};

  ###
  ###
  ###

  ## Answer and term information caching.
  my $answer_cache = {};

  ### Make it something a little more easy to use.
  if( ! $spec || ! scalar(@$terms) || scalar(@$terms) < 2 ){
    die "Wrong number of arguments to make_matrix: $!";
  }

  foreach my $row (@$terms){

    foreach my $col (@$terms){

      ###
      ### Calculate row.
      ###

      ## Old answer check first.
      if( defined $answer_cache->{$row} &&
	  defined $answer_cache->{$row}{$col} ){
      }else{

	## Generate new answer.
	## TODO/BUG: Add the ability to toggle 'regulates' here on and
	## off once the database supports it.
	my $matrix_results = undef;
	if( $graph_type eq 'change_me_later' ){
	  $matrix_results = [];
	}else{
	  my $mquery =
	    GOBO::DBIC::GODBModel::Query->new({type => 'gene_product_two_terms'});
	  $matrix_results =
	    $mquery->get_all_results({-and=>[
					     {'species.ncbi_taxa_id' => $spec},
					     {'object.acc' => $row},
					     {'object_2.acc' => $col}
					    ]});
	}

	## Cache gp information.
	my $gp_cache = [];
	foreach my $gp (@$matrix_results){
	  push @$gp_cache,
	    $gp->dbxref->xref_dbname. ':' . $gp->dbxref->xref_key;
	}

	## Cache new answer.
	$answer_cache->{$row} = {} if ! defined $answer_cache->{$row};
	$answer_cache->{$row}{$col}{gene_products} = $gp_cache;
	$answer_cache->{$row}{$col}{link} = undef;

	## Symmetric.
	$answer_cache->{$col} = {} if ! defined $answer_cache->{$col};
	$answer_cache->{$col}{$row} = {};
	$answer_cache->{$col}{$row}{gene_products} = $gp_cache;
	$answer_cache->{$col}{$row}{link} = undef;

	if( scalar( @{$answer_cache->{$col}{$row}{gene_products}} ) ){
	  my $linky =
	    $self->get_interlink({mode=>'gp-assoc',
				  arg=>{gps=>
					$answer_cache->{$row}{$col}{gene_products}
				       }});
	  $answer_cache->{$col}{$row}{link} = $linky;
	  $answer_cache->{$row}{$col}{link} = $linky;
	}
      }
    }
  }

  $self->{MATRIX_INFO} = $answer_cache;

  bless $self, $class;
  return $self;
}


=item get_matrix

Args: n/a
Returns: {term_acc_1}{term_acc_2}{gene_products => [ids...], link=>'http://...'}

=cut
sub get_matrix {
  my $self = shift;
  return $self->{MATRIX_INFO} || undef;
}



1;
