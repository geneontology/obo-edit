=head1 GOBO::DBIC::GODBModel::Schema::GeneProductPhylotree


=cut

use utf8;
use strict;

package GOBO::DBIC::GODBModel::Schema::GeneProductPhylotree;

##
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('gene_product_phylotree');

__PACKAGE__->add_columns(
			 id =>
			 {
			  accessor  => 'id', #overrides default of 'id' (irony)
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 1,
			  default_value => undef,
			 },
			 gene_product_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 phylotree_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			);

##
__PACKAGE__->set_primary_key('id');

__PACKAGE__->belongs_to('gene_product' =>
                        'GOBO::DBIC::GODBModel::Schema::GeneProduct', 'gene_product_id');
__PACKAGE__->belongs_to('phylotree' =>
                        'GOBO::DBIC::GODBModel::Schema::Phylotree', 'phylotree_id');

## Allow many_to_many from Phylotree.

__PACKAGE__->has_many
  ('association' => 'GOBO::DBIC::GODBModel::Schema::Association',
   { 'foreign.gene_product_id' => 'self.gene_product_id' },
   { join_type => 'right' }  ); # get's rid of NULLs?


1;
