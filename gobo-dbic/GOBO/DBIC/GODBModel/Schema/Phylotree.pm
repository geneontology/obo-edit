=head1 GOBO::DBIC::GODBModel::Schema::Phylotree


=cut

use utf8;
use strict;

package GOBO::DBIC::GODBModel::Schema::Phylotree;

##
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core
				 +GOBO::DBIC::GODBModel::Extension /);

__PACKAGE__->table('phylotree');

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
			 name =>
			 {
			  data_type => 'varchar',
			  size      => 255,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 dbxref_id =>
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

##
__PACKAGE__->might_have('gene_product_phylotree' =>
			'GOBO::DBIC::GODBModel::Schema::GeneProductPhylotree',
			'phylotree_id');
__PACKAGE__->many_to_many(gene_products => 'gene_product_phylotree', 'gene_product');

##
__PACKAGE__->belongs_to('dbxref' =>
			'GOBO::DBIC::GODBModel::Schema::DBXRef', 'dbxref_id');

##
__PACKAGE__->add_unique_constraint("i0", ["id"]);



1;
