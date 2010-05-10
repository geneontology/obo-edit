=head1 AmiGO::Model::Schema::GeneProduct


=cut

use utf8;
use strict;

package AmiGO::Model::Schema::GeneProduct;

## TODO: Make sure that AmiGO
#use base ("AmiGO");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('gene_product');

__PACKAGE__->add_columns(
			 id =>
			 {
			  accessor  => 'id',#overrides default of 'id' (irony)
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 1,
			  default_value => undef,
			 },
			 symbol =>
			 { data_type => 'varchar',
			   size      => 128,
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
			 species_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			 type_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			 full_name =>
			 {
			  data_type => 'text',
			  size      => 65535,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			);

##
__PACKAGE__->set_primary_key('id');

##
__PACKAGE__->has_many('association' =>
		      'AmiGO::Model::Schema::Association',
		      'gene_product_id');
## BUG/TODO: Used in the simple coannoation. Should eventually be
## replaced by the general one below.
__PACKAGE__->has_many('association_aux' =>
		      'AmiGO::Model::Schema::Association',
		      'gene_product_id');
## Used in higher axes for coannotation. Uses this counter as shared
## (BUG?) information with Query.pm (or its subclasses) in some cases.
__PACKAGE__->has_many('association_aux_1' =>
		      'AmiGO::Model::Schema::Association',
		      'gene_product_id');
__PACKAGE__->has_many('association_aux_2' =>
		      'AmiGO::Model::Schema::Association',
		      'gene_product_id');
__PACKAGE__->has_many('association_aux_3' =>
		      'AmiGO::Model::Schema::Association',
		      'gene_product_id');
__PACKAGE__->has_many('association_aux_4' =>
		      'AmiGO::Model::Schema::Association',
		      'gene_product_id');
##
__PACKAGE__->has_many('gene_product_seq' =>
		      'AmiGO::Model::Schema::GeneProductSeq',
		      'gene_product_id');
__PACKAGE__->has_many('gene_product_synonym' =>
		      'AmiGO::Model::Schema::GeneProductSynonym',
		      'gene_product_id');
__PACKAGE__->has_many('gene_product_homolset' =>
		      'AmiGO::Model::Schema::GeneProductHomolset',
		      'gene_product_id');
__PACKAGE__->belongs_to('dbxref' =>
			'AmiGO::Model::Schema::DBXRef',
			'dbxref_id');
__PACKAGE__->belongs_to('species' =>
			'AmiGO::Model::Schema::Species',
			'species_id');

__PACKAGE__->belongs_to('type' =>
			'AmiGO::Model::Schema::Term',
			'type_id');



##
__PACKAGE__->add_unique_constraint("dbxref_id", ["dbxref_id"]);
__PACKAGE__->add_unique_constraint("g0", ["id"]);




1;
