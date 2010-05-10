=head1 AmiGO::Model::Schema::Term


=cut

use utf8;
use strict;

package AmiGO::Model::Schema::Term;

## TODO: Make sure that AmiGO
#use base ("AmiGO");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core
				 +AmiGO::Model::Extension /);

__PACKAGE__->table('term');

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
			 name =>
			 {
			  data_type => 'varchar',
			  size      => 255,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 term_type =>
			 {
			  data_type => 'varchar',
			  size      => 55,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 acc =>
			 {
			  data_type => 'varchar',
			  size      => 255,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 is_root =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => 0,
			 },
			 is_obsolete =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => 0,
			 },
			);

##
__PACKAGE__->set_primary_key('id');

##
__PACKAGE__->has_many('association' =>
		      'AmiGO::Model::Schema::Association', 'term_id');
__PACKAGE__->has_many('term_dbxref' =>
		      'AmiGO::Model::Schema::TermDBXRef', 'term_id');
__PACKAGE__->has_many('term_synonym' =>
		      'AmiGO::Model::Schema::TermSynonym', 'term_id');
__PACKAGE__->has_many('gene_product_count' =>
		      'AmiGO::Model::Schema::GeneProductCount', 'term_id');

## term_definition
__PACKAGE__->might_have('term_definition' =>
			'AmiGO::Model::Schema::TermDefinition',
			{'foreign.term_id' => 'self.id'});

## term_subset
__PACKAGE__->has_many('subset' =>
		      'AmiGO::Model::Schema::TermSubset', 'term_id');

## term2term
__PACKAGE__->has_many('parent_relations' =>
		      'AmiGO::Model::Schema::Term2Term', 'term2_id');
__PACKAGE__->has_many('child_relations' =>
		      'AmiGO::Model::Schema::Term2Term', 'term1_id');

## graph_path
__PACKAGE__->has_many('ancestors' =>
		      'AmiGO::Model::Schema::GraphPath', 'term2_id');
__PACKAGE__->has_many('descendents' =>
		      'AmiGO::Model::Schema::GraphPath', 'term1_id');

## Direct GP bridge--not apparently a "real" relationship (e.g. can't
## be used for templates).
__PACKAGE__->many_to_many('gene_products' => 'association', 'gene_product');

##
__PACKAGE__->add_unique_constraint("t0", ["id"]);
__PACKAGE__->add_unique_constraint("acc", ["acc"]);


1;
