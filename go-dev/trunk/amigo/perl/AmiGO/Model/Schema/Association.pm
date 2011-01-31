=head1 AmiGO::Model::Schema::Association


=cut

use utf8;
use strict;

package AmiGO::Model::Schema::Association;

## TODO: Make sure that AmiGO
#use base ("AmiGO");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('association');

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
			 term_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 gene_product_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 is_not =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			 role_group =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			 assocdate =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			 source_db_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			);

##
__PACKAGE__->set_primary_key('id');

## ...
__PACKAGE__->has_many('evidence' =>
		      'AmiGO::Model::Schema::Evidence',
		      'association_id');
__PACKAGE__->belongs_to('term' =>
			'AmiGO::Model::Schema::Term', 'term_id');
__PACKAGE__->has_many('graph_path_relations' =>
		      'AmiGO::Model::Schema::GraphPath',
		      {'foreign.term2_id' => 'self.term_id'});
##
__PACKAGE__->has_many('graph_path_relations_aux_1' =>
		      'AmiGO::Model::Schema::GraphPath',
		      {'foreign.term2_id' => 'self.term_id'});
__PACKAGE__->has_many('graph_path_relations_aux_2' =>
		      'AmiGO::Model::Schema::GraphPath',
		      {'foreign.term2_id' => 'self.term_id'});
__PACKAGE__->has_many('graph_path_relations_aux_3' =>
		      'AmiGO::Model::Schema::GraphPath',
		      {'foreign.term2_id' => 'self.term_id'});
__PACKAGE__->has_many('graph_path_relations_aux_4' =>
		      'AmiGO::Model::Schema::GraphPath',
		      {'foreign.term2_id' => 'self.term_id'});
##
__PACKAGE__->belongs_to('gene_product' =>
			'AmiGO::Model::Schema::GeneProduct', 'gene_product_id');
__PACKAGE__->belongs_to('db' =>
			'AmiGO::Model::Schema::DB', 'source_db_id');

##
__PACKAGE__->add_unique_constraint("a0", ["id"]);



1;
