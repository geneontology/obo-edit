=head1 AmiGO::Model::Schema::Seq


=cut

use utf8;
use strict;

package AmiGO::Model::Schema::Seq;

## TODO: Make sure that AmiGO
#use base ("AmiGO");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('seq');

__PACKAGE__->add_columns(
			 id =>
			 {
			  accessor  => 'id',
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 1,
			  default_value => undef,
			 },
			 display_id =>
			 {
			  data_type => 'varchar',
			  size      => 64,
			  is_nullable => 1,
			  default_value => undef,
			 },
			 description =>
			 {
			  data_type => 'varchar',
			  size      => 255,
			  is_nullable => 1,
			  default_value => undef,
			 },
			 seq =>
			 {
			  data_type => 'mediumtext',
			  is_nullable => 0,
			  default_value => undef,
			 },
			 seq_len =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			 md5checksum =>
			 {
			  data_type => 'varchar',
			  size      => 32,
			  is_nullable => 1,
			  default_value => undef,
			 },
			 moltype =>
			 {
			  data_type => 'varchar',
			  size      => 25,
			  is_nullable => 1,
			  default_value => undef,
			 },
			 timestamp =>
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

##
__PACKAGE__->has_many('seq_dbxref' =>
 		      'AmiGO::Model::Schema::SeqDBXRef',
 		      'dbxref_id');
__PACKAGE__->has_many('gene_product_seq' =>
 		      'AmiGO::Model::Schema::GeneProductSeq',
 		      'seq_id');
# __PACKAGE__->has_many('gene_product_synonym' =>
# 		      'AmiGO::Model::Schema::GeneProductSynonym',
# 		      'gene_product_id');
# __PACKAGE__->might_have('gene_product_homolset' =>
# 		      'AmiGO::Model::Schema::GeneProductHomolset',
# 		      'gene_product_id');
# __PACKAGE__->belongs_to('dbxref' =>
# 			'AmiGO::Model::Schema::DBXRef',
# 			'dbxref_id');
# __PACKAGE__->belongs_to('species' =>
# 			'AmiGO::Model::Schema::Species',
# 			'species_id');

##
#__PACKAGE__->add_unique_constraint("dbxref_id", ["dbxref_id"]);
#__PACKAGE__->add_unique_constraint("g0", ["id"]);



1;
