=head1 GODBModel::Schema::Seq


=cut

use utf8;
use strict;

package GODBModel::Schema::Seq;

## TODO: Make sure that GODBModel
#use base ("GODBModel");
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
 		      'GODBModel::Schema::SeqDBXRef',
 		      'dbxref_id');
__PACKAGE__->has_many('gene_product_seq' =>
 		      'GODBModel::Schema::GeneProductSeq',
 		      'seq_id');
# __PACKAGE__->has_many('gene_product_synonym' =>
# 		      'GODBModel::Schema::GeneProductSynonym',
# 		      'gene_product_id');
# __PACKAGE__->might_have('gene_product_homolset' =>
# 		      'GODBModel::Schema::GeneProductHomolset',
# 		      'gene_product_id');
# __PACKAGE__->belongs_to('dbxref' =>
# 			'GODBModel::Schema::DBXRef',
# 			'dbxref_id');
# __PACKAGE__->belongs_to('species' =>
# 			'GODBModel::Schema::Species',
# 			'species_id');

##
#__PACKAGE__->add_unique_constraint("dbxref_id", ["dbxref_id"]);
#__PACKAGE__->add_unique_constraint("g0", ["id"]);



1;
