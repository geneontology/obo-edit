=head1 GODBModel::Schema::Homolset

I believe that this one is done correctly.

=cut

use utf8;
use strict;

package GODBModel::Schema::Homolset;

## TODO: Make sure that GODBModel
#use base ("GODBModel");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('homolset');

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
			 {
			  data_type => 'varchar',
			  size      => 128,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 dbxref_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			 target_gene_product_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			 taxon_id =>
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
			 description =>
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

__PACKAGE__->belongs_to('gene_product' =>
                        'GODBModel::Schema::GeneProduct',
			'target_gene_product_id');
__PACKAGE__->belongs_to('dbxref' =>
                        'GODBModel::Schema::DBXRef',
			'dbxref_id');
__PACKAGE__->has_many('gene_product_homolset' =>
		      'GODBModel::Schema::GeneProductHomolset', 'homolset_id');

__PACKAGE__->add_unique_constraint("dbxref_id", ["dbxref_id"]);
#__PACKAGE__->add_unique_constraint("h0", ["id"]);

1;
