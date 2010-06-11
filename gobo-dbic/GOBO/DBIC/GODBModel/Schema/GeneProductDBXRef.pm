=head1 GOBO::DBIC::GODBModel::Schema::GeneProductDBXRef


=cut

use utf8;
use strict;

package GOBO::DBIC::GODBModel::Schema::GeneProductDBXRef;

## TODO: Make sure that GOBO::DBIC::GODBModel
#use base ("GOBO::DBIC::GODBModel");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('gene_product_dbxref');

__PACKAGE__->add_columns(
			 gene_product_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			 dbxref_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			);

##
__PACKAGE__->set_primary_key('gene_product_id');
__PACKAGE__->set_primary_key('dbxref_id');

__PACKAGE__->belongs_to('gene_product' =>
                        'GOBO::DBIC::GODBModel::Schema::GeneProduct', 'gene_product_id');
__PACKAGE__->belongs_to('dbxref' =>
                        'GOBO::DBIC::GODBModel::Schema::DBXRef', 'dbxref_id');

#__PACKAGE__->add_unique_constraint("g0", ["id"]);



1;
