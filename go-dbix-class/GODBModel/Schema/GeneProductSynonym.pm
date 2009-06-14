=head1 GODBModel::Schema::GeneProductSynonym


=cut

use utf8;
use strict;

package GODBModel::Schema::GeneProductSynonym;

## TODO: Make sure that GODBModel
#use base ("GODBModel");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('gene_product_synonym');

__PACKAGE__->add_columns(
			 gene_product_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
                         product_synonym =>
                         { data_type => 'varchar',
                           size      => 255,
                           is_nullable => 0,
                           is_auto_increment => 0,
                           default_value => '',
                         },
			);

##
__PACKAGE__->set_primary_key('gene_product_id');
__PACKAGE__->set_primary_key('product_synonym');

__PACKAGE__->belongs_to('gene_product' =>
                        'GODBModel::Schema::GeneProduct', 'gene_product_id');

#__PACKAGE__->add_unique_constraint("g0", ["id"]);


1;
