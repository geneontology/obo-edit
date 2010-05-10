=head1 AmiGO::Model::Schema::GeneProductHomolset


=cut

use utf8;
use strict;

package AmiGO::Model::Schema::GeneProductHomolset;

## TODO: Make sure that AmiGO
#use base ("AmiGO");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('gene_product_homolset');

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
			 gene_product_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 homolset_id =>
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
                        'AmiGO::Model::Schema::GeneProduct', 'gene_product_id');
__PACKAGE__->belongs_to('homolset' =>
                        'AmiGO::Model::Schema::Homolset', 'homolset_id');

#__PACKAGE__->add_unique_constraint("g0", ["id"]);


1;
