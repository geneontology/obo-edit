=head1 GOBO::DBIC::GODBModel::Schema::PhylotreeProperty


=cut

use utf8;
use strict;

package GOBO::DBIC::GODBModel::Schema::PhylotreeProperty;

## TODO: Make sure that GOBO::DBIC::GODBModel
#use base ("GOBO::DBIC::GODBModel");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('phylotree_property');

__PACKAGE__->add_columns(
			 id =>
			 {
			  accessor  => 'id',
			  data_type => 'integer',
			  size      => 20,
			  is_nullable => 0,
			  is_auto_increment => 1,
			  default_value => undef,
			 },
			 phylotree_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			 property_key =>
			 { data_type => 'varchar',
			   size      => 64,
			   is_nullable => 0,
			   is_auto_increment => 0,
			   default_value => undef,
			 },
                         property_val =>
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
__PACKAGE__->belongs_to('phylotree' =>
			'GOBO::DBIC::GODBModel::Schema::Phylotree',
			'phylotree_id');



1;
