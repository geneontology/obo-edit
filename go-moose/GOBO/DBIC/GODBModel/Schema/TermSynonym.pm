=head1 GODBModel::Schema::TermSynonym


=cut

use utf8;
use strict;

package GODBModel::Schema::TermSynonym;

## TODO: Make sure that GODBModel
#use base ("GODBModel");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('term_synonym');

__PACKAGE__->add_columns(
			 term_id =>
			 {
			  accessor  => 'id',#overrides default of 'id' (irony)
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
                         term_synonym =>
                         {
			  data_type => 'varchar',
			  size      => 996,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
                         },
                         acc_synonym =>
                         {
			  data_type => 'varchar',
			  size      => 255,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
                         },
			 synonym_type_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 synonym_category_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			);

##
#__PACKAGE__->set_primary_key('id');

__PACKAGE__->belongs_to('term' =>
			'GODBModel::Schema::Term', 'term_id');

##
#__PACKAGE__->add_unique_constraint("t0", ["id"]);
#__PACKAGE__->add_unique_constraint("acc", ["acc"]);


1;
