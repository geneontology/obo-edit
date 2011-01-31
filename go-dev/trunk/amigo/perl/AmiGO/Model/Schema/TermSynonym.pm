=head1 AmiGO::Model::Schema::TermSynonym


=cut

use utf8;
use strict;

package AmiGO::Model::Schema::TermSynonym;

## TODO: Make sure that AmiGO
#use base ("AmiGO");
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
			'AmiGO::Model::Schema::Term', 'term_id');
__PACKAGE__->belongs_to('synonym_type' =>
			'AmiGO::Model::Schema::Term', 'synonym_type_id');

##
#__PACKAGE__->add_unique_constraint("t0", ["id"]);
#__PACKAGE__->add_unique_constraint("acc", ["acc"]);


1;
