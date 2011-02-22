=head1 AmiGO::Model::Schema::GraphPath


=cut

use utf8;
use strict;

package AmiGO::Model::Schema::GraphPath;

## TODO: Make sure that AmiGO
#use base ("AmiGO");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('graph_path');

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
			 term1_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 term2_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 relationship_type_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 distance =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			 relation_distance =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => '',
			 },
			);

##
__PACKAGE__->set_primary_key('id');

__PACKAGE__->belongs_to('term1' =>
			'AmiGO::Model::Schema::Term', 'term1_id');
__PACKAGE__->belongs_to('term2' =>
			'AmiGO::Model::Schema::Term', 'term2_id');
__PACKAGE__->belongs_to('object' =>
			'AmiGO::Model::Schema::Term', 'term1_id');
__PACKAGE__->belongs_to('object_aux' =>
			'AmiGO::Model::Schema::Term', 'term1_id');
## NOTE/TODO: this is an experiment for making axes easier in
## coannotation cases.
__PACKAGE__->belongs_to('object_aux_1' =>
			'AmiGO::Model::Schema::Term', 'term1_id');
__PACKAGE__->belongs_to('object_aux_2' =>
			'AmiGO::Model::Schema::Term', 'term1_id');
__PACKAGE__->belongs_to('object_aux_3' =>
			'AmiGO::Model::Schema::Term', 'term1_id');
__PACKAGE__->belongs_to('object_aux_4' =>
			'AmiGO::Model::Schema::Term', 'term1_id');
##
__PACKAGE__->belongs_to('subject' =>
			'AmiGO::Model::Schema::Term', 'term2_id');
__PACKAGE__->belongs_to('graph_object' =>
			'AmiGO::Model::Schema::Term', 'term1_id');
__PACKAGE__->belongs_to('graph_subject' =>
			'AmiGO::Model::Schema::Term', 'term2_id');
__PACKAGE__->belongs_to('relationship_type' =>
			'AmiGO::Model::Schema::Term', 'relationship_type_id');
__PACKAGE__->belongs_to('rel_type' =>
			'AmiGO::Model::Schema::Term', 'relationship_type_id');

##
#__PACKAGE__->add_unique_constraint("t0", ["id"]);
#__PACKAGE__->add_unique_constraint("acc", ["acc"]);


1;
