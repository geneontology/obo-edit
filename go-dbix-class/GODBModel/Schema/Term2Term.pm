=head1 GODBModel::Schema::Term2Term


=cut

use utf8;
use strict;

package GODBModel::Schema::Term2Term;

## TODO: Make sure that GODBModel
#use base ("GODBModel");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('term2term');

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
			 relationship_type_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => '',
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
			 complete =>
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

##
#__PACKAGE__->belongs_to('term' =>
#			'GODBModel::Schema::Term', 'term1_id');
#__PACKAGE__->belongs_to('term' =>
#			'GODBModel::Schema::Term', 'term2_id');
__PACKAGE__->belongs_to('term1' =>
			'GODBModel::Schema::Term', 'term1_id');
__PACKAGE__->belongs_to('term2' =>
			'GODBModel::Schema::Term', 'term2_id');
__PACKAGE__->belongs_to('object' =>
			'GODBModel::Schema::Term', 'term1_id');
__PACKAGE__->belongs_to('subject' =>
			'GODBModel::Schema::Term', 'term2_id');
__PACKAGE__->belongs_to('relationship' =>
			'GODBModel::Schema::Term', 'relationship_type_id');
## ...?
__PACKAGE__->belongs_to('graph_path' =>
			'GODBModel::Schema::GraphPath',
			{'foreign.term1_id' => 'self.term1_id',
			 'foreign.term2_id' => 'self.term2_id'});

##
#__PACKAGE__->add_unique_constraint("t0", ["id"]);
#__PACKAGE__->add_unique_constraint("acc", ["acc"]);


1;
