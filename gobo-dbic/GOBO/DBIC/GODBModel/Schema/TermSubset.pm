=head1 GOBO::DBIC::GODBModel::Schema::TermSubset


=cut


package GOBO::DBIC::GODBModel::Schema::TermSubset;

use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('term_subset');

__PACKAGE__->add_columns(
			 term_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			 subset_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => undef,
			 },
			);

##
#__PACKAGE__->set_primary_key('id');

__PACKAGE__->belongs_to('term' => 'GOBO::DBIC::GODBModel::Schema::Term', 'term_id');
__PACKAGE__->belongs_to('subset' => 'GOBO::DBIC::GODBModel::Schema::Term', 'subset_id');

##



1;
