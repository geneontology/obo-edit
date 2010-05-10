=head1 AmiGO::Model::Schema::TermSubset


=cut


package AmiGO::Model::Schema::TermSubset;

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

__PACKAGE__->belongs_to('term' => 'AmiGO::Model::Schema::Term', 'term_id');
__PACKAGE__->belongs_to('subset' => 'AmiGO::Model::Schema::Term', 'subset_id');

##



1;
