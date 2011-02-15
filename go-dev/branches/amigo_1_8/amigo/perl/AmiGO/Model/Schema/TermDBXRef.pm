=head1 AmiGO::Model::Schema::TermDBXRef

=cut

use utf8;
use strict;

package AmiGO::Model::Schema::TermDBXRef;

## TODO: Make sure that AmiGO
#use base ("AmiGO");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('term_dbxref');

__PACKAGE__->add_columns(
			 term_id =>
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
			 is_for_definition =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  default_value => 0,
			 },
			);

##
__PACKAGE__->set_primary_key('term_id');
__PACKAGE__->set_primary_key('dbxref_id');
__PACKAGE__->set_primary_key('is_for_definition');

##
__PACKAGE__->belongs_to('term' =>
			'AmiGO::Model::Schema::Term',
			'term_id');
__PACKAGE__->belongs_to('dbxref' =>
			'AmiGO::Model::Schema::DBXRef',
			'dbxref_id');

##
#__PACKAGE__->add_unique_constraint("evidence_id", ["evidence_id"]);
#__PACKAGE__->add_unique_constraint("dbxref_id", ["dbxref_id"]);
#__PACKAGE__->add_unique_constraint("g0", ["id"]);



1;
