=head1 AmiGO::Model::Schema::SeqDBXRef


=cut

use utf8;
use strict;

package AmiGO::Model::Schema::SeqDBXRef;

## TODO: Make sure that AmiGO
#use base ("AmiGO");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('seq_dbxref');

__PACKAGE__->add_columns(
			 seq_id =>
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
			);

##
__PACKAGE__->set_primary_key('seq_id');
__PACKAGE__->set_primary_key('dbxref_id');

__PACKAGE__->belongs_to('seq' =>
                        'AmiGO::Model::Schema::Seq', 'seq_id');
__PACKAGE__->belongs_to('dbxref' =>
                        'AmiGO::Model::Schema::DBXRef', 'dbxref_id');

#__PACKAGE__->add_unique_constraint("g0", ["id"]);



1;
