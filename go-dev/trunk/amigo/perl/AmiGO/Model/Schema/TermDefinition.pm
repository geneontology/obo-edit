=head1 AmiGO::Model::Schema::TermDefinition


=cut

use utf8;
use strict;

package AmiGO::Model::Schema::TermDefinition;

## TODO: Make sure that AmiGO
#use base ("AmiGO");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('term_definition');

__PACKAGE__->add_columns(
			 term_id =>
			 {
			  data_type => 'integer',
			  size      => 11,
			  is_nullable => 0,
			  is_auto_increment => 0,
			  # default_value => '',
			 },
                         term_definition =>
                         {
			  data_type => 'text',
			  # size      => 255,
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
                         term_comment =>
                         {
			  data_type => 'meduimtext',
			  # size      => 255,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
                         },
                         reference =>
                         {
			  data_type => 'varchar',
			  size      => 255,
			  is_nullable => 1,
			  is_auto_increment => 0,
			  default_value => undef,
                         },
			);

## BUG/TODO: this is WRONG! But I haven't been able to figure out yet
## how to make it work just by setting a unique constraint yet...
__PACKAGE__->set_primary_key('term_id');
#__PACKAGE__->add_unique_constraint('term_id', ['term_id']);
#print STDERR ">>>". __PACKAGE__-> ."\n";

__PACKAGE__->belongs_to('term' => 'AmiGO::Model::Schema::Term', 'term_id');



1;
