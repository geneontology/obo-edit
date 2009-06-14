=head1 GODBModel::Schema::DBXRef

I believe that this one is done correctly.

=cut

use utf8;
use strict;

package GODBModel::Schema::DBXRef;

## TODO: Make sure that GODBModel
#use base ("GODBModel");
use base qw/DBIx::Class/;

##
__PACKAGE__->load_components(qw/ PK::Auto Core /);

__PACKAGE__->table('dbxref');

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
			 xref_dbname =>
			 { data_type => 'varchar',
			   size      => 55,
			   is_nullable => 0,
			   is_auto_increment => 0,
			   default_value => '',
			 },
			 xref_key =>
			 { data_type => 'varchar',
			   size      => 255,
			   is_nullable => 0,
			   is_auto_increment => 0,
			   default_value => '',
			 },
			 xref_keytype =>
			 { data_type => 'varchar',
			   size      => 32,
			   is_nullable => 1,
			   is_auto_increment => 0,
			   default_value => undef,
			 },
			 xref_desc =>
			 { data_type => 'varchar',
			   size      => 255,
			   is_nullable => 1,
			   is_auto_increment => 0,
			   default_value => undef,
			 },
			);

##
__PACKAGE__->set_primary_key('id');

__PACKAGE__->has_one('gene_product' =>
		     'GODBModel::Schema::GeneProduct',
		     'dbxref_id');
__PACKAGE__->has_one('evidence_dbxref' =>
		     'GODBModel::Schema::EvidenceDBXRef',
		     'dbxref_id');
__PACKAGE__->has_one('term_dbxref' =>
		     'GODBModel::Schema::TermDBXRef',
		     'dbxref_id');
__PACKAGE__->has_one('seq_dbxref' =>
		     'GODBModel::Schema::SeqDBXRef',
		     'dbxref_id');
__PACKAGE__->has_one('homolset' =>
		     'GODBModel::Schema::Homolset',
		     'dbxref_id');
## DEPRECATED: This is just here temporarily to enable a hack in
## HomolsetEvidence.pm.
__PACKAGE__->has_one('seq_hack' =>
		     'GODBModel::Schema::Seq',
		     {'foreign.display_id' => 'self.xref_key'});

#__PACKAGE__->add_unique_constraint("g0", ["id"]);



1;
