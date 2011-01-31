=head1 AmiGO::Cache::Species

A library to manage and access the species cache.
Includes creation and pre-determined grabbing.

This cache subclass is partially a testbed for other subclasses.

=cut

package AmiGO::Cache::Species;

use base 'AmiGO::Cache';
use GOBO::DBIC::GODBModel::Query;


=item new

Args: int: how many species do we want in there?
Returns:

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new('species');

  ## How many of the possible thousands?
  $self->{ACS_LIMIT} = shift || 50;

  bless $self, $class;
  return $self;
}


=item build

Args:
Returns:

=cut
## BUG/NOTE/TODO: This may actually be very very dangerous (thinking
## about threading in SQLite3 here)...
sub build {

  my $self = shift;

  ## TODO: should there also be a probe method?
  ## Wipe.
  $self->initialize();

  ## Get all of the interesting data concerning species, including
  ## count...
  my $q = GOBO::DBIC::GODBModel::Query->new({type=>'species'});
  my $results =
    $q->{SCHEMA}->resultset('Species')->search({},
					       {
						select =>
						## Apparently, this is
						## bad, and the one
						## below is good.
						# ['me.*',
						['me.id',
						 'me.ncbi_taxa_id',
						 'me.common_name',
						 'me.lineage_string',
						 'me.genus',
						 'me.species',
						 'me.parent_id',
						 'me.left_value',
						 'me.right_value',
						 'me.taxonomic_rank',
						 \'COUNT(gene_product.id) as c'],
						join => ['gene_product'],
						order_by => ['c desc'],
						page => 1,
						rows => $self->{ACS_LIMIT},
						group_by => 'me.id',
					       }
					      );
  my @res = $results->all;

  ##
  $self->open();

  ## Connect/create database and load schema.
  my $schema = qq{
 CREATE TABLE species (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    ncbi_taxa_id INTEGER NOT NULL,
    species VARCHAR2(128),
    genus VARCHAR2(128)
 )
};
  $self->{CACHE_DBH}->do( $schema )
    or die $self->{CACHE_DBH}->errstr;

  ## Insert the data into the db.
  my $sth = $self->{CACHE_DBH}->prepare('INSERT INTO species (ncbi_taxa_id, species, genus) VALUES (?,?,?)')
    or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  my $i = 0;
  foreach my $spc (@res){

    ##
    my $ncbi_taxa_id = $spc->ncbi_taxa_id
      || die "Gotta at least have an ncbi_taxa_id: $!";
    my $species = $spc->species || '';
    my $genus = $spc->genus || '';
    $sth->execute($ncbi_taxa_id, $species, $genus)
      or die "Couldn't execute statement: " . $sth->errstr;

    ##
    $i++;
    last if $i >= $self->{ACS_LIMIT};
  }

  ## Wrap up.
  $sth->finish;
  undef $sth;
  $self->close();
}


=item test

BUG/TODO: should check to see if things are alright.

=cut
sub test { return 1; }



1;
