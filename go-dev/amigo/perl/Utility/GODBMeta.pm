=head1 Utility::GODBMeta.pm

Jimmy metadata out of database at the time of installation rather than
at runtime. This should mostly run separately to generate a cache.

=cut
package Utility::GODBMeta;

## Not going to base this on AmiGO since there is no guarantee of a
## vars file--
# use base 'AmiGO';
use utf8;
use strict;
use GOBO::DBIC::GODBModel::Schema;

#use DBI;


=item new

BUG: Needs to be more flexible for real installations.

=cut
sub new {

  ##
  my $class = shift;

  #my $self  = $class->SUPER::new();
  my $self = {};

  ## Init schema (db) connection.
  my $GO_DBNAME = shift || undef;
  my $GO_DBHOST = shift || undef;
  $self->{SCHEMA} =
    GOBO::DBIC::GODBModel::Schema->connect('dbi:mysql:go_latest_lite');

  #   ## We'll borrow SUCCESS and ERROR_MESSAGE from AmiGO.

  bless $self, $class;
  return $self;
}


=item get_release_name

Returns the GODB release name as a string.

=cut
sub get_release_name {
  my $self = shift;
  my $rs = $self->{SCHEMA}->resultset('InstanceData');
  return $rs->first->release_name || 'unknown';
}


=item get_release_type

Returns the GODB release type as a string.

=cut
sub get_release_type {
  my $self = shift;
  my $rs = $self->{SCHEMA}->resultset('InstanceData');
  return $rs->first->release_type || 'unknown';
}


=item get_release_notes

Returns the GODB release notes as a string.

=cut
sub get_release_notes {
  my $self = shift;
  my $rs = $self->{SCHEMA}->resultset('InstanceData');
  return $rs->first->release_notes || 'unknown';
}


=item get_databases

Returns an array ref of strings for the dbs.

=cut
sub get_databases {

  my $self = shift;
  my $rs = $self->{SCHEMA}->resultset('DB');

  my $all_dbs = [];
  while( my $db = $rs->next ){
    push @{$all_dbs}, $db->name;
  }

  return $all_dbs;
}


=item get_top_species

Arguments (optional): int for the number of top species. Default is 10.
Returns an array ref of strings for the top species.

=cut
## Easiest: SELECT species_id, count(*) AS count FROM gene_product GROUP BY species_id ORDER BY count DESC LIMIT 10;
sub get_top_species {

  my $self = shift;
  my $num = shift || 1;

  ## BUG: This is a kludge until I learn all of this stuff better. Not
  ## even sure this is correct...but it is fast!
  my $rs = $self->{SCHEMA}->resultset('GeneProduct')->search(
    undef,
    {
     '+select'   => [ \'COUNT(species_id) AS count' ],
     #'as'       => ['count'],
     'group_by' => ['species_id'],
     order_by => [\'count DESC'],
     #limit    => $num
    }
   );

  my $all_sps = {};
  my $i = 0;
  while( (my $gp = $rs->next) && $i < $num ){
    my $sp = $gp->species;
    #print "---" . $sp->genus . " " . $sp->species . "\n";
    #sleep 2;
    $all_sps->{$sp->ncbi_taxa_id} = $sp->genus . ' ' . $sp->species;
    #push @{$all_sps}, $sp->common_name;
    $i++;
  }

  return $all_sps;
}



1;
