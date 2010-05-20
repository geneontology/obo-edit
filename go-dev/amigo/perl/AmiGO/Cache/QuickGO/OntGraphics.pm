=head1 AmiGO::Cache::QuickGO::OntGraphics

A library to manage and access the QuickGO ontology image graphics cache.

Note: even after build, this will still be a read-and-write cache.

=cut

package AmiGO::Cache::QuickGO::OntGraphics;

use base 'AmiGO::Cache';


=item new

Args:
Returns:

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new('qgontgraph');

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
  ## Wipe. Connect/create database and load schema.
  $self->initialize();
  $self->open();
  my $schema = qq{
 CREATE TABLE data (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    key VARCHAR2(256),
    image BLOB
 )
};
  $self->{CACHE_DBH}->do( $schema )
    or die $self->{CACHE_DBH}->errstr;
  $self->close();
}


=item cache_data

Args: hashref like:
   key,
   image,
Returns: 0...

=cut
sub cache_data {

  my $self = shift;
  my $args = shift || {};
  my $data_key = $args->{key} || die "gotta have a key $!";
  my $image = $args->{image} || die "gotta have image data $!";

  my $ret = 0;

  ## TODO: check to see if it is already there.

  ##
  $self->open();
  my $sth = $self->{CACHE_DBH}->prepare('INSERT INTO data (key, image) VALUES (?,?)')
    or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  my $undef_p = $sth->execute($data_key, $image)
    or die "Couldn't execute statement: " . $sth->errstr;

  undef $sth;
  $self->close();
  return $ret;
}


=item get_data

Args: unique key
Returns: cached data hashref, or undef if nothing...

=cut
sub get_data {

  my $self = shift;
  my $key = shift || die "gotta have a key $!";

  ##
  my $ret = undef;

  ##
  $self->open();
  my $query = "SELECT * FROM data WHERE key = ?";
  my $sth = $self->{CACHE_DBH}->prepare($query)
    or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  my $undef_p = $sth->execute($key)
    or die "Couldn't execute statement: " . $sth->errstr;

  ## Extract from the results.
  my @row = $sth->fetchrow_array();
  if( $row[0] && $row[1] && $row[2] ){
    $ret =
      {
       id => $row[0],
       key => $row[1],
       image => $row[2]
      };
  }

  undef $sth;
  $self->close();
  return $ret;
}


=item test

BUG/TODO: should check to see if things are alright.

=cut
sub test { return 1; }



1;
