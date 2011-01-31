=head1 AmiGO::Cache::GONavi

A library to manage and access the GONavi cache.

Note: even after build, this will still be a read-and-write cache.

=cut

package AmiGO::Cache::GONavi;

use base 'AmiGO::Cache';


=item new

Args:
Returns:

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new('gonavi');

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
    meta_data TEXT,
    image BLOB,
    mini_image BLOB
 )
};
  $self->{CACHE_DBH}->do( $schema )
    or die $self->{CACHE_DBH}->errstr;
  $self->close();
}


=item cache_data

Args: hashref like:
   key,
   meta_data,
   image,
   mini_image
Returns: 0...

=cut
sub cache_data {

  my $self = shift;
  my $args = shift || {};
  my $data_key = $args->{key} || die "gotta have a key $!";
  my $meta_data = $args->{meta_data} || die "gotta have meta data $!";
  my $image = $args->{image} || die "gotta have image data $!";
  my $mini_image = $args->{mini_image} || die "gotta have mini image data $!";

  my $ret = 0;

  ## TODO: check to see if it is already there.

  ##
  $self->open();
  my $sth = $self->{CACHE_DBH}->prepare('INSERT INTO data (key, meta_data, image, mini_image) VALUES (?,?,?,?)')
    or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
  my $undef_p = $sth->execute($data_key, $meta_data, $image, $mini_image)
    or die "Couldn't execute statement: " . $sth->errstr;

  undef $sth;
  $self->close();
  return $ret;
}


=item get_data

Args: unique key
Returns: cached data hashref

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
       meta_data => $row[2],
       image => $row[3],
       mini_image => $row[4],
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
