=head1 AmiGO::Cache::ART

A library to manage and access the species cache.
Includes creation and pre-determined grabbing.

Note: even after build, this will still be a read-and-write cache.

Usage:

use AmiGO::Cache::ART;
my $art = AmiGO::Cache::ART->new();
my $req = $art->add_request({data=>"stuff"});
print $req;

=cut

package AmiGO::Cache::ART;

use base 'AmiGO::Cache';


=item new

Args:
Returns:

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new('art');

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
 CREATE TABLE request (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    uuid TEXT,
    date TEXT,
    data TEXT,
    done INTEGER
 )
};
  $self->{CACHE_DBH}->do( $schema )
    or die $self->{CACHE_DBH}->errstr;
  $self->close();
}


=item add_request

Args: 
Returns: next available id

=cut
sub add_request {

  my $self = shift;
  my $args = shift || {};
  my $data = $args->{data} || undef;
  my $retval = undef;

  # Generate date and uuid.
  my $uuid = $self->unique_id();
  my $now = localtime();

  $self->open();

  ## Ummm...this is a transaction in DBI, right?
  $self->{CACHE_DBH}{AutoCommit} = 0;
  $self->{CACHE_DBH}{RaiseError} = 1;
  eval {

    ## Prep.
    my $ins_h = $self->{CACHE_DBH}->prepare('INSERT INTO request (uuid, date, data, done) VALUES (?,?,?,?)')
      or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;
    my $sel_h = $self->{CACHE_DBH}->prepare('SELECT * FROM request ORDER BY request.id DESC LIMIT 1')
      or die "Couldn't prepare statement: " . $self->{CACHE_DBH}->errstr;

    my $ins_undef_p = $ins_h->execute($uuid, $now, $data, 0)
      or die "Couldn't execute statement: " . $sth->errstr;
    my $sel_undef_p = $sel_h->execute()
      or die "Couldn't execute statement: " . $sth->errstr;

    ## Generate a: 9000000 <= unique id <= 10000000.
    my @row = $sel_h->fetchrow_array();
    if( $row[0] ){
      my $new_id = 9000000 + int($row[0]);
      if( $new_id < 10000000 ){
	$retval = $new_id;
      }else{
	$self->kvetch('AmiGO::Cache::ART: FAIL: over: ' . $new_id);
      }
    }else{
      $self->kvetch('AmiGO::Cache::ART: FAIL: select: ' . $data);
    }

    $self->{CACHE_DBH}->commit();
  };
  if($@){
    $self->kvetch('AmiGO::Cache::ART: FAIL: rollback: ' . $@);
    $self->{CACHE_DBH}->rollback();
  }

  ## Close out.
  undef $ins_h;
  undef $sel_h;
  $self->close();

  return $retval;
}


=item test

BUG/TODO: should check to see if things are alright.

=cut
sub test { return 1; }



1;
