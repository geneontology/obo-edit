=head1 AmiGO::Cache

A library to manage and access the produced caches (the subclasses).

=cut

package AmiGO::Cache;

use base 'AmiGO';
use DBD::SQLite;


# ## Signifier to path translation.
my $sig2name = {};
# my $sig2name =
#   {
#    'rg' =>
#    'rg.db',
#    'gonavi' =>
#    'gonavi.db',
#   };


=item new

Args:
Returns:

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  ## Location.
  my $sig = shift || die 'gotta have an arg here ' . "$!";
  $cname = $sig . '.db';
  if( defined $sig2name->{$sig} ){ $cname = $sig2name->{$sig}; }
  $self->{CACHE_LOCATION} = $self->amigo_env('AMIGO_CACHE_DIR') . '/'.  $cname;

  ## If it isn't there, create and connect.
  _create_db($self->{CACHE_LOCATION});

  ##
  $self->{CACHE_DBH} = undef;

  bless $self, $class;
  return $self;
}


## If it isn't there, create/connnect, then disconnect.
sub _create_db {
  my $name = shift || die "need name: $!";
  if( ! -f $name ){
    my $dbh =
      DBI->connect('dbi:SQLite:dbname=' . $name, '','',
		   { RaiseError => 1, })
	|| die "Database connection not made: $DBI::errstr";
    $dbh->disconnect;
  }
}


## Attempt to give the db permissive permissions.
sub _permissive_db {
  my $name = shift || die "need name: $!";
  if( -f $name ){
    my $mode = 0777;
    eval {
      chmod $mode, $name;
    };
  }
}


=item open

Args:
Returns:

=cut
sub open {
  my $self = shift;
  if( ! defined $self->{CACHE_DBH} ){
    $self->{CACHE_DBH} =
      DBI->connect('dbi:SQLite:dbname=' . $self->{CACHE_LOCATION},
		   '','',
		   { RaiseError => 1, })
	|| die "Database connection not made: $DBI::errstr";

  }
  return $self->{CACHE_DBH};
}


=item close

Args:
Returns:

=cut
sub close {
  my $self = shift;
  if( defined $self->{CACHE_DBH} ){
    $self->{CACHE_DBH}->disconnect();
    $self->{CACHE_DBH} = undef;
  }
}


=item destroy

Args:
Returns:

=cut
sub destroy {

  my $self = shift;

  $self->close();
  unlink $self->{CACHE_LOCATION};
}


=item initialize

Args:
Returns:

=cut
sub initialize {

  my $self = shift;

  $self->destroy();
  _create_db($self->{CACHE_LOCATION});
  _permissive_db($self->{CACHE_LOCATION});
}


=item build

This must get overrided in subclasses.
It should add the schema, add data, etc.

=cut
sub build { die "must override build method $!"; }


=item test

This must get overrided in subclasses.
It should check to see if things are alright.

=cut
sub test { die "must override test method $!"; }


## Make sure that we at least drop our connection if we fall out of
## scope or something...
sub DESTROY {
  my $self = shift;
  $self->close();
}



1;
