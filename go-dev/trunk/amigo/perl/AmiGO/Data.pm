=head1 AmiGO::Data

Mostly super class stuff for data models. However, this is where we
connect to the database and get the raw connection.

=cut
package AmiGO::Data;

use base 'AmiGO';
use utf8;
use strict;
#use DBI;


## BUG: This is a lousy way to do this--there must be something
## better! Also, even if we keep this, it needs to be extended for
## Stanford.
#my $db_handle =
#  DBI->connect("DBI:mysql:$ENV{GO_DBNAME}:$ENV{GO_DBHOST}", "", "");


# sub new {

#   ##
#   my $class = shift;
#   my $self  = $class->SUPER::new();
#   #my $arg = shift || {};

#   ## We'll borrow SUCCES and ERROR_MESSAGE from AmiGO.

#   bless $self, $class;
#   return $self;
# }



1;
