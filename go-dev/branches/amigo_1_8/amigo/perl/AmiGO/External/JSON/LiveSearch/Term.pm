=head1 AmiGO::External::JSON::LiveSearch::Term

use AmiGO::External::JSON::LiveSearch::Term;
$a = AmiGO::External::JSON::LiveSearch::Term->new();
$b = $a->query({query=>"pigment"});
print keys %$b

=cut

package AmiGO::External::JSON::LiveSearch::Term;

use base ("AmiGO::External::JSON::LiveSearch");


=item new

Just arg to superclass.

=cut
sub new {

  ## 
  my $class = shift;
  my $self  = $class->SUPER::new('term');
  bless $self, $class;
  return $self;
}



1;
