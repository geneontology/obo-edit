=head1 GODBModel::Schema


=cut

use utf8;
use strict;

package GODBModel::Schema;


## TODO: Make sure that GODBModel
#use base ("GODBModel");
use base qw/DBIx::Class::Schema/;

## Get all of the classes in our namespace automatically.
## BUG: for some reason, the below autoloading wasn't working.
#__PACKAGE__->load_classes;
__PACKAGE__->load_classes( qw/
			       Association
			       DBXRef
			       Species
			       GraphPath
			       DB
			       InstanceData
			       Homolset

			       Seq
			       SeqDBXRef

			       GeneProduct
			       GeneProductHomolset
			       GeneProductSynonym
			       GeneProductSeq

			       Evidence
			       EvidenceDBXRef

			       Term
			       Term2Term
			       TermSynonym
			       TermDBXRef
			       GeneProductCount
			     / );

# =item new
# Constructor.
# =cut
# sub new {
#   ##
#   my $class = shift;
#   my $self  = $class->SUPER::new();
#   ## We'll borrow SUCCESS and ERROR_MESSAGE from GODBModel.
#   bless $self, $class;
#   return $self;
# }



1;
