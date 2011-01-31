=head1 SuGR::Graph

Graph interface. Use your favorite graph system with SuGR!

TODO: should be implementing a wrapper for Graph::Directed so that we
can make sure that we can contain edge labels.

=cut

package SuGR::Graph;

use base 'SuGR';


# =item new

# Constructor.

# =cut
# sub new {

#   ## SHIFT IN (graph, rel)

#   my $class = shift;
#   my $self = {
# 	      LOG => Log::Log4perl->get_logger('SuGR::Graph'),
# 	     };

#   bless $self, $class;
#   return $self;
# }


## Return an array ref of
sub get_roots { die "SuGR::Graph::get_roots has no method $!"; }

