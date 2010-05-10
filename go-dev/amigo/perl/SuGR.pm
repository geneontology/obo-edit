=head1 SuGR

...

=cut

## Try some loggin fun.
package SuGR;

use strict;
use utf8;

use Log::Log4perl qw(get_logger :levels); #qw(:easy);
Log::Log4perl->easy_init(
# 			 {
# 			  #file  => ">> eraseme.error.log",
# 			  #file  => "> eraseme.error.log",
# 			  file  => "STDERR",
# 			  level => $ERROR,
# 			 },
			 {
			  file  => "STDERR",
			  level => $DEBUG,
			 },
			);

# ###
# ###
# ###

# =item new

# Constr.

# =cut
# sub new {

#   my $class = shift;
#   my $self = {
# 	     };

#   bless $self, $class;
#   return $self;
# }



1;
