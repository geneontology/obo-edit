=head1 TrivialGraph

Creates trivial graph elements. They are just hashrefs 'cause we'll be
practicing "duck typing" here and there (showing the JavaScript
heritage as well).

=cut

package SuGR::TrivialGraph;

use strict;
use utf8;
use base 'SuGR';


=item new

Constr.

=cut
sub new {

  my $class = shift;

  my $self = {
	      LOG => Log::Log4perl->get_logger('SuGR::TrivialGraph'),
	     };

  bless $self, $class;
  return $self;
}


=item make_simple_vertex

Create a simple vertex object.

A vertex is virtual if...

=cut
sub make_simple_vertex {

  my $self = shift;

  my $v_id = shift;
  my $virtual_p = shift || 0;

  return {
	  id => $v_id,
	  virtual_p => $virtual_p,
	  level => undef,
	 };
}


=item make_simple_edge

Create a simple edge--a pair of ids.

An edge is virtual if...

=cut
sub make_simple_edge {

  my $self = shift;

  my $sub_id = shift;
  my $obj_id = shift;
  my $pred_id = shift;
  my $virtual_p = shift || 0;

  return {
	  #id => $sub_id . '^' . $obj_id,
	  id => $sub_id . '^' . $pred_id . '^' . $obj_id,
	  subject_id => $sub_id,
	  object_id => $obj_id,
	  predicate_id => $pred_id,
	  virtual_p => $virtual_p,
	 };
}



1;
