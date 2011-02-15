=head1 AmiGO::Aid::HeatMap

Crunch numbers and give colors for a heatmap.

=cut

package AmiGO::Aid::HeatMap;

use base 'AmiGO';
use utf8;
use strict;

use Math::Round qw();

=item new



=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  ## Args in.
  my $in_args = shift || {};
  my $args = $self->merge({
			   high => 100,
			   low => 0,
			   color_range => 255,
			  }, $in_args);
  $self->{HM_HIGH} = $args->{high};
  $self->{HM_LOW} = $args->{low};
  $self->{HM_COLOR_RANGE} = $args->{color_range};

  $self->{HM_STEP} = 
    $self->{HM_COLOR_RANGE} / ($self->{HM_HIGH} - $self->{HM_LOW});

  bless $self, $class;
  return $self;
}


## Make sure that the argument is in range.
sub _rangify {
  my $self = shift;
  my $i = shift || 0.0;

  my $retval = $i;
  if( $i > $self->{HM_HIGH} ){
    $retval = $self->{HM_HIGH};
  }elsif( $i < $self->{HM_LOW} ){
    $retval = $self->{HM_LOW};
  }

  return $retval;
}


=item get_color_offset

...

=cut
sub get_color_offset {

  my $self = shift;
  my $inc = shift || 0.0;
  $inc = $self->_rangify($inc);

  return $self->{HM_COLOR_RANGE} -
    Math::Round::round(($inc - $self->{HM_LOW}) * $self->{HM_STEP});
}


=item get_color

...

=cut
sub get_color {

  my $self = shift;
  my $inc = shift || 0.0;
  $inc = $self->_rangify($inc);

  my $index = $self->get_color_offset($inc);

  #return sprintf("#%02lx%02lx%02lx", $red, $green, $blue);
  return sprintf("#ff" . "%02lx%02lx", $index, $index);
}



1;
