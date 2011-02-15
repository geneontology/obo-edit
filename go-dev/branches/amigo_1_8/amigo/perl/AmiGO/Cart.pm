=head1 AmiGO::Cart

TODO: This is temporary. This wil have to be moved somewhere later on,
probably a subclass os WebApp somewhere...

=cut

package AmiGO::Cart;

use base 'AmiGO';
use utf8;
use strict;


=item new

Args: none (return a new cart) or a session object from an AmiGO::WebApp.
Returns a Cart object. EnjoyQ

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  my $session = shift || undef;

  ## We'll borrow SUCCESS and ERROR_MESSAGE from AmiGO.

  ## Check that we have the structure that we want.

  ## Do we have the top structure?
  if( ! defined($session) || ! $session ){
    
  }else{
    $self->{CART} = {};
  }

  ## Do we have the default cart?


  bless $self, $class;
  return $self;
}

=item get_cart

Args: 
Returns an array ref to the specified cart.

=cut
sub get_cart {

  my $self = shift;
  my $name_of_cart = shift || "";

  my $gotten_cart = undef;

  if( defined($cart{$name_of_cart}) && $cart{$name_of_cart} ){
    $gotten_cart = $cart{$name_of_cart};
  }

  return $gotten_cart;
}


=item get_cart_names

Returns a hash pointer of all of the cart names, with type as the
value.

=cut
sub get_cart_names {

  my $self = shift;

  my $cart_names = {};

  ##
  foreach my $name (keys %{$cart{name}}){
    $cart_names{$name} = 1;
  }

  return $cart_names;
}


=item add_cart

Args: cart name and type

=cut
sub add_cart {

}


=item add_to_cart

Returns a hash pointer of all of the cart names.

=cut
sub add_to_cart {

  ## TODO: There must be type agreement within the cart. After the
  ## first item is entered, all items in the future must agree with
  ## the types already there. In practice, this should mean just
  ## checking against the first type if the list is not empty.

}


=item finalize_cart

This is necessary because we may have made a new cart in $self->new().

Returns a hash pointer to the whole of the cart.

=cut
sub finaliaze_cart {
  return $self->{CART};
}



1;
