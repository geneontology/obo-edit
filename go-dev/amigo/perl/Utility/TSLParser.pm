=head1 Utility::TSLParser

Trivial Search Language Parser. Uses a variation on Dijkstra's
Shunting Yard algorithm.

=cut

use strict;

package Utility::TSLParser;

use base ('Utility');


=head2 Operators and their precedence.

BINARY:
'SANS'=>3
'AND'=>3
'OR'=>2
'('=>0

UNARY:
'NOT'=>1

=cut
my %binary = (
	      'SANS'=>3,
	      'AND'=>3,
	      'OR'=>2,
	      '('=>0
	     );
my %unary = (
	     'NOT'=>4
	    );

=item new

Constructor.

=cut
sub new {
  my $class = shift;

  my $self  = $class->SUPER::new();

  $self->{MIN_SIZE} = 3;
  $self->{ARG_STACK} = [];
  $self->{TOKENS} = [];
  $self->{BAD_TOKENS} = [];

  bless $self, $class;
  return $self;
}


=item tokenize

Tokenizer. A token is a:
   group of alphanums and '*' (includes operators)
   alphanums, '*', and spaces enclosed in quotation marks
   left paren
   right paren

Arguments: string, min
Returns: array of tokens

=cut
sub tokenize{

  my $self = shift;

  my $input = shift || '';
  my $success = 1;

  ##
  my @tokenized = $input =~ /[\w\*]+|\"[\w\*\s]+\"|\(|\)/g;

  ## Check to make sure that everything that's not an operator or a
  ## paren is at least the minimum size.
  foreach my $item (@tokenized){

    ## Doesn't look like an operator.
    if( _is_a_word($item) ){

      ## Strip quotes if extant.
      $item =~ tr/\"//d;

      ## Only add if right size.
      if( length($item) >= $self->{MIN_SIZE} ){
	push @{$self->{TOKENS}}, $item;
      }else{
	$success = 0;
	push @{$self->{BAD_TOKENS}}, $item;
      }
    }else{
      push @{$self->{TOKENS}}, $item;
    }
  }

  return $success;
}


# =item add_implicit

# First parse pass: scan tokens to find all implicit 'OR's and add them.

# =cut
# sub add_implicit{

#   my $self = shift;
#   my $token_arrayref = shift || ();
#   my $op = shift || 'OR';

#   my $prev = '';
#   my @all_tokens = ();
#   foreach my $curr (@$token_arrayref){

#     ## If the current token is a word or an '(' and the previous token
#     ## was a word or a ')', push an 'OR' onto the stack first.
#     if( $prev &&
# 	( is_a_word($curr) || $curr eq '(' ) &&
# 	( is_a_word($prev) || $prev eq ')' )){
#       push @all_tokens, $op;
#     }

#     push @all_tokens, $curr;
#     $prev = $curr;
#   }

#   return @all_tokens;
# }


=item get_tokens

=cut
sub get_tokens {
  my $self = shift;
  return $self->{TOKENS};
}


=item get_bad_tokens

=cut
sub get_bad_tokens {
  my $self = shift;
  return $self->{BAD_TOKENS};
}


=item parse

Real parse pass...

=cut
sub parse{

  my $self = shift;

  my $tokens = $self->{TOKENS};

  ##
  my @op_stack = ();
  my @output_stack = ();
  foreach my $token (@$tokens) {

    ## Token cascade.
    if( _is_a_word($token) ){

      $self->whine("[P] word case");
      push @output_stack, $token;

    }elsif( $token ne '(' &&
	    $token ne '(' &&
	    (defined($binary{$token}) || defined($unary{$token}) )){

      $self->whine("[P] op case");
      $self->whine("\t" . $token . ' ' . _precedence($token) .
		   ' =?= ' . $op_stack[$#op_stack] .
		   ' ' . _precedence($op_stack[$#op_stack]))
	if scalar(@op_stack) > 0;
      while( scalar(@op_stack) > 0 &&
	     _precedence($token) <= _precedence($op_stack[$#op_stack]) ){
	$self->whine("\t\tpush and shift: " .$op_stack[$#op_stack]);
	push @output_stack, pop @op_stack;
      }
      push @op_stack, $token;

    }elsif( $token eq '(' ){

      $self->whine("[P] ( case");
      push @op_stack, $token;

    }elsif( $token eq ')' ){

      $self->whine("[P] ) case");

      ## Pop to the output stack until we get the opening brace.
      my $too_many_closing = 1;
      while( scalar(@op_stack) > 0 &&
	     $op_stack[$#op_stack] ne '(' ){
	push @output_stack, pop @op_stack;
	$too_many_closing = 0;
      }
      die "TOO MANY CLOSING PAREN (a)" if $too_many_closing;

      ## Correct ordering--no longer need the parens.
      my $stack_top = pop(@op_stack);
      if( ! $stack_top || $stack_top ne '(' ){
	die "TOO MANY CLOSING PAREN (b)";
      }

    } else {
      die "NO TOKEN";
    }
  }

  ## Done main parsing, so pop all of the ops onto the output
  ## stack. This is also the final check for matching parens.
  while( scalar @op_stack > 0 ){
    my $tmp = pop @op_stack;
    if( $tmp eq '(' || $tmp eq ')' ){
      die "UNBALANCED PAREN";
    }
    push @output_stack, $tmp;
  }

  return \@output_stack;
}


##
## Subs.
##


=item _precedence

Return operator precedence.

=cut
sub _precedence{

  my $op = shift || '';
  my $ret = -1;
  if( defined($binary{$op}) ){
    $ret = $binary{$op};
  }elsif( defined($unary{$op}) ){
    $ret = $unary{$op};
  }
  return $ret;
}


=item _is_a_word

Determine whether something is a work or not.

=cut
sub _is_a_word{

  my $item = shift || '';

  my $return_val = 0;
  if( $item &&
      ! $binary{$item} &&
      ! $unary{$item} &&
      $item ne '(' &&
      $item ne ')' ){
    $return_val = 1;
  }

  return $return_val;
}



1;
