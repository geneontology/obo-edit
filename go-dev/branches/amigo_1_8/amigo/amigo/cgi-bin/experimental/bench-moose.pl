#!/usr/bin/perl -w
####
####   A) hash object constructor composition
####   B) hash object function composition
####   C) N/A
####   D) moose composition
####   E) moose composition (w/immutable)
####
#### Usage: bench-moose.pl 10000
####
#### Notes on D3: Seems to show that in this trivial case, there can
#### be a greater then 30x slowdown during object creation (I believe,
#### could be accessors, but other online benchmarks seems to show
#### that this might not be a problem). Interestly, when the string
#### size is increased, this difference can shrink (I have no idea
#### what that means, maybe more efficient destruction or something).
####
#### Obviously, there needs to be a non-trivial example where more of
#### the exciting features can be used. Maybe I'll do a full mini-GO
#### term one and compare it to DBIx::Class and manual...
####
#### Notes on E3: Much faster than D3--looks like only a greater than
#### 2x penalty for object creation. This probably wouldn't be a
#### show-stopper, but the second note for D3 is still true.
####

###
### A) Hash objects: data composition.
###

##
package A1;

sub new {

  ##
  my $class = shift;
  my $self = {};

  ## Add property.
  $self->{ID} = shift || '';

  bless $self, $class;
  return $self;
}

## Accessor.
sub id {
  my $self = shift;
  return $self->{ID};
}


##
package A2;
use base 'A1';

sub new {

  ##
  my $class = shift;
  my $arg = shift || '';
  my $self  = $class->SUPER::new($arg);

  ## Mutate property after.
  $self->{NAME} = $self->{ID} . '_id_after';

  bless $self, $class;
  return $self;
}

## Accessor.
sub name {
  my $self = shift;
  return $self->{NAME};
}


##
package A3;
use base 'A2';

sub new {

  ##
  my $class = shift;
  my $arg = shift || '';
  my $self  = $class->SUPER::new($arg);

  ## Mutate property before.
  $self->{NICK} = 'name_before_' . $self->{NAME};

  bless $self, $class;
  return $self;
}

## Accessor.
sub nick {
  my $self = shift;
  return $self->{NICK};
}


###
### B) Hash objects: function composition.
###

##
package B1;

sub new {

  ##
  my $class = shift;
  my $self = {};

  ## Add property.
  $self->{ID} = shift || '';

  bless $self, $class;
  return $self;
}

## Accessor.
sub id {
  my $self = shift;
  return $self->{ID};
}


##
package B2;
use base 'B1';

sub new {

  ##
  my $class = shift;
  my $arg = shift || '';
  my $self  = $class->SUPER::new($arg);

  bless $self, $class;
  return $self;
}

## Mutator.
sub name {
  my $self = shift;
  return $self->id() . '_id_after';
}


##
package B3;
use base 'B2';

sub new {

  ##
  my $class = shift;
  my $arg = shift || '';
  my $self  = $class->SUPER::new($arg);

  bless $self, $class;
  return $self;
}

## Mutator.
sub nick {
  my $self = shift;
  return 'name_before_' . $self->name();
}


###
### TODO: ??? C) above
###


###
### D) Moose objects: composition.
###

##
package D1;
use Moose;

has 'id' => (is => 'rw', isa => 'Str', default => '');

sub construct {
  my ( $self, $arg ) = @_;
  $self->id($arg);
}


##
package D2;
use Moose;
extends 'D1';

has 'name' => (is => 'rw', isa => 'Str', default => '');

after 'construct' => sub {
  my ( $self, $arg ) = @_;
  $self->name($self->id() . '_id_after');
};


##
package D3;
use Moose;
extends 'D2';


has 'nick' => (is => 'rw', isa => 'Str', default => '');

after 'construct' => sub {
  my ( $self, $arg ) = @_;
  $self->nick('name_before_' . $self->name());
};

no Moose;

###
### E) Moose objects: composition using immutable.
###

##
package E1;
use Moose;

has 'id' => (is => 'rw', isa => 'Str', default => '');

sub construct {
  my ( $self, $arg ) = @_;
  $self->id($arg);
}

__PACKAGE__->meta->make_immutable;

##
package E2;
use Moose;
extends 'E1';

has 'name' => (is => 'rw', isa => 'Str', default => '');

after 'construct' => sub {
  my ( $self, $arg ) = @_;
  $self->name($self->id() . '_id_after');
};

__PACKAGE__->meta->make_immutable;

##
package E3;
use Moose;
extends 'E2';


has 'nick' => (is => 'rw', isa => 'Str', default => '');

after 'construct' => sub {
  my ( $self, $arg ) = @_;
  $self->nick('name_before_' . $self->name());
};

__PACKAGE__->meta->make_immutable;

no Moose;

###
### Return to our regularly scheduled program.
###

package main;

use Time::HiRes qw( gettimeofday tv_interval );

my ($NUM_OF_ITERATIONS, $STRING_LENGTH) = @ARGV;

die("Need at least one numeric argument...\n...")
  if ! defined $NUM_OF_ITERATIONS;
$STRING_LENGTH = 10 if ! defined($STRING_LENGTH);

## Random string generator.
sub rstring {
  my $len = $STRING_LENGTH;
  my @chars = ('A'..'Z', 'a'..'z', '0'..'9', '_');
  my $string = '';
  foreach( 0 .. $len-1 ){
    $string .= $chars[rand($#chars)];
  }
  return $string;
}

## Do a little testing to make sure this is all okay...
print "Example:\n";
sub pp {
  my $str = shift;
  my $obj = shift;
  print "\t" . $str . "->id(): \"" . $obj->id() . "\"\n";
  print "\t" . $str . "->name(): \"" . $obj->name() . "\"\n";
  print "\t" . $str . "->nick(): \"" . $obj->nick() . "\"\n";
}
my $a = A3->new(rstring());
pp('A3', $a);
my $b = B3->new(rstring());
pp('B3', $b);
my $d = D3->new;
$d->construct(rstring());
pp('D3', $d);
my $e = E3->new;
$e->construct(rstring());
pp('E3', $e);
print "NUM_OF_ITERATIONS: $NUM_OF_ITERATIONS\n";
print "STRING_LENGTH: $STRING_LENGTH\n";

## Make a mess of random strings.
my @raw_data = ();

foreach( 0 .. $NUM_OF_ITERATIONS-1 ){
  push @raw_data, rstring();
}

my $count = undef;
my $time = undef;
my $delta = undef;

## Try life with A3;
$count = 0;
$time = [gettimeofday()];
foreach my $raw (@raw_data){
  my $x = A3->new($raw);
  $count += length($x->id()) + length($x->name()) + length($x->nick());
}
$delta = tv_interval($time);
print "_A_ time: " . $delta . "   (" . $count . ")\n";

## Try life with B3;
$count = 0;
$time = [gettimeofday()];
foreach my $raw (@raw_data){
  my $x = B3->new($raw);
  $count += length($x->id()) + length($x->name()) + length($x->nick());
}
$delta = tv_interval($time);
print "_B_ time: " . $delta . "   (" . $count . ")\n";

## Try life with D3;
$count = 0;
$time = [gettimeofday()];
foreach my $raw (@raw_data){
  my $x = D3->new;
  $x->construct($raw);
  $count += length($x->id()) + length($x->name()) + length($x->nick());
}
$delta = tv_interval($time);
print "_D_ time: " . $delta . "   (" . $count . ")\n";

## Try life with E3;
$count = 0;
$time = [gettimeofday()];
foreach my $raw (@raw_data){
  my $x = E3->new;
  $x->construct($raw);
  $count += length($x->id()) + length($x->name()) + length($x->nick());
}
$delta = tv_interval($time);
print "_E_ time: " . $delta . "   (" . $count . ")\n";

## Print results and leave.
print "Done!\n";



1;
