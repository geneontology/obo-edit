use strict;
use Test::More 'no_plan';

use GO::SQLite3::KVStore;
use Data::UUID;

##
my $uuid_gen = Data::UUID->new();
my $file = $uuid_gen->to_string( $uuid_gen->create() ) . '.db';

## Build.
my $s = GO::SQLite3::KVStore->new({location => $file});
ok(defined($s), "object is defined");
ok( -f $file, "file is defined: $file");

## Add stuff.
$s->put('a', '1');
$s->put('b', '2');
$s->put('c', '3');

## Check things in.
my $a1 = $s->get('a');
my $b1 = $s->get('b');
my $c1 = $s->get('c');
is($a1, '1', "got a: $a1 (1)");
is($b1, '2', "got b: $b1 (1)");
is($c1, '3', "got c: $c1 (1)");

## Clobber.
$s->put('b', '4');

## Check things in.
my $a2 = $s->get('a');
my $b2 = $s->get('b');
my $c2 = $s->get('c');
is($a2, '1', "got a: $a2 (2)");
is($b2, '4', "got b: $b2 (2)");
is($c2, '3', "got c: $c2 (2)");

## Check things not in.
ok( ! defined $s->get('d'), "d is (rightly) not defined");

## Destroy (1).
$s->destroy();
ok( ! -f $file, "file is not defined (1)");

## Initialize.
$s->create();
ok( -f $file, "file is reinitialized");

## Check things not in.
ok( ! defined $s->get('a'), "a is (rightly) not defined");

## Destroy (2).
$s->destroy();
ok( ! -f $file, "file is not defined (2)");
