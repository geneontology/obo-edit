use Test::More;
plan tests => 38;
use GOBO::ClassExpression;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::GAFParser;
use GOBO::InferenceEngine;
use GOBO::Graph;
use FileHandle;

use Data::Dumper;

my $g = new GOBO::Graph;
my $x = GOBO::ClassExpression->parse_idexpr($g, 'a^foo(bar)');
print "x id: " . $x->id . "; as string: $x\n";
printf "  target: %s\n", $_ foreach @{$g->get_outgoing_ontology_links(node=>$x)};
ok($x->isa('GOBO::ClassExpression::Intersection'));
ok scalar(@{$x->arguments}) == 2;
ok grep { $_->id eq 'a' } @{$x->arguments};
ok grep { $_->isa('GOBO::ClassExpression::RelationalExpression') &&
             $_->relation->id eq 'foo' &&
             $_->target->id eq 'bar'
             } @{$x->arguments};

$x = GOBO::ClassExpression->parse_idexpr($g, 'a|foo(bar)');
print "x id: " . $x->id . "; as string: $x\n";
ok($x->isa('GOBO::ClassExpression::Union'));
ok scalar(@{$x->arguments}) == 2;
ok grep { $_->id eq 'a' } @{$x->arguments};
ok grep { $_->isa('GOBO::ClassExpression::RelationalExpression') &&
             $_->relation->id eq 'foo' &&
             $_->target->id eq 'bar'
             } @{$x->arguments};

$x = GOBO::ClassExpression->parse_idexpr($g, 'a|b');
print "x id: " . $x->id . "; as string: $x\n";
ok($x->isa('GOBO::ClassExpression::Union'));
ok scalar(@{$x->arguments}) == 2;
ok ( (grep { $_->id eq 'a' } @{$x->arguments}) && (grep { $_->id eq 'b' } @{$x->arguments}));

$x = GOBO::ClassExpression->parse_idexpr($g, 'a|b|c');
print "x id: " . $x->id . "; as string: $x\n";

ok($x->isa('GOBO::ClassExpression::Union'));
ok(scalar(@{$x->arguments}) == 3);
ok((grep { $_->id eq 'a' } @{$x->arguments}) && (grep { $_->id eq 'b' } @{$x->arguments}) && (grep { $_->id eq 'c' } @{$x->arguments}));
ok($x->as_string eq "a OR b OR c");

$x = GOBO::ClassExpression->parse_idexpr($g, 'a^part_of(b^part_of(c))');
#$x = GOBO::ClassExpression->parse_idexpr($g, 'x:a^r:part_of(x:b^r:part_of(x:c))');
ok($x->isa('GOBO::ClassExpression::Intersection'));
ok scalar(@{$x->arguments}) == 2;
ok grep { $_->id eq 'a' } @{$x->arguments};
ok grep { $_->isa('GOBO::ClassExpression::RelationalExpression') &&
             $_->relation->id eq 'part_of' &&
             $_->target->isa('GOBO::ClassExpression::Intersection') &&
             scalar(@{$_->target->arguments}==2)
             } @{$x->arguments};


$x = GOBO::ClassExpression->parse_idexpr($g, 'part_of(b)^part_of(c)');
print "x id: " . $x->id . "; as string: $x\n";
ok($x->isa('GOBO::ClassExpression::Intersection'));
ok scalar(@{$x->arguments}) == 2;
ok grep { $_->isa('GOBO::ClassExpression::RelationalExpression') &&
             $_->relation->id eq 'part_of' &&
             $_->target->id eq 'b'
             } @{$x->arguments};
ok grep { $_->isa('GOBO::ClassExpression::RelationalExpression') &&
             $_->relation->id eq 'part_of' &&
             $_->target->id eq 'c'
             } @{$x->arguments};

#$ENV{VERBOSE} = 1;
my $test_strs = [
[ 'a^(b|c)', 'a AND (b OR c)' ],
[ '(b|c)^a', '(b OR c) AND a' ],
[ '(a^b)|(a^c)', "(a AND b) OR (a AND c)" ],
[ 'a|(b|(c|d))', "a OR b OR c OR d" ],
[ '-a', "NOT a" ],
[ '-a|b', "(NOT a) OR b" ],
[ '-(a|b)', "NOT (a OR b)" ],
[ '-a^-b', "(NOT a) AND (NOT b)" ]];

foreach (@$test_strs)
{	$x = GOBO::ClassExpression->parse_idexpr($g, $_->[0]);
	ok( $x->as_string eq $_->[1], "Checking the parsing of " . $_->[0] );
	if ($x->as_string ne $_->[1])
	{	$ENV{VERBOSE} = 1;
		print STDERR "\n\nExpected " . $_->[1] . ", got $x\n";
		$x = GOBO::ClassExpression->parse_idexpr($g, $_->[0]);
		$ENV{VERBOSE} = 0;
		print STDERR "\n\n";
	}
}

my $ie = new GOBO::InferenceEngine(graph=>$g);

$x = GOBO::ClassExpression->parse_idexpr($g, '-a');
ok ( $x->isa('GOBO::ClassExpression::Complement') && $x->id eq '-a' && $x->as_string eq 'NOT a', "Checking Complement construction" );
#print "x id: " . $x->id . "; as string: $x\n" . $x->dump . "\n\n";
my $err;

my $tests = [
	## straight forward complements
	[ 'a', 'a', 1, "a c a" ],
	[ '-a', '-a', 1, "-a c -a" ],
	[ '-a', 'a', 0, "-a !c a" ],
	[ 'a', '-a', 0, "a !c -a" ],
	[ 'a', '-b', 1, "a c -b" ],
	[ '-b', 'a', 1, "-b c a" ],
	## a^-a = empty
	## a|-a = the universe
#	[ 'a^-a', 'a', 1, "-a^a (empty set) c a" ],  ## a^-a is empty
#	[ 'a^-a', '-a', 1, "-a^a (empty set) c -a" ],
#	[ 'a|-a', 'a', 0, "-a|a (universe) !c a" ],  ## a|-a is the universe
#	[ 'a|-a', '-a', 0, "-a|a (universe) !c -a" ],
#	[ 'a', 'a^-a', 0, "a !c -a^a (empty set)" ],
#	[ '-a', 'a^-a', 0, "-a !c -a^a (empty set)" ],
#	[ 'a', 'a|-a', 1, "a c -a|a (universe)" ],
#	[ '-a', 'a|-a', 1, "-a c -a|a (universe)" ],
];

foreach (@$tests)
{	$result = $ie->subsumed_by( GOBO::ClassExpression->parse_idexpr( $g, $_->[0]), GOBO::ClassExpression->parse_idexpr( $g, $_->[1] ));
	cmp_ok ( $result, "==", $_->[2], $_->[3]);
	if ($result != $_->[2])
	{	push @$err, $_;
	}
}

exit(0);

##             1 2   3     4   5     6   7       8           9  10       11
my @vars = qw! a a|b a|b|c a^b a^b^c a^z a^(b|c) (a^b)|(a^c) -c -(c|d|e) -(a|b)!;
my $result_words = { '0' => 'false', '1' => 'true' };
my $grid = {
#                  1  2  3  4  5  6  7  8  9  10 11
'a' =>           [ 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0 ],    # a
'a|b' =>         [ 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0 ],    # a|b
'a|b|c' =>       [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ],    # a|b|c
'a^b' =>         [ 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0 ],    # a^b
'a^b^c' =>       [ 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0 ],    # a^b^c
'a^z' =>         [ 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0 ],
'a^(b|c)' =>     [ 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0 ],    # a^(b|c)
'(a^b)|(a^c)' => [ 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0 ],    # (a^b)|(a^c)
'-c' =>          [ 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1 ],    # -c
'-(c|d|e)' =>    [ 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0 ],    # -(c|d|e)
'-(a|b)' =>      [ 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1 ]     # -(a|b)

}; # a^z

my $result;
my $max = (scalar @vars) - 1;
foreach my $n (0 .. $max)
{	foreach my $m ( 0 .. $max )
	{	my $n_x = $vars[$n];
		my $m_x = $vars[$m];
		$result = $ie->subsumed_by(GOBO::ClassExpression->parse_idexpr($g, $n_x), GOBO::ClassExpression->parse_idexpr($g, $m_x));

		cmp_ok( $result, "==", $grid->{$n_x}[$m], $n_x . " subsumed by " . $m_x . " : " . $result_words->{ $grid->{$n_x}[$m] } );

		if ($result != $grid->{$n_x}[$m])
		{	push @$err, [ $n_x, $m_x, $grid->{$n_x}[$m], $n_x . " c " . $m_x ];
#			$ENV{VERBOSE} = 1;
#			$ie->subsumed_by(GOBO::ClassExpression->parse_idexpr($g, $n_x), GOBO::ClassExpression->parse_idexpr($g, $m_x));
#			$ENV{VERBOSE} = 0;
#			print STDERR "\n\n\n";
		}
	}
}

print STDERR "\n\n";

if ($err)
{	$ENV{VERBOSE} = 1;
	foreach (@$err)
	{	## redo
		print STDERR $_->[3] .": expecting " . $_->[2] . "\n";
		$ie->subsumed_by(GOBO::ClassExpression->parse_idexpr($g, $_->[0]), GOBO::ClassExpression->parse_idexpr($g, $_->[1]));
		print STDERR "\n\n\n";
	}
	$ENV{VERBOSE} = 0;
}
exit(0);



