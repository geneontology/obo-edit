=head1 NAME

GOBO::ClassExpression

=head1 SYNOPSIS

  my $xp = GOBO::ClassExpression->parse_idexpr('GO:0005737^part_of(CL:0000023)');

=head1 DESCRIPTION

A class expression is an GOBO::ClassNode whose members are identified
by a boolean or relational expression. For example, the class 'nuclei
of cardiac cells' is expressed as the intersection (an
GOBO::ClassExpression::Intersection) between the set 'nucleus' (an
GOBO::TermNode) and the set of all things that stand in the part_of
relation to 'class node' (a GOBO::ClassExpression::RelationalExpression).

Simple ontologies do not include class expressions. They can often be
ignored for many purposes.

An GOBO::TermNode can be formally and logically defined by stating
equivalence to a GOBO::ClassExpression

  [Term]
  id: GO:new
  name: oocyte cytoplasm
  intersection_of: GO:0005737 ! cytoplasm
  intersection_of: part_of CL:0000023 ! oocyte

This can also be thought of as necessary and sufficient conditions for
membership of a class.

On parsing the above using GOBO::Parsers::OBOParser, the following should hold

  $t->label eq 'oocyte cytoplasm';
  $t->logical_definition->isa('GOBO::ClassExpression');

=head2 Example files

The xp version of SO includes intersection-based class expressions.
See http://sequenceontology.org

The extended version of GO will soon include these for regulation terms. See also

http://wiki.geneontology.org/index.php/Category:Cross_Products

=head2 Mapping to OWL

The notion of ClassExpression here is largely borrowed from OWL and
Description Logics. See

http://www.w3.org/TR/2008/WD-owl2-syntax-20081202/#Class_Expressions

Note that not everything in OWL is expressable in OBO format or the
GOBO model.

Conversely, this model and OBO format can express things not in OWL,
such as relation expressions involving intersection and union.

=head2 Mapping to the GO Database schema

Currently the GO database is not able to store the full range of class
expressions. It can only store logical definitions to
GOBO::ClassExpression::Intersection objects. See the POD docs for this
class for details.

=head2 Mapping to the Chado schema

Currently the GO database is not able to store the full range of class
expressions. It can only store logical definitions to
GOBO::ClassExpression::Intersection objects. See the POD docs for this
class for details.

=cut

package GOBO::ClassExpression;
use Moose;
use Moose::Util::TypeConstraints;
use GOBO::Types;
extends 'GOBO::ClassNode';
use GOBO::ClassExpression::RelationalExpression;
use GOBO::ClassExpression::Intersection;
use GOBO::ClassExpression::Union;
use GOBO::ClassExpression::Complement;

use Data::Dumper;

# abstract class - no accessors

# utility methods follow...

=head2 ID Expressions

A class expression can be expressed as an ID expression. See:

http://www.geneontology.org/GO.format.obo-1_3.shtml#S.1.6

For example:
  GO:0005737^part_of(CL:0000023)

The set of all cytoplasm (GO:0005737) instances that are part_of some oocyte (CL:0000023)


=head3 parse_idexpr

Generates a GOBO::ClassExpression based on an ID expression string

  Usage - $xp = GOBO::ClassExpression->parse_idexpr('GO:0005737^part_of(CL:0000023)');

The grammar for ID expressions is:

  GOBO::ClassExpression = GOBO::BooleanExpression | GOBO::RelationalExpression | GOBO::TermNode
  GOBO::BooleanExpression = GOBO::Intersection | GOBO::Union | GOBO::Complement
  GOBO::Intersection = GOBO::ClassExpression '^' GOBO::ClassExpression
  GOBO::Union = GOBO::ClassExpression '|' GOBO::ClassExpression
  GOBO::Complement = '-' GOBO::ClassExpression
  GOBO::RelationalExpression = GOBO::RelationNode '(' GOBO::ClassExpression ')'


=cut

sub parse_idexpr {
	my $self = shift;
	my $g = shift;
	my $expr = shift;
	return unless $expr;
	#print STDERR "Parsing: $expr\n" if $ENV{VERBOSE};
	my @toks = split(/([\(\)\^\|\-])/,$expr);
	@toks = grep {/\S/} @toks;
	my $lbr = 0;
	my $rbr = 0;
	map { $lbr++ if $_ eq '('; $rbr++ if $_ eq ')'; } @toks;
	warn "Unbalanced brackets detected in $expr!" if $lbr != $rbr;
	my $x = _parse_idexpr_toks($g,\@toks);
	$x->normalize if $x->can('normalize');
	return $x;
}


sub __is_operator {
	my $x = shift;
	return 1 if grep { $_ eq $x } ('|', '-', '^');
	return 0;
}

sub __is_bracket {
	my $x = shift;
	return 1 if $x eq '(' || $x eq ')';
	return 0;
}

sub __maybe_atom {
	my $x = shift;
	return 0 if grep {$_ eq $x} ('|', '-', '^', '(', ')');
	return 1;
}

# a^part_of(b^part_of(c))
# part_of(b)^part_of(c)

sub _parse_idexpr_toks {
	my $g = shift;
	my $toks = shift;
	if (!@$toks) {
		return;
	}
#	printf STDERR "Parsing tokens: \"%s\"\n", join('", "',@$toks) if $ENV{VERBOSE};

	my $tok = shift @$toks;
	while (@$toks && !$tok) {
		# process null tokens
		$tok = shift @$toks;
	}
#	print STDERR "tok: $tok; rest=@$toks\n" if $ENV{VERBOSE};


	# RETURN: atom
	if (!@$toks) {
		#printf STDERR "last atom: $tok\n" if $ENV{VERBOSE};
		#return $tok;
		return $g->noderef($tok);
	}

	my $this;
	if ($tok eq '-') {
		# complement
		#printf STDERR "parsing complement expr from @$toks\n" if $ENV{VERBOSE};

		my $filler;
		if ($toks->[0] eq '(')
		{	$filler = _parse_idexpr_toks($g,$toks);
			#print STDERR "filler: $filler\n" if $ENV{VERBOSE};
		}
		else
		{	$tok = shift @$toks;
			#print STDERR "tok: $tok\n" if $ENV{VERBOSE};
			$filler = $g->noderef($tok);
		}

		$this = new GOBO::ClassExpression::Complement(arguments=>[ $filler ]);

		#printf STDERR "complement $filler ==> $this ; remaining = @$toks\n" if $ENV{VERBOSE};
		return $this if ! @$toks;
	}

	if ($tok eq '(')
	{
		#printf STDERR "parsing expr in brackets from @$toks\n" if $ENV{VERBOSE};
		$this = _parse_idexpr_toks($g,$toks);

		#print STDERR "expr in brackets: $this; toks: @$toks\n" if $ENV{VERBOSE};

		if (! @$toks)
		{	#print STDERR "No more toks... returning $this\n" if $ENV{VERBOSE};
			return $this;
		}

		#print STDERR "Found some more toks!\n" if $ENV{VERBOSE};

		if ($toks->[0] eq '|' || $toks->[0] eq '^')
		{	my $op = shift @$toks;
			#print STDERR "op: $op\n" if $ENV{VERBOSE};
			my $next = _parse_idexpr_toks($g, $toks);
			#print STDERR "next: $next\n" if $ENV{VERBOSE};
			my $combo;
			if ($op eq '^') {
				#printf STDERR "intersection: $this ^ $next\n" if $ENV{VERBOSE};
				$combo = new GOBO::ClassExpression::Intersection(arguments=>[$this,$next]);
			}
			elsif ($op eq '|') {
				#printf STDERR "union: $this | $next\n" if $ENV{VERBOSE};
				$combo = new GOBO::ClassExpression::Union(arguments=>[$this,$next]);
			}
			#print STDERR "returning $combo\n" if $ENV{VERBOSE};
			return $combo;
		}
		elsif ($toks->[0] eq ')')
		{	# TODO: check balance
			#printf STDERR "end-brace: $this\n" if $ENV{VERBOSE};
			return $this;
		}
		else
		{	print STDERR "  HELP! not sure what to do here: @$toks\n" if $ENV{VERBOSE};
		}
	}

	if ($toks->[0] eq '(') {
		# relational expression or brackets
		shift @$toks;
		#printf STDERR "parsing relational expr from @$toks\n" if $ENV{VERBOSE};
		my $filler = _parse_idexpr_toks($g,$toks);

		#print STDERR "filler: $filler\n" if $ENV{VERBOSE};

		$this = new GOBO::ClassExpression::RelationalExpression(relation=>$tok,target=>$filler);
		#printf STDERR "relexpr $tok $filler ==> $this ; remaining = @$toks\n" if $ENV{VERBOSE};
	}
	elsif (defined $this)
	{	## we already have "this" defined
	}
	else {
		if (__is_bracket($tok) || __is_operator($tok) )
		{	print STDERR "  HELP! I think that $tok is an atom!\n" if $ENV{VERBOSE};
		}
		#printf STDERR "atom: $tok\n" if $ENV{VERBOSE};
		$this = $g->noderef($tok);
	}

	if (@$toks) {
		my $combo;
		my $op = shift @$toks;
		#printf STDERR "op: '$op'; rest=@$toks\n" if $ENV{VERBOSE};

		if ($op eq ')') {
			# TODO: check balance
			#printf STDERR "end-brace: $this\n" if $ENV{VERBOSE};
			return $this;
		}

		my $next = _parse_idexpr_toks($g,$toks);
		#print STDERR "next: $next\n" if $ENV{VERBOSE};
		if ($op eq '^') {
			#printf STDERR "intersection: $this ^ $next\n" if $ENV{VERBOSE};
			$combo = new GOBO::ClassExpression::Intersection(arguments=>[$this,$next]);
		}
		elsif ($op eq '|') {
			#printf STDERR "union: $this | $next\n" if $ENV{VERBOSE};
			$combo = new GOBO::ClassExpression::Union(arguments=>[$this,$next]);
		}
		else {
			print STDERR "  HELP! Dunno what to do about $op\n" if $ENV{VERBOSE};
		}
		#printf STDERR "return combo: $combo\n" if $ENV{VERBOSE};
		return $combo; # TODO -- DNF
	}
	#printf STDERR "return this: $this\n" if $ENV{VERBOSE};
	return $this;
}

=head2 normalize

A or (B or C) ==> A or B or C
A and (B and C) ==> A and B and C


=cut

sub normalize {
	my $self = shift;
	return;
}


=head2 get_expr_type

 input:	 hash (optional)
 output: hash

=cut


sub get_expr_type {
	my $self = shift;
	return \@_;
}


sub label {
	my $self = shift;
	return $self->as_string;
}

1;

