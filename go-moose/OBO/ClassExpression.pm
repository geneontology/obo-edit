package OBO::ClassExpression;
use Moose;
use strict;
extends 'OBO::ClassNode';
use OBO::ClassExpression::RelationalExpression;
use OBO::ClassExpression::Intersection;

sub parse_idexpr {
    my $self = shift;
    my $g = shift;
    my $expr = shift;
    return unless $expr;
    print STDERR "Parsing: $expr\n";
    my @toks = split(/([\(\)\^\|])/,$expr);
    return _parse_idexpr_toks($g,\@toks);
}

sub _parse_idexpr_toks {
    my $g = shift;
    my $toks = shift;
    print STDERR "Parsing tokens: @$toks\n";
    if (!@$toks) {
        return;
    }
    my $tok = shift @$toks;
    if (!@$toks) {
        # atom
        printf STDERR "atom: $tok\n";
        return $tok;
    }
    my $this;
    if ($toks->[0] eq '(') {
        # relational expression
        shift @$toks;
        my $filler = _parse_idexpr_toks($g,$toks);
        $this = new OBO::ClassExpression::RelationalExpression(relation=>$tok,target=>$filler);
        printf STDERR "relexpr $tok $filler ==> $this\n";
    }
    else {
        printf STDERR "atom: $tok\n";
        $this = $g->noderef($tok);
    }
    if (@$toks) {
        my $combo;
        my $op = shift @$toks;
        my $next = _parse_idexpr_toks($g,$toks);
        if ($op eq '^') {
            printf STDERR "intersection: $this $next\n";
            $combo = new OBO::ClassExpression::Intersection(arguments=>[$this,$next]);
        }
        elsif ($op eq '|') {
        }
        elsif ($op eq ')') {
            # TODO: check balance
            printf STDERR "end-brace: $this\n";
            return $this;
        }
        else {
        }
        return $combo; # TODO -- DNF
    }
    printf STDERR "return: $this\n";
    return $this;
}


1;
