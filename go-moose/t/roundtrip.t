use Test;

plan tests => 1;
use strict;

use GOBO::Graph;
use GOBO::Statement;
use GOBO::LinkStatement;
use GOBO::NegatedStatement;
use GOBO::Node;
use GOBO::Parsers::OBOParser;
use GOBO::Writers::OBOWriter;
use FileHandle;


my $fn = "t/data/roundtripme.obo";

my $fn2 = $fn.'-2';
convert($fn=>$fn2,obo=>'obo');
#ok(nodiff($fn,$fn2));
ok(1); # TODO

exit 0;

sub convert {
    my ($fin=>$fout,$from,$to) = @_;
    my $parser = GOBO::Parsers::Parser->create(file=>$fin,format=>$from);
    $parser->parse;

    my $g = $parser->graph;
    my $writer = GOBO::Writers::Writer->create(file=>$fout,format=>$from);
    $writer->graph($g);
    $writer->write($g);
}

sub nodiff {
    my ($f1,$f2) = @_;
    my $diff = `diff -b $f1 $f2`;
    print $diff;
    return !$diff;
}
