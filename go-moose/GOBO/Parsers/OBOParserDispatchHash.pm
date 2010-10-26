=head1 NAME

GOBO::Parsers::OBOParserDispatchHash

=head1 SYNOPSIS

 use GOBO::Parsers::OBOParserDispatchHash;
 my $parser = new GOBO::Parsers::OBOParser(file => $options->{input});
 $parser->parse;
 my $graph = $parser->graph;

=head1 DESCRIPTION

An L<GOBO::Parsers::Parser> that parses OBO Files.

Mostly identical to GOBO::Parsers::OBOParser but uses a dispatch table rather than an if/else cascade

=cut

package GOBO::Parsers::OBOParserDispatchHash;
use Moose;
use Data::Dumper;
extends 'GOBO::Parsers::OBOParser';



=head1 SEE ALSO

L<GOBO::Parsers::OBOParser>

=cut

__PACKAGE__->meta->make_immutable;

1;





