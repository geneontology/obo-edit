=head1 AmiGO::Lucene::Analysis::AmiGOTextAnalyzer

A custom Lucene analyzer for our custom GO needs.

=cut

use AmiGO::Lucene::Analysis::AmiGOTokenizer;

package AmiGO::Lucene::Analysis::AmiGOTextAnalyzer;

use base 'Lucene::Analysis::Analyzer';

use Lucene;

my $core = AmiGO->new();

=item new

Foo.

=cut
sub new {
  my $class = shift;
  my $self = $class->SUPER::new();

  return $self;
}

=item tokenStream

Bar.

=cut
sub tokenStream {
  my ($self, $field, $reader) = @_;

  #my $ret = new Lucene::Analysis::StandardTokenizer($reader);
  my $tokenized = new Lucene::Analysis::StandardTokenizer($reader);
  #my $tokenized = new Lucene::Analysis::AmiGOTokenizer($reader);

  ## DEBUG
  #my $core = AmiGO->new();
  $core->kvetch("\t\t(field: ".$field.')');
  $core->kvetch("\t\t(reader: ".$reader.')');
  $core->kvetch("\t\t(t: ".$tokenized.')');

 #  ## Special handling if it is a "term" or "acc" field. Bail out.
#   if( $field eq 'term' ||
#       $field eq 'acc' ){
#     #$core->kvetch("\t" . '...caught key field..."term" or "acc"');
#     $ret = new Lucene::Analysis::KeywordAnalyzer($ret);
#     #$ret = Lucene::Analysis::KeywordAnalyzer::new($ret);
#   }

  ## Standard: Lowercase and split on whitespace.
  #$ret = new Lucene::Analysis::LowerCaseFilter($ret);
  #$ret = new Lucene::Analysis::WhitespaceAnalyzer($ret);
  #$ret = new Lucene::Analysis::Standard::StandardAnalyzer($ret);
  return $tokenized;
}



1;
