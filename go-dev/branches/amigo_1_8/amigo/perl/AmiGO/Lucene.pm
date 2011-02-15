=head1 AmiGO::Lucene

Helper functions to make working with CLucene easier.

=cut

package AmiGO::Lucene;

use base 'AmiGO';
use utf8;
use strict;

=item new


=cut
sub new {

  ##
  my $class = shift;
  #my $arg_hash = shift || {};
  my $self  = $class->SUPER::new();

  ## Term-related.
  $self->{LUCENE_TERM_KEYWORDS} =
    {
     acc => 1,
     name => 1,
     ontology => 1,
     synonym => 1,
    };

  ## GP-related.
  $self->{LUCENE_GP_KEYWORDS} =
    {
     dbxref => 1,
     full_name => 1,
     symbol => 1,
     species => 1,
     scientific => 1,
     source => 1,
     gptype => 1,
     gpsynonym => 1,
     homolset => 1,
     association => 1,
    };

  ## Evidence.
  $self->{LUCENE_ASSOC_KEYWORDS} =
    {
     evidence => 1,
    };

  ## All together now...
  my %foo = ();
  foreach my $k (keys %{$self->{LUCENE_TERM_KEYWORDS}}){ $foo{$k} = 1; }
  foreach my $k (keys %{$self->{LUCENE_GP_KEYWORDS}}){ $foo{$k} = 1; }
  foreach my $k (keys %{$self->{LUCENE_ASSOC_KEYWORDS}}){ $foo{$k} = 1; }
  $self->{LUCENE_KEYWORDS} = \%foo;

  bless $self, $class;
  return $self;
}



=item term_keywords


=cut
sub term_keywords {
  my $self = shift;
  return $self->{LUCENE_TERM_KEYWORDS};
}


=item gene_product_keywords


=cut
sub gene_product_keywords {
  my $self = shift;
  return $self->{LUCENE_GP_KEYWORDS};
}


=item association_keywords


=cut
sub association_keywords {
  my $self = shift;
  return $self->{LUCENE_ASSOC_KEYWORDS};
}


=item all_keywords


=cut
sub all_keywords {
  my $self = shift;
  return $self->{LUCENE_KEYWORDS};
}


=item sanitize_query

Input: string
Output: string

Add proper escaping to the input string so as not to confuse lucene
with all of our colons. Also, remove leading and multiple'*'s.

=cut
sub sanitize_query {

  my $self = shift;
  $_ = shift || '';

  ## Leading * is bad.
  s/^\*+//g;

  ## Multiple *s are bad.
  s/\*+/\*/g;

  ## Floating control characters are bad.
  s/\s+[\*\+\^]+\s*/ /g;

  ## All colons are escaped.
  s/\:/\\\:/g;

  return $_;
}


=item fix_query

Input: string
Output: string

Add proper escaping to the input string so as not to confuse lucene
with all of our colons. Also, remove leading and multiple'*'s.
Also tries to keep keywords from getting clobbered.

=cut
sub fix_query {

  my $self = shift;
  $_ = shift || '';

  $_ = $self->sanitize_query($_);

  ## Unescape things that our lucene analyzer will use as keywords.
  foreach my $kw (keys %{$self->{LUCENE_KEYWORDS}}){
    s/$kw\\\:/$kw\:/g;
  }

  return $_;
}


=item strip_query

Input: string
Output: string

Strip keyword information out from the input string. This is intended
to be helpful to the HMTL highlighter--it has not other purpose at
this point.

##

=cut
sub strip_query {

  my $self = shift;
  $_ = shift || '';

  ## Unescape things that our lucene analyzer will use as keywords.
  foreach my $kw (keys %{$self->{LUCENE_KEYWORDS}}){
    s/$kw\://g;
    s/\*//g;
    s/\?//g;
  }

  ## The parens seem to confuse (crash) HTML::Highlighter so let's
  ## just take them out.
  s/\(//g;
  s/\)//g;

  return $_;
}


=item make_query_wild

Input: string
Output: string

Make everything that is a word, but not a field or a special word,
wild (i.e. '*').

TODO

##

=cut
sub make_query_wild {

  my $self = shift;
  $_ = shift || '';

  ## Split on white.
  my @query_terms = split;
  my @wild_query_buffer = ();
  foreach my $query_term (@query_terms){

    #print STDERR "_working on $query_term\n";

    ## If wordish.
    if( $query_term =~ /^[0-9a-zA-Z]+$/i ){

      #print STDERR "\tis a word\n";

      ## If not escaped field...
      if( $query_term !~ /\\\:/ ){

	#print STDERR "\tis not field\n";

	## If not control wordish.
	if( lc($query_term) ne 'and' &&
	    lc($query_term) ne 'or' &&
	    lc($query_term) ne 'not' ){

	 #print STDERR "\tis not control\n";

	  ## If not already escaped for something (~, ^4, etc.).
	  if( $query_term !~ /\*$/ &&
	      $query_term !~ /\^$/ &&
	      $query_term !~ /\^[0-9]+$/ ){

	    #print STDERR "\tis not already something\n";

	    ## Add wildcard.
	    $query_term .= '*';

	    #print STDERR "\tmade wild\n";
	  }
	}
      }
    }
    push @wild_query_buffer, $query_term;
  }

  ## And reassemble.
  return join ' ', @wild_query_buffer;
}


=item make_query_narrow

Input: string
Output: string

Make everything a detailed wildcard search of the name.

=cut
sub make_query_narrow {

  my $self = shift;
  my $q = shift || '';
  my $ont = shift || '';

  my @query_terms = split;
  my @fixed_query_terms =
    map { '+' . $_ . '*'; }
      @query_terms;
  $q = 'name:(' . join(' ', @fixed_query_terms) . ')';

  ## Restrict to ontology if desired.
  if( $ont ){
    $q = 'ontology:' . $ont . ' AND ' . $q;
  }

  return $q;
}



1;
