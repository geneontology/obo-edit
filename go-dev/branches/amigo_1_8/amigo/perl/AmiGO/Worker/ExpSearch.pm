=head1 AmiGO::Worker::ExpSearch

An attempt at combining the search of GOBO::DBIC::GODBModel::Query and
Utility::TSLParser.

NOTE/TODO: an interesting take on this could be to use a tree browser
to specify a term, then find, say, all IC's from human below it. Good
fun!

=cut

use utf8;
use strict;

package AmiGO::Worker::ExpSearch;

use base ("AmiGO");

## Parser tests.
use Lucene;
#use Lucene::Query;
use Lucene::QueryParser;
use Parse::RecDescent;

## Necessaries.
use GOBO::DBIC::GODBModel::Schema;
use GOBO::DBIC::GODBModel::Query;
use Utility::TSLParser;
use Data::Dumper;
use Time::HiRes qw(gettimeofday tv_interval);


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();

  my $type = shift || 'gp';
  $self->{EXP_SEARCH_QUERY}= GOBO::DBIC::GODBModel::Query->new({type=>$type});
  $self->{EXP_SEARCH_RESULTS} = [];
  $self->{PARSER} = Utility::TSLParser->new();

  $self->{PARSER}->verbose(1) if $self->verbose_p();

  ## We'll borrow SUCCESS and ERROR_MESSAGE from AmiGO.

  bless $self, $class;
  return $self;
}


=item query

perform an intelligent query

=cut
sub query {

  my $self = shift;
  my $query = shift || '';
  my $page = shift || 1;

  ## WARNING: Lucene parsing test.
  $self->kvetch("START Lucene parser test");

  my $analyzer = new Lucene::Analysis::Standard::StandardAnalyzer();
  my $parser = new Lucene::QueryParser('body', $analyzer);
  #my $parser = new Lucene::Query('body', $analyzer);
  #my $parser = new Lucene::QueryParser();
  #my $s_struct = $parser->parse($query);
  # $self->kvetch(Dumper($s_struct));
  #   $self->kvetch("NEXT Lucene parser test");
  my $s_struct = parse_query($query);
  $self->kvetch(Dumper($s_struct));
  my $edible = $self->_lucene_translate($s_struct);
  $self->kvetch(Dumper($edible));
  $self->kvetch("END Lucene parser test");

## TODO: this should be the way to go...but we may want to feed it the
## edibles produced by the Lucene parser to keep it consistant with
## what is going on with the lexical search...
#   ## WARNING: RecDescent parsing test.
#   $self->kvetch("START RecDescent parser test");
#   my $grammar = <<'_EOGRAMMAR_';

#   OP   : m([-+*/%])      # Mathematical operators
#   QUERY: /\w[a-z0-9_]*/i # Variable

# _EOGRAMMAR_
#   my $rd_parser = new Parse::RecDescent ($grammar) or die "Bad grammar!\n";
#   defined $rd_parser->startrule($query) or die "Bad text!\n";
#   $self->kvetch("END RecDescent parser test");

  $self->kvetch("\t___START PARSE");

  $self->{PARSER}->tokenize($query);
  #$self->{PARSER}->add_implicit();
  my $parsed = $self->{PARSER}->parse();
  my $tokens = _translate($parsed);
  foreach my $t (@{$self->{PARSER}->get_tokens()}){$self->kvetch("\t\t\t$t");}
  foreach my $p (@$parsed){ $self->kvetch("\t\t$p"); }
  foreach my $t (@$tokens){ $self->kvetch("\t$t"); }
  #sleep 1;

  ## Argument matching...
  my @arg_stack = ();
  foreach my $t ( @$tokens ){

    $self->kvetch("___on: $t");

    if( $t eq '-not' ){
      ## TODO: right now binary not gets dropped, so we need to use
      ## not_like and -not_and instead
      my $a1 = pop @arg_stack || die "NO A1 ARG UNARY";
      push @arg_stack, { $t => $a1 };
    }elsif( $t eq '-and' ||
	    $t eq '-or' ||
	    $t eq '-and_not' ){
      my $a1 = pop @arg_stack || die "NO A1 ARG BINARY";
      my $a2 = pop @arg_stack || die "NO A2 ARG BINARY";
      push @arg_stack, { $t => [ $a2, $a1 ] };
    }else{
      push @arg_stack, $t;
    }
  }
  die "THERE IS ARG STRANGENESS" if scalar(@arg_stack) > 1;

  # $self->kvetch(Dumper(@arg_stack));
  my $final = pop @arg_stack;
  $self->kvetch(Dumper($final));

  $self->kvetch("\t___END PARSE");

  # my $res =
  #   $self->{EXP_SEARCH_QUERY}->get_result_set($final);
  # $self->{EXP_SEARCH_QUERY}->get_result_set({'full_name' =>
  #					       {'like' => '%' . $query . '%'}});

  my @res = $self->{EXP_SEARCH_QUERY}->get_paged_results($final, $page);

  my $ret = [];
  foreach my $gp (@res){

    $self->kvetch("\t___got: " . $gp);

    ## Collect the synonyms into a usable form.
    my $synonyms = [];
    foreach my $syn ($gp->gene_product_synonym->all){
      push @$synonyms, $syn->product_synonym;
    }

    push @$ret,
      {
       full_name => $gp->full_name,
       symbol => $gp->symbol,
       synonym => $synonyms,
       gp_link => $self->get_interlink({mode=>'gp_details',
					arg=>{db=>$gp->dbxref->xref_dbname,
					      acc=>$gp->dbxref->xref_key}}),
		};
  }

  return $ret;
}


=item results

about complicated structure...

=cut
sub results {
  my $self = shift;
  return $self->{EXP_SEARCH_RESULTS};
}


=item page_info

array of page info

=cut
sub page_info {
  my $self = shift;
  return $self->{EXP_SEARCH_QUERY}->get_page_info();
}


## Translate...change the UI tokens into their internal counterparts.
sub _translate {

  my $stack = shift || [];

  my $out = [];
  foreach my $t (@$stack){

    if( $t eq 'NOT' ){
      push @$out, '-not';
    }elsif( $t eq 'AND' ){
      push @$out, '-and';
    }elsif( $t eq 'OR' ){
      push @$out, '-or';
    }elsif( $t eq 'SANS' ){
      push @$out, '-and_not';
    }else{
      $_ = $t;
      tr/\*/\%/;
      $_ = '%' . $_ . '%';
      s/(\%)\1/$1/g;
      push @$out, {'full_name' => {'-like' => $_}};
    }
  }
  return $out;
}


## Translate the Lucene structure into a string for 
## TODO: Finish.
## TODO: Add token checker to see if we're examining some field
## explicitly.
sub _lucene_translate {

  my $self = shift;
  my $struct = shift || {};

  my $out = [];

  foreach my $item (@$struct){

    $self->kvetch($item);

    my $query = $item->{query} || undef;
    my $conj = $item->{conj} || undef;
    my $type = $item->{type} || undef;
    my $term = $item->{term} || undef;
    my $subquery = $item->{subquery} || undef;
    #if( defined $conj && $conj eq 'AND' ){
    #  $conj = '-and';
    #}elsif( defined $conj && $conj eq 'OR' ){
    #  $conj = '-or';
    #}

    if( $query eq 'SUBQUERY' ){
      $self->kvetch("\tfound a subquery...diving with " . $item->{conj});

      push @$out, $conj if defined $conj;
      push @$out, 'NOT' if defined $type && $type eq 'PROHIBITED';
      #push @$out, '-not' if defined $type && $type eq 'PROHIBITED';
      push @$out, '(';
      push @$out, $self->_lucene_translate($subquery);
      push @$out, ')';

    }elsif( $query eq 'TERM' ){
      $self->kvetch("\tfound a query term");

      push @$out, $conj if defined $conj;
      push @$out, 'NOT' if defined $type && $type eq 'PROHIBITED';
      #push @$out, '-not' if defined $type && $type eq 'PROHIBITED';
      push @$out, $term;
      #$self->kvetch("\t\t" . $term . ' (' . $conj . ')');
      #       $_ = $t;
      #       tr/\*/\%/;
      #       $_ = '%' . $_ . '%';
      #       s/(\%)\1/$1/g;
      #       push @$out, {'full_name' => {'-like' => $_}};
    }else{
      $self->kvetch("\tfound something unknown");
    }
  }
  return join ' ', @$out;
}



1;
