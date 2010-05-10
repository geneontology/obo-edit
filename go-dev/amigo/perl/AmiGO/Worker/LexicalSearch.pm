=head1 AmiGO::Worker::LexicalSearch

Building it from autocompleter stuff...

=cut

use utf8;
use strict;

package AmiGO::Worker::LexicalSearch;

use base ("AmiGO");

use Lucene;
use AmiGO::Lucene;
use Data::Dumper;
use Time::HiRes qw(gettimeofday tv_interval);
use Data::Page;
#use Search::Tools::HiLiter;
use HTML::Highlight;


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();
  my $args = shift || {};

  $self->{PAGER} = Data::Page->new();

  ## Default spot.
  $self->{SPOT} = $self->amigo_env('INDEX_DIR') . '/lucene/general';
  ## Try and get a requested spot.
  if( defined $args->{index} ){
    $self->{SPOT} =
      $self->amigo_env('INDEX_DIR') . '/lucene/' .  $args->{index};
  }

  bless $self, $class;
  return $self;
}


=item query

perform a lexical query

=cut
sub query {

  my $self = shift;
  my $qstr = shift || '';
  my $page = shift || die; # required
  my $per_page = shift || die; # required

  my $analyzer = new Lucene::Analysis::Standard::StandardAnalyzer();
  my $store = Lucene::Store::FSDirectory->getDirectory($self->{SPOT}, 0);
  my $searcher = new Lucene::Search::IndexSearcher($store);

  # build a query on another field
  #my $query = $parser->parse("title:cookbook");
  ## Build a query on 'body'.
  my $lucy = AmiGO::Lucene->new();
  #my $parser = new Lucene::QueryParser('name', $analyzer);
  my $parser =
      #new Lucene::MultiFieldQueryParser(['acc','symbol','name','synonym'],
      new Lucene::MultiFieldQueryParser(['acc','symbol','name','synonym',
					 'gptype', 'source', 'species'],
					$analyzer);
  my $query = $parser->parse($lucy->fix_query($qstr));
  #my $query = $parser->getWildcardQuery('name', $lucy->fix_query($qstr));

  $self->kvetch("search query: " . $qstr);
  $self->kvetch("fixed query: " . $lucy->fix_query($qstr));
  $self->kvetch("stripped query: " . $lucy->strip_query($qstr));
  $self->kvetch("parsed query: " . $query->toString());

  # query index and get results
  my $hits = $searcher->search($query);

  # Get the total number of results, and let the pager know.
  my $num_hits = $hits->length() ; ## BUG? is this *really* off by
                                   ## one? it sure crashes like it
                                   ## is...
  $self->{PAGER}->entries_per_page($per_page);
  $self->{PAGER}->current_page($page);
  $self->{PAGER}->total_entries($num_hits);

  $self->kvetch("num hits: " . $num_hits);
  #$self->kvetch("___over", 1);

  ## Create a highlighter for the query.
  #   #   my $re = Search::Tools::RegExp->new();
  #   #   my $rekw = $re->build($qstr);
  #   #   my $hiliter = Search::Tools::HiLiter->new( rekw => $rekw );
  #   my $hiliter = Search::Tools::HiLiter->new( query => $qstr );
  my @foo = split(/ /, $lucy->strip_query($qstr));
  my $hiliter = new HTML::Highlight
    (
     words => \@foo,
     wildcards => [undef, '%', '*'],
#     colors => [],
     czech_language => 0,
     debug => 0
    );

  ## Get fields and ranking score for each hit. Note that Lucene wants
  ## indexing to start at zero, so the loop is of by one and wrapped
  ## in a test to make sure we got something.
  my $first = $self->{PAGER}->first;
  my $last = $self->{PAGER}->last;
  my $results = [];
  if( $first > 0 ){
    for( my $i = $first -1; $i < $last; $i++ ){

      $self->kvetch("\t". $first-1 . ' ... ' . $i . ' ... ' . $last-1);

      my $score = $hits->score($i);
      my $doc = $hits->doc($i);
      my $acc = $doc->get('acc');
      my $symbol = $doc->get('symbol');
      my $name = $doc->get('name');
      my $synonym = $doc->get('synonym');
      my $species = $doc->get('species');
      my $source = $doc->get('source');
      my $gptype = $doc->get('gptype');

      $self->kvetch('hit: ' . $name . ' (' . $acc . ')');

      ## Switch between gps and terms.
      my $regexp = $self->term_regexp_string();
      my $qurl = '';
      if( $acc =~ /$regexp/ ){
	$qurl = $self->get_interlink({mode=>'term-details', arg=>{acc=>$acc}});
	#$desc = $acc;
      }else{
	$qurl = $self->get_interlink({mode=>'gp-details', arg=>{gp=>$acc}});
      }

      ## Results to be pushed out...
      push @$results, {
		       'acc' => $acc,
		       'symbol' => $symbol,
		       'name' => $name,
		       'synonym' => $synonym,
		       'species' => $species,
		       'source' => $source,
		       'gptype' => $gptype,
		       'hilite_acc' =>
		       $hiliter->highlight($lucy->strip_query($acc)),
		       'hilite_symbol' =>
		       $hiliter->highlight($lucy->strip_query($symbol)),
		       'hilite_name' =>
		       $hiliter->highlight($lucy->strip_query($name)),
		       'hilite_synonym' =>
		       $hiliter->highlight($lucy->strip_query($synonym)),
		       'hilite_species' =>
		       $hiliter->highlight($lucy->strip_query($species)),
		       'hilite_source' =>
		       $hiliter->highlight($lucy->strip_query($source)),
		       'hilite_gptype' =>
		       $hiliter->highlight($lucy->strip_query($gptype)),
# 		       'hilite_acc' => $hiliter->light($acc),
# 		       'hilite_symbol' => $hiliter->light($symbol),
# 		       'hilite_name' => $hiliter->light($name),
# 		       'hilite_synonym' => $hiliter->light($synonym),
		       'score' => sprintf("%.2f", $score) * 100,
		       'link' => $qurl,
		      };
    }
  }

  #$self->kvetch("___under", 1);

  ## Free memory and close everything.
  undef $hits;
  undef $query;
  undef $parser;
  undef $analyzer;
  $searcher->close();
  #undef $fsdir;
  undef $searcher;
  $store->close;
  undef $store;

  return $results;
}


=item get_page_info

Args: nil
Returns: an array of paging info

=cut
sub get_page_info {

  #$self->{QUERY_PAGER} = $results->pager();

  my $self = shift;

  my $total = $self->{PAGER}->total_entries || 0;
  my $first = $self->{PAGER}->first || 0;
  my $last = $self->{PAGER}->last || 0;
  my $first_page = $self->{PAGER}->first_page || 0;
  my $curr_page = $self->{PAGER}->current_page || 0;
  my $last_page = $self->{PAGER}->last_page || 0;

  $self->kvetch("pager total: " . $total);
  $self->kvetch("pager f: " . $first);
  $self->kvetch("pager l: " . $last);
  $self->kvetch("pager fp: " . $first_page);
  $self->kvetch("pager cp: " . $curr_page);
  $self->kvetch("pager lp: " . $last_page);

  return ($total, $first, $last, $curr_page, $first_page, $last_page);
}


1;
