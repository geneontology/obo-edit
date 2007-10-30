#!/usr/bin/perl -w

##
## Creates HTML pages that directly link large datasets. Made to help
## spiders. The output will be of the form of index*.html. Currently,
## there is only a target for GO.
##
## The $results_to_use variable is a pointer to an array of hashes
## whose fields are 'id' and 'info'. These wi
##
## The $to_url contains a pointer to a function that takes a single
## string and returns a URL.
##
## Search for "HERE" to see where to put code.
##

use strict;
use utf8;
use DBI;
use Getopt::Std;
use vars qw(
	    $opt_h
	    $opt_v
	    $opt_x
	   );

## Items per page.
my %local = (
	     ITEMS_PP => 1000
	    );

getopts('hvx:');

if ( $opt_h ) {

  print <<EOC;

Creates HTML pages that directly link large datasets. Made to help
spiders. The output will be of the form of index*.html. Currently,
there is only a target for GO.

  Usage:
     indexer.pl [-h] [-v] [-c] [-x <target>]

  Options:
     -h               Print this message.
     -v               Enable more verbose messages.
     -x <target>      Name of target.

  Example Usage:
     perl indexer.pl -x GO

EOC

} else {

  ## Check our options and set variables accordingly.
  if ( $opt_v ) {
    print "Will be verbose.\n"; }
  if ( $opt_x ) {
    $local{AMIGO_TARGET} = $opt_x;
    print "Will work on target: $opt_x.\n" if $opt_v;
  }else{
    die "Need to have a target: $!";
  }

  ##
  my $results_to_use;
  my $to_url;

  ##
  if( $opt_x =~ /^go$/i ){

    $results_to_use = get_from_local_db();
    $to_url = \&amigo_url_toy;

## HERE:
## Put additions here and the functions at the bottom.
##
#  }elsif( $opt_x =~ /^foo$/i ){
#
#    $results_to_use = get_foo_from_db();
#    $to_url = \&foo_url;
#
  }else{
    die "Need to have a target: $!";
  }

  ## Output into multiple (ugly) html files.
  my $page_count = 0;
  my $OFILE = undef;
  for( my $i = 0; $i < $#$results_to_use; $i++ ){

    ## We should hit this when we first come in.
    if( $i % ($local{ITEMS_PP}) == 0){
      $page_count++;
      if( $OFILE ){
	print $OFILE print_ending();
	close $OFILE;
      }
      open $OFILE, "> index_$page_count.html"
	or die "failed to open local file for writing: $!";
      print $OFILE print_beginning();

      print STDERR "On page: $page_count\n" if $opt_v;
    }

    ## Print an entry
    #print $OFILE '<p>p: ' . $page_count . ' i: ' . $i . ' ';
    print $OFILE '<p>';
    print $OFILE '<a href="' . &$to_url( $$results_to_use[$i]->{id} ) . '">';
    print $OFILE $$results_to_use[$i]->{id};
    print $OFILE '</a>';
    print $OFILE ' : ';
    print $OFILE $$results_to_use[$i]->{info};
    print $OFILE '</p>' . "\n";
  }
  print $OFILE print_ending();
  close $OFILE;

  print STDERR "Writing index...\n" if $opt_v;

  ## Write (ugly) index page.
  open $OFILE, "> index.html"
    or die "failed to open local file for writing: $!";

  print $OFILE print_beginning();
  print $OFILE "<p><b>An index of <em>$opt_x</em></b></p>";
  for( my $i = 0; $i < $page_count; $i++ ){

    my $beg = $i * $local{ITEMS_PP};
    my $end = ($i * $local{ITEMS_PP}) + $local{ITEMS_PP} -1;
    $end = $#$results_to_use -1 if $i +1 == $page_count;

    print $OFILE '<p>';
    print $OFILE '<a href="index_' . ($i + 1) . '.html">';
    print $OFILE '[' . $beg . ' - ' . $end . ']';
    print $OFILE '</a>';
    print $OFILE '</p>';
  }
  print $OFILE print_ending();

  close $OFILE;
}
print STDERR "Done!\n" if $opt_v;


#####
##
## Erm...templates?
##
#####


sub print_beginning{
  return '<html><head></head><body><div>';
}

sub print_ending{
  return '</div></body></html>';
}


##########
##
## Subroutines.
##
##########


## Scrape all of the terms with their definitions from the local GO
## database.
sub get_from_local_db{

  ## Connect to DB.
  my $db_handle =
    DBI->connect("DBI:mysql:go_latest_lite:spitz", "", "") ||
      die_template("Couldn't connect to that database on that host.");

  ## Scrape out results.
  my $statement = $db_handle->prepare('SELECT term.acc, term_definition.term_definition FROM term INNER JOIN term_definition ON (term.id = term_definition.term_id) WHERE term.acc LIKE "GO:%"');
  $statement->execute()
    or die_template("Couldn't run query.");

  my $count = 0;

  my @results = ();
  while( my @row = $statement->fetchrow_array ){
    push @results, {id => $row[0],
		    info => $row[1]};
  }

  ## Close connection.
  $statement->finish();
  $db_handle->disconnect();

  return \@results;
}


## Return a URL onto toy.
sub amigo_url_toy {
  my $var = shift || '_INDEXER_ERROR_';
  return 'http://toy.lbl.gov:9002/cgi-bin/amigo/go.cgi?term=' . $var . '';
}
