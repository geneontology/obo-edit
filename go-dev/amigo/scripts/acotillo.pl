#!/usr/bin/perl -w

##
## 'Acotillo' is 'sledgehammer' in Spanish. It used to stress AmiGO
## and time the results.
##
## WARNING:
##
## Both perl threading libraries are *awful*--regularly 
##

use strict;
use threads;
use Getopt::Std;
use WWW::Mechanize;
#use WWW::Mechanize::XML;
use vars qw(
	    $opt_h
	    $opt_v
	    $opt_c
	    $opt_n
	    $opt_t
	    $opt_i
	    $opt_l
	   );

## Sane and easy to modify defaults.
my %local = (
	     AMIGO_USER_NUMBER => 1,
	     AMIGO_TARGET => 'http://amigo.geneontology.org/cgi-bin/amigo/go.cgi?action=query&view=query&query=kinase&search_constraint=terms',
	     #AMIGO_TARGET => 'http://amigo.geneontology.org/cgi-bin/amigo/go.cgi?action=query&view=query&query=kinase&search_constraint=gp',
	     #AMIGO_TARGET => 'http://toy.lbl.gov:9002/cgi-bin/amigo/go.cgi?action=query&view=query&query=kinase&search_constraint=terms',
	     #AMIGO_TARGET => 'http://toy.lbl.gov:9002/cgi-bin/amigo/go.cgi?action=query&view=query&query=kinase&search_constraint=gp',

	     TRIAL_ITERATIONS => 1,
	     TRIAL_COUNT => 1,

	     BOT_TIME_LIMIT => 0
	    );

getopts('hvcn:t:i:l:');

if ( $opt_h ) {

  print <<EOC;

  Usage:
     acotillo.pl [-h] [-v] [-c]
                 [-n <number>] [-t <url>] [-i <iterations>] [-l <limit>]

  Options:
     -h               Print this message.
     -v               Enable more verbose messages. This is useful for checking
                      installation errors.
     -c               Repeat the number of iterations from 1 to <iterations>.
     -n <number>      Number of AmiGO users to simulate.
     -t <url>         URL of an AmiGO target.
     -i <iterations>  Number of iterations to try.
     -l <limit>       Number of seconds a bot takes before bailing.

  Example Usage:
     perl acotillo.pl -v -l 300 -n 8 -i 5 -t "http://foo.org/foo.cgi" > foo.txt;

EOC

} else {

  ##
  ## Preparation with command line arguments.
  ##

  ## Check our options and set variables accordingly.
  if ( $opt_v ) {
    print "Will be verbose.\n"; }
  if ( $opt_n && $opt_n > 0 ) {
    $local{AMIGO_USER_NUMBER} = $opt_n;
    print "Will simulate $opt_n user(s).\n" if $opt_v; }
  if ( $opt_t ) {
    $local{AMIGO_TARGET} = $opt_t;
    print "Will check AmiGO at: $opt_t.\n" if $opt_v; }
  if ( $opt_i ) {
    $local{TRIAL_ITERATIONS} = $opt_i;
    print "Will take average of $opt_i iteration(s).\n" if $opt_v; }
  if ( $opt_c ) {
    $local{TRIAL_COUNT} = $local{AMIGO_USER_NUMBER};
    print "Will count up to: $local{TRIAL_COUNT}.\n" if $opt_v;
  }
  if ( $opt_l && $opt_l >= 0 ) {
    $local{BOT_TIME_LIMIT} = $opt_l;
    print "Will limit to $local{BOT_TIME_LIMIT}.\n" if $opt_v;
  }


  ##
  for( my $j = 1; $j <= $local{TRIAL_COUNT}; $j++ ){

    ## Check to see whether we count up or not.
    my $number_of_bots = $local{AMIGO_USER_NUMBER};
    if( $opt_c ){
      $number_of_bots = $j;
    }

    ## Start averaging timer.
    my $cumulative_time = 0;

    ## Average over a number of iterations.
    for( my $i = 0; $i < $local{TRIAL_ITERATIONS}; $i++ ){

      ## Start local timer.
      my $time = time();

      ## Ready the bots and send them to the fields.
      my @bot_threads = ();
      for( my $i = 0; $i < $number_of_bots; $i++ ){
	push @bot_threads, threads->create(\&bot);
      }

      ## Collect the bots safely
      foreach my $bot_thread (@bot_threads){
	my $response = $bot_thread->join();
	if( $@ ){
	  warn "Thread failure: $@"; }
	else{
	}
      }

      ## End timer.
      $time = time() - $time;
      my $it_num = $i + 1;
      print "\tTime for $number_of_bots user(s) in iteration $it_num: " .
	"$time seconds.\n" if $opt_v;
      $cumulative_time += $time;

      ## Check limits and bail if exceeded.
      if( $opt_l && $time >= $local{BOT_TIME_LIMIT} ){
	print "A bot has returned exceeding the time limit. Terminating...\n";
	$i = $local{TRIAL_ITERATIONS} + 10;
	$j = $local{TRIAL_COUNT} + 10;
      }
    }

    ## Print the average for this count.
    my $average_time = $cumulative_time / $local{TRIAL_ITERATIONS};
    print "Average time for $number_of_bots user(s) " .
      "over $local{TRIAL_ITERATIONS} iterations: $average_time seconds.\n";
  }
}


##########
##
## Subroutines.
##
##########


## I could make the bot more complex...click on things...etc.
sub bot {

  my $new_mech = WWW::Mechanize->new();
  $new_mech->get($local{AMIGO_TARGET});
  #$new_mech->cookie_jar(HTTP::Cookies->new());
  #print $new_mech->content();
  return $new_mech->response();

}
