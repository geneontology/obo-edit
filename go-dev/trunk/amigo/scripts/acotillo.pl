#!/usr/bin/perl -w

##
## 'Acotillo' is 'sledgehammer' in Spanish. It used to stress AmiGO
## and time the results.
##
## WARNING:
##
## Both perl threading libraries are *awful*.
##

use strict;
use threads;
use Getopt::Std;
use WWW::Mechanize;
use vars qw(
	    $opt_h
	    $opt_v
	    $opt_c
	    $opt_r
	    $opt_b
	    $opt_t
	    $opt_i
	    $opt_l
	    $opt_f
	    $opt_o
	   );

## Sane and easy to modify defaults.
my %local = (
	     BOT_NUMBER => 1,
	     BOT_TIMEOUT => 180,

	     TRIAL_ITERATIONS => 1,
	     TRIAL_COUNT => 1,

	     AMIGO_TARGETS => ['http://amigo.geneontology.org/cgi-bin/amigo/go.cgi'],
	     AMIGO_TARGETS_FILE => 'acotillo_urls.txt',

	     AMIGO_REPORT => []
	    );

getopts('hvrcb:t:i:l:f:o');


##
if ( $opt_h ) {
  usage();
  exit();
}


##
## Preparation with command line arguments.
##

if ( $opt_v ) {
  print "Will be verbose.\n"; }

## Check our options and set variables accordingly.
$local{BOT_NUMBER} = $opt_b if $opt_b && $opt_b > 0;
$local{TRIAL_ITERATIONS} = $opt_i if $opt_i;
$local{BOT_TIMEOUT} = $opt_l if $opt_l && $opt_l >= 0;

print "Will simulate $local{BOT_NUMBER} user(s).\n" if $opt_v;
print "Will limit each bot to $local{BOT_TIMEOUT} seconds.\n" if $opt_v;
print "Will do repeated trial(s) by bot number counting up.\n"
  if $opt_v && $opt_c;
print "Will take average of $local{TRIAL_ITERATIONS} iteration(s).\n" if $opt_v;

## Load targets.
if ( $opt_f ) {

  ## Remove defaults.
  $local{AMIGO_TARGETS} = [];

  ## File adjustment.
  $local{AMIGO_TARGETS_FILE} = $opt_f;
  die "$local{AMIGO_TARGETS_FILE} not available"
    if ! -f $local{AMIGO_TARGETS_FILE} || ! -R $local{AMIGO_TARGETS_FILE} ;
  print "Will use URLs in: $local{AMIGO_TARGETS_FILE}.\n" if $opt_v;

  open(TARGETS, "< $local{AMIGO_TARGETS_FILE}");
  #      || "$local{AMIGO_TARGETS_FILE} not found";
  while (<TARGETS>) {
    chomp;
    if ( ! /^[\s\n\t]*$/ && ! /^\s*\#.*/) {
      #print "Adding URL: $_ \n" if $opt_v;
      push @{$local{AMIGO_TARGETS}}, $_;
    }
  }
}else{
  if ( $opt_t ) {
    #print "Will single target at: $local{AMIGO_TARGET}.\n" if $opt_v;
    @{$local{AMIGO_TARGETS}}[0] = $opt_t;
  }
}

## DEBUG.
foreach my $url (@{$local{AMIGO_TARGETS}}) {
  print "Will target: $url.\n" if $opt_v;
}

##
print "\n" if $opt_v;
foreach my $url (@{$local{AMIGO_TARGETS}}) {

  ## Whether or not to do it once or count up to the number of bots.
  $local{TRIAL_COUNT} = $local{BOT_NUMBER} if $opt_c;

  ##
  for( my $j = 1; $j <= $local{TRIAL_COUNT}; $j++ ){

    ## Check to see whether we count up or not.
    my $number_of_bots = $local{BOT_NUMBER};
    $number_of_bots = $j if $opt_c;

    print "Running trial with $number_of_bots bot(s)"
      . " $local{TRIAL_ITERATIONS} times on \"$url\"...\n"
	if $opt_v;

    ## Start averaging timer.
    my $cumulative_time = 0;

    ## Average over a number of iterations.
    for( my $i = 0; $i < $local{TRIAL_ITERATIONS}; $i++ ){


      ## Ready the bots and send them to the fields.
      my @bot_threads = ();
      for( my $i = 0; $i < $number_of_bots; $i++ ){
	push @bot_threads, threads->create(\&bot, $i, $url);
      }

      ## Collect the bots safely.
      foreach my $bot_thread (@bot_threads){
	my $results = $bot_thread->join();
	my $id = $results->{id};
	my $time = $results->{time};
	if( $@ ){
	  #warn "Thread failure: $@";
	  die "Thread failure: $@";
	}else{
	  $cumulative_time += $time;
	  my $it_num = $i + 1;
	  print "Time for bot with id:$id in iteration $it_num: " .
	    "$time seconds.\n" if $opt_v;
	}
      }
    }

    ## Print the average for this count.
    my $average_time = ($cumulative_time / $local{TRIAL_ITERATIONS})
      / $number_of_bots;
    print "Average time for $number_of_bots user(s) " .
      "over $local{TRIAL_ITERATIONS} iterations: $average_time seconds.\n\n"
    	if $opt_v;

    ## Set up the struct as necessary.
    #if( $local{AMIGO_REPORT}->{$url} ){
    #  $local{AMIGO_REPORT}->{$url} = {};
    #}
    push @{$local{AMIGO_REPORT}},
      {
       url=>$url,
       bots=>$number_of_bots,
       time=>$average_time,
      };
  }
}

## Report.
if ( $opt_r ) {
  if ( $opt_o ) {
    #html_report();
    report();
  }else{
    report();
  }
}


##########
##
## Subroutines.
##
##########


##
sub bot {

  ## Init bot.
  my $id = shift;
  my $url = shift;
  my $new_mech = WWW::Mechanize->new();
  $new_mech->timeout($local{BOT_TIMEOUT});
  #$new_mech->cookie_jar(HTTP::Cookies->new());

  ## Start local timer.
  my $time = time();

  ## Get.
  $new_mech->get($url);

  ## End timer.
  $time = time() - $time;

  my $retval = 0;
  if ( $new_mech->success() ){
    $retval = $time;
  }
  return {
	  id=>$id,
	  time=>$retval,
	 };
}


# ##
# sub html_report {
#   print <<EOC;
# <html><body><table>
# EOC
#   foreach my $url (keys %{ $local{AMIGO_REPORT} }){
#     foreach my $nbots (keys %{}){
#     my $time = $local{AMIGO_REPORT}->{$_};
#     print "<tr><td>\n";
#     print $_;
#     print "\n</td><td>\n";
#     print $time;
#     print "\n</td></tr>\n\n";
#   }
#   print <<EOC;
# </table></body></html>
# EOC
# }


##
sub report {

  print <<EOC;
##@ START benchmark table (url #bots #seconds)
EOC

  foreach my $item (@{ $local{AMIGO_REPORT} }){
    #  foreach my $url (keys %{ $local{AMIGO_REPORT} }){
    #    foreach my $nbots (keys %{ $local{AMIGO_REPORT}->{$url} }){
    #      my $time = $local{AMIGO_REPORT}->{$url}{$nbots};
    my $url = $item->{"url"};
    my $bots = $item->{"bots"};
    my $time = $item->{"time"};
    print $url;
    print "\t";
    print $bots;
    print "\t";
    print $time;
    print "\n";
    # }
}

  print <<EOC;
##@ END benchmark table
EOC

}


##
sub usage{

  print <<EOC;

  Usage:
     acotillo.pl [-h] [-v]
                 [-c] [-b <number>] [-t <url>] [-i <iterations>] [-l <limit>]
                 [-r] [-o] ][-f <filename>]

  General Options:
     -h               Print this message.
     -v               Enable more verbose messages. This is useful for checking
                      installation errors.

  Options for stress mode:

     -b <number>      Number of bots to dispatch.
     -t <url>         URL of a target for a trial.
     -i <iterations>  Number of iterations to try per trial.
     -c               Repeat the trial using 1 to <number> bots.
     -l <limit>       Number of seconds a bot takes before bailing.
     -f <filename>    If a file is defined, read URLs from the file
                      and use them as targets (overrides -t flag).
     -r               Emit an additional summary table of results.
     -o *TODO*        Output report (see above) in HTML instead of text *TODO*.

  Example usage:
     perl acotillo.pl -v -l 300 -b 3 -c -i 2 -t "http://www.google.com"
     perl acotillo.pl -v -r -f acotillo_urls.txt

  Note:
     If you select neither the -v nor the -r option, you will very likely
     get no output at all.

  WARNING:
     Threads in perl are pretty awful, so you must expect the occasional
     core dump.

EOC

}
