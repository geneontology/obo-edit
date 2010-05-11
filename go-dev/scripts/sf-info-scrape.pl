#!/usr/bin/perl -w

####
##
## NOTE: I disabled the login ability for tracking reasons.
## BUGS: See below.
##
####

use strict;
#use HTTP::Cookies;
use HTML::TableExtract;
use Getopt::Std;
use WWW::Mechanize;

use vars qw(
	    $opt_h
	    $opt_v
	    $opt_V
	    $opt_i
	    $opt_l
	    $opt_p
	    $opt_d
	   );

## Sane and easy to modify defaults.
my %local = (
	     PROJECT_ID => '36855', # geneontology.sf.net
	     PROJECT_LOGIN => '',
	     PROJECT_PASSWORD => '',
	     DELAY_IN_SECONDS => 1
	    );

getopts('hvVi:l:p:d:');

if ( $opt_h ) {

  print <<EOC;

  Usage:
     amigo-test-install [-h] [-v] [-V]
                        [-i <id>] [-l <login>] [-p <password>] [-d <number>]

  Options:
     -h               Print this message.
     -v               Enable verbose messages.
     -V               Enable even more verbose messages.
     -i <id>          Project ID on SF.net (TODO).
     -l <login>       User login on SF.net (DISABLED).
     -p <password>    User password on SF.net (DISABLED).
     -d <seconds>     Number of seconds to wait between tracker scrapes.

EOC

} else {

  ##
  ## Preparation from command line arguments.
  ##

  ## Check our options and set variables accordingly.
  print "Will be verbose.\n" if $opt_v || $opt_V;
  print "Will be very verbose.\n" if $opt_V;
  ##
  $local{PROJECT_ID} = $opt_i if $opt_i;
  print "Project ID will be: $opt_i\n" if ($opt_v  || $opt_V) && $opt_i;
  ##
  $local{PROJECT_LOGIN} = $opt_l if $opt_l;
  print "Project login will be: $opt_l.\n" if ($opt_v  || $opt_V ) && $opt_l;
  ##
  $local{PROJECT_PASSWORD} = $opt_p if $opt_p;
  print "Project password will be: $opt_p.\n" if ($opt_v  || $opt_V ) && $opt_p;
  ##
  $local{DELAY_IN_SECONDS} = $opt_d if $opt_d;
  print "Delay will be: $opt_d.\n" if ($opt_v  || $opt_V ) && $opt_d;


  ## Here is the user login subsection. If there is a login, there
  ## must also be a password.
  if( ( $local{PROJECT_LOGIN} && ! $local{PROJECT_PASSWORD} )||
      ( ! $local{PROJECT_LOGIN} && $local{PROJECT_PASSWORD} ) ){

    die "Need to login: $!";

    #}elsif( ! $local{PROJECT_LOGIN} && ! $local{PROJECT_PASSWORD} ){
    ## TODO: Anonymous scrape.
    #  die "Need to login: $!";

  }else{

    #     ## Goto login page.
    my $mech = WWW::Mechanize->new();
    $mech->cookie_jar(HTTP::Cookies->new());
    #     my $response =
    #       $mech->get('https://sourceforge.net/account/login.php');
    #     #open(OUTFILE, ">out1.html");
    #     #print OUTFILE $mech->content();
    #     #close(OUTFILE);
    #     ## Get the login form and login
    #     $mech->form_name('login');
    #     $mech->field(form_loginname => $local{PROJECT_LOGIN});
    #     $mech->field(form_pw => $local{PROJECT_PASSWORD});
    #     $mech->click();
    #     ## Check login success.
    #     if( $mech->content =~ /Invalid Password or User Name/s ){
    #       die "Invalid login or password: $!";
    #     }

    ## Goto tracker page.
    $mech->get('https://sourceforge.net/tracker/?' .
	       'func=add' .
	       '&group_id=' . $local{PROJECT_ID} );

    ## Scrape to get trackers atids.
    ## BUG: This is hard coded.
    my %atids =
      $mech->content =~
	/<li><a href=\"\/tracker\/\?group_id=36855\&amp\;atid=(.*?)\">(.*?)<\/a><\/li>/gs;

    ## Now go to each tracker and gut the tables.
    foreach my $atid (keys %atids){

      print STDERR "Working on \"$atids{$atid}\"\n" if $opt_v || $opt_V;
      print "[[$atids{$atid}:$atid]]\n\n";

      $mech->get('https://sourceforge.net/tracker/?' .
		 '&group_id=' . $local{PROJECT_ID} .
		 '&atid=' . $atid);
      my $tracker_content = $mech->content;

      ## Delay...
      print STDERR "\tSleeping $local{DELAY_IN_SECONDS} seconds..."
	if $opt_V;
      sleep $local{DELAY_IN_SECONDS};
      print STDERR "...done.\n" if $opt_V;

      ## Next, extract the aids from the table.
      my @column_headers = ("Request ID", "Summary", "Open Date",
                            "Priority", "Assigned To", "Submitted By");
      my $te = HTML::TableExtract->new( headers => [@column_headers] );
      $te->parse($tracker_content);
      foreach my $ts ($te->tables) { # Should only be one table.
        foreach my $row ($ts->rows) {

          my $aid = join(' ', split(' ', $$row[0]));
          my $summary = join(' ', split(' ', $$row[1]));
          (my $foo = $$row[2]) =~ tr/0-9\:\- //cd; # Nuke strange char.
          my $open_date = join(' ', split(' ', $foo));
          my $priority = join(' ', split(' ', $$row[3]));
          my $assigned_to = join(' ', split(' ', $$row[4]));
          my $submitted_by = join(' ', split(' ', $$row[5]));

	  print STDERR "Detailing \"$aid\"\n" if $opt_v || $opt_V;

	  ## Get the details page for that aid.
	  $mech->get('https://sourceforge.net/tracker/index.php?func=detail' .
		     '&group_id=' . $local{PROJECT_ID} .
		     '&atid=' . $atid .
		     '&aid=' . $aid);

	  my @detail_content =
	    $mech->content =~
	      /<\!-- google_ad_section_start -->(.*?)<\!-- google_ad_section_end -->/gs;
	  print "[AID:]$aid\n";
	  print "[SUMMARY:]$aid\n";
	  print "[OPEN_DATE:]$open_date\n";
	  print "[PRIORITY:]$priority\n";
	  print "[ASSIGNED_TO:]$assigned_to\n";
	  print "[SUBMITTED_BY:]$submitted_by\n";
	  foreach my $item (@detail_content){
	    print "[ITEM:]$item\n";
	  }
	  print "\n";

	  ## Delay...
	  print STDERR "\tSleeping $local{DELAY_IN_SECONDS} seconds..."
	    if $opt_V;
	  sleep $local{DELAY_IN_SECONDS};
	  print STDERR "...done.\n" if $opt_V;
	}
      }
    }
  }
}
