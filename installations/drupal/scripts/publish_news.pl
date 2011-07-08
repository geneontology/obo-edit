#!/usr/local/bin/perl -w
####
#### Publish (or enqueue) a story on gonews.berkeleybop.org with CLI.
#### A good way to debug this all is with: perl -MWWW::Mechanize::Shell -eshell
####

use strict;
use WWW::Mechanize;
use HTTP::Cookies;
use Getopt::Std;
use Data::Dumper;

my $site = "http://gonews.berkeleybop.org";
my $login_path = "/news4go/user/login";
my $story_path = "/news4go/node/add/news";

use vars qw(
            $opt_h
            $opt_v
            $opt_n
            $opt_e
            $opt_u
            $opt_p
            $opt_t
            $opt_b
            $opt_T
           );

getopts('hvneu:p:b:t:T:');

## Embedded help through perldoc.
if( $opt_h ){
  system('perldoc', __FILE__);
  exit 0;
}

sub bark { my $s = shift || ''; print STDERR $s . "\n"  if $opt_v; }
bark("Will be verbose.");

###
### Check our required flags.
###

## Username.
my $username = undef;
if( ! $opt_u ){ die "A username is necessary, use the \"-u\" flag"; }
else{
  $username = $opt_u;
  bark("The username will be: $username");
}

## Password.
my $password = undef;
if( ! $opt_p ){ die "A password is necessary, use the \"-p\" flag"; }
else{
  $password = $opt_p;
  bark("The password will be: $password");
}

## Title.
my $title = undef;
if( ! $opt_t ){ die "A title is necessary, use the \"-t\" flag"; }
else{
  $title = $opt_t;
  bark("The story title will be: $title");
}

## Body.
my $body = undef;
if( ! $opt_b ){ die "A story body is necessary, use the \"-b\" flag"; }
else{
  $body = $opt_b;
  bark("The news item body will be:\n=====\n$body\n=====");
}

## Tags.
my @tags = undef;
if( ! $opt_T ){ die "A tag list is necessary, use the \"-T\" flag"; }
else{
  @tags = split(',', $opt_T);
  foreach my $tag (@tags){
    bark("Will add tag: $tag");
  }
}

###
### Check options.
###

## No-op: do a dry run to see what happens. Probably most useful with
## the verbose flag.
my $no_op = 0;
if( $opt_n ){
  $no_op = 1;
  bark("Will do a dry run.");
}

## No-op: do a dry run to see what happens. Probably most useful with
## the verbose flag.
my $should_publish = 1;
my $should_enqueue = 0;
if( $opt_e ){
  $should_publish = 0;
  $should_enqueue = 1;
  bark("Will not publish, but will enqueue story for later editing.");
}

###
### Try and login to the site.
###

## User login subsection.
my $mech = WWW::Mechanize->new();
$mech->cookie_jar(HTTP::Cookies->new());
my $response = $mech->get($site . $login_path);
die "Failed to login to news" if ! $response->is_success;

## Get the login form and login
#$mech->form_name('user-login');
#$mech->field('edit-name' => $username);
#$mech->field('edit-pass' => $password);
$mech->form_number(2);
$mech->field('name' => $username);
$mech->field('pass' => $password);
$mech->submit();

## Check login success.
die "Bad login or password"
  if $mech->content =~ /error/s; # this string is found in the error class

bark('Login successful!');

###
### Try to submit story.
###

## Go to story page.
$mech->get($site . $story_path);
die "Couldn't find story page"
  if ! $mech->content =~ /Create GO News Story/s; # proper page header

bark('Moved to story submission page.');

## Build-up form body.
#$mech->form_name('node-form'); # form had no _name_
$mech->form_number(2);

$mech->field('title' => $title);
$mech->field('body' => $body);

## Tags seem to work better in batch.
# foreach my $tag (@tags){
#   $mech->select("taxonomy[1][]" => $tag);
# }
$mech->select("taxonomy[1][]" => \@tags);

## Only submit if not no-op.
if( $should_enqueue ){
  ## NOTE: this item depends on our default drupal behavior not to
  ## publish.
  bark("Enqueuing story--will not directly publish.");
}else{
  bark("Will directly publish story (ticked \"publish\" box).");
  $mech->field('status', 1);
}

## Check on final available.
#my @available_fields = $mech->find_all_inputs();
#bark("Final:\n" . Dumper(\@available_fields));

## Only submit if not no-op.
if( $no_op ){
  bark("No-op: skipping submission.");
}else{

  ## Submit.
  $mech->field('op' => 'Save');
  $mech->submit();
  bark("Submitted story.");

  ## Double check.
  die "Submission not successful"
    if ! $mech->success();
  if( $mech->content =~ /error/s ){
    bark("Error content:\n=====" . $mech->content . "\n=====");
    die "Submission error";
  }
}

bark("Finished.");


###
### Help.
###

=head1 NAME

publish_news.pl

=head1 SYNOPSIS

publish_news.pl [-h] [-v] [-n] [-e]
                -u <username> -p <password> -t <title> -b <body>

=head1 DESCRIPTION

Publish (or enqueue) stories for GO News from the command line.

=head1 OPTIONS

=over

=item -h

Print this message.

=item -v

Enable more verbose messages. Useful for error checking.

=item -n

Dry run--do not actually change the site.

=item -e

Enqueue the story instead of publishing it.

=item -u <username>

The username of the publishing user.

=item -p <password>

The password of the publishing user.

=item -t <title>

The title of the news story to publish.

=item -b <body>

The body of the news story to publish.

=item -T <numerical CSVs>

The numberical list of tags that you want to apply.
The current number to tag mappings are:

 1 => announcement
 2 => feature
 3 => gene of the quarter
 4 => tutorial
 5 => tip
 6 => report
 7 => annotation
 8 => software
 9 => ontology
 10 => event
 11 => meeting
 12 => conference
 13 => course/workshop
 14 => warning
 15 => publication
 16 => jobs

=back

=head1 EXAMPLES

To enqueue (not publish) a story tagged with "tip" and "report":

/home/user/local/bin/publish_news.pl -v -e -u myname -p mypass -t "Test Story 002" -b "This is the second story." -T "5,6"
