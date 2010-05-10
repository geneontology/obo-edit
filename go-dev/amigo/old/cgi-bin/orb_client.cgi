#!/usr/local/bin/perl -w

##
## Tags that I use: BUG, TODO, WARN, NOTE, ASK
##

## Try to get the local environment sane.
BEGIN {
  require "config.pl" if -f "config.pl" ;
  if( ! defined($ENV{GO_ROOT}) &&
      -f "../cvs/go-dev/"){
    $ENV{GO_ROOT} = "../cvs/go-dev";
  }
  die "cannot find templates: $!" if ! $ENV{GO_TEMPLATE_PATHS};
}
use lib "$ENV{GO_ROOT}/go-perl";
use lib "$ENV{GO_ROOT}/go-db-perl";
use lib "$ENV{GO_ROOT}/amigo/perl";

## Bring in necessaries.
use utf8;
use strict;
use Template;

## Set up CGI environment,
use CGI qw/:standard/;
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);
$CGI::POST_MAX=1024 * 10;	## 10k uploads max. TODO: disable uploads
my $query = new CGI;


## The location of an ORB server.
my $ORB = 'http://toy.lbl.gov:9002/cgi-bin/amigo2/orb.cgi';
my $AGENT_STRING = 'AmiGO ORB Client v0.2b';

##########
##
## Set defaults for values that will be used during template
## processing.
##
##########

## Main/default template.
my $template = 'orb_client_main.tmpl';

## Main template variable.
my $vars = {};


##########
##
## Publish results using template.
##
##########

## Template runtime parameters.
my $tt = Template->new({
			INCLUDE_PATH => $ENV{GO_TEMPLATE_PATHS},
			EVAL_PERL => 1,
			#PRE_CHOMP => 1,
			#POST_CHOMP => 1,
			TRIM => 1,
			RECURSION => 1
		       });

html_header();
$tt->process($template, $vars) ||
  die_template("failed to process $template");


#####
##
## Subs.
##
####


##
sub html_header{ print "content-type:text/html\n\n"; }

##
sub die_template {

  my $message = shift || 'generic error';

  ## Template runtime parameters.
  my $diet = Template->new({
			    INCLUDE_PATH => $ENV{GO_TEMPLATE_PATHS},
			    EVAL_PERL => 1,
			    TRIM => 1,
			   });
  html_header();
  $diet->process('amigo_error.tmpl', {ERROR_MESSAGE => $message}) ||
  die "$message: $!"; # ASK: Is croak or confess more appropriate?
  exit(-1); ## BUG: Why do I need both.
}
