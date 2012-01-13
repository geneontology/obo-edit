#!/usr/bin/perl -w
####
#### Description: TODO...
####
#### For help and usage, try the -h flag.
####

## Bring in necessaries.
use utf8;
use strict;
use Getopt::Std;
use LWP::UserAgent;
use Data::Dumper;
use WebService::Solr;

###
### Setup.
###

use vars qw(
	     $opt_h
	     $opt_v
	  );

getopts('hv');

## Embedded help through perldoc.
if( $opt_h ){
  system('perldoc', __FILE__);
  exit 0;
}

## Just a little printin' when feeling verbose.
sub ll {
  my $str = shift || '';
  print $str . "\n" if $opt_v;
}

## Make sure we have a URL defined.
my $url = shift @ARGV || die "url not defined";
ll("URL: " . $url);

###
### Get documents and alter them.
###

## Setup agent.
my $solr = WebService::Solr->new($url, { agent =>
					 LWP::UserAgent->new(keep_alive=>1)});

## Continue only if we can find the server.
if( $solr->ping() ){

  ll("Contacted server, continuing...");

  ## Response document processor.
  sub process_doc {
    my $solr = shift || die 'need a solr instance';
    my $doc = shift || die 'need a doc to process';

    my $id = $doc->value_for('id');
    ll('id: ' . $id);
    my($tmp, $acc) = split(/\^\^\^/, $id);
    ll("\tacc: " . $acc);

    # ## DEBUG
    # my @fnames = $doc->field_names();
    # for my $fnm (@fnames){
    #   ll("\tfield name: " . $fnm);
    # }

    ## TODO:
    ## Commit new info?
    #my $alt_id_field = WebService::Solr::Field->new('alternate_id' => $acc);
    #$doc->add_fields([$alt_id_field]);
    #$solr->add($doc);
    #$solr->commit();
  }

  ## How to process a doc response set.
  sub process_response {
    my $solr = shift || die 'need a solr instance';
    my $response = shift || die 'need a response to process';
    for my $doc ($response->docs()){
      &process_doc($solr, $doc);
    }
  }

  ## Initial query.
  my $num_rows = 100;
  my $query = "document_category:annotation_aggregate";
  my $response = $solr->search($query, {'rows' => $num_rows });
  &process_response($solr, $response);

  ## The pager to help traverse the results.
  my $pager = $response->pager();

  ## Continuing results--walk through with pager.
  while( my $next = $pager->next_page ){
    $pager->current_page($next);
    my $response = $solr->search($query, {'rows' => $num_rows,
  					  'start' => $pager->last});
    &process_response($solr, $response);
  }

}else{
  die "could not contact server at: " . $url;
}

###
### Perldoc help.
###

=head1 NAME

rcreate-ev-agg-term-ids.pl

=head1 SYNOPSIS

rcreate-ev-agg-term-ids.pl [-h] [-v] URL

=head1 DESCRIPTION

This script is a one-off that changes...TODO
An example run might be:

  ./create-ev-agg-term-ids.pl -v http://skewer.lbl.gov:8080/solr

=head1 OPTIONS

=over

=item -h

Print this message.

=item -v

Be verbose.

=back

=cut
