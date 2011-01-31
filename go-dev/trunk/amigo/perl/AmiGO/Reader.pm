#!/usr/bin/perl -w

##
##
##


use strict;
use FileHandle;
use File::Temp;
use GO::Parser;


package AmiGO::Reader;

require Exporter;
my @ISA = qw(Exporter);
my @EXPORT = qw(new readh success error_message);
#my @EXPORT_OK = qw();


## Takes GO::Parser handler strings and a scratch directory as
## arguements.
sub new {

  my $class = shift;
  my $target = shift || '';
  my $path = shift || 'sessions/scratch';

  #print STDERR '>>>[type]' . $type . "\n";
  #sleep 2;

  my $self = {};
  $self->{TARGET} = $target; # type of target
  $self->{PATH} = $path; # where to put the scratch files
  $self->{SUCCESS} = 1;
  $self->{ERROR_MESSAGE} = 'n/a';
  #$self->{RESULTS} = [];
  $self->{PARSER} = {};

  ##
  if( ! $self->{TARGET} ){
    $self->{ERROR_MESSAGE} = 'target type not declared';
    $self->{SUCCESS} = 0;
  }elsif( $self->{TARGET} eq 'go_assoc' ||
	  $self->{TARGET} eq 'obo_text' ||
	  $self->{TARGET} eq 'go_ids' ){

    $self->{PARSER} =
      GO::Parser->new({format=>$self->{TARGET}, handler=>'obj'});

  }else{
    $self->{ERROR_MESSAGE} = 'unknown target type';
    $self->{SUCCESS} = 0;
  }

  bless $self, $class;
  return $self;
}

## Convert the filehandle into a graph.
sub readh {

  my $self = shift;
  my $incoming = shift;

  #my $graph = {};
  my $term_l = [];

  if( ! $incoming ){

    $self->{ERROR_MESSAGE} = "AmiGO::Reader didn\'t have anything to read";
    $self->{SUCCESS} = 0;

  }else{

    ## The way it should be:
    #$self->{PARSER}->parse_fh($incoming);

    ## The way it is:
    ## BUG:
    ## Can't:
    ##  Direct filehandles--Chris's go-perl depends on it.
    ##  File::Temp--doesn't work on NFS.
    ##  "+>"--never worked properly and made zoidberg cry.
    ## TODO: I tried to do direct access via filehandles, but problems
    ## just kept coming up. I'm going to abandon that and use a tmp
    ## file to pass to the parser. This is not optimal--this should be
    ## fixed in go-perl.
    ##
    ## To fix this, the GO::Parser stuff needs to be fixed to be input
    ## agnostic like DB::Stag::base_parser, parser.
    ##
    ## BUG: did nasty temp file thing instead. All but a few lines
    ## should be unnecessary.
    ## BUG: This stuff isn't threadable and would probably break mod_perl.

    ## Get a temp file to dump stuff in to, then reopen for reading.
    my $tmp_fname = $self->{PATH} . "/TMP.$$.eraseme";
    open my $fh, ">", $tmp_fname
      or die "AmiGO::Reader, couldn\'t create: $tmp_fname: $!";
    while( <$incoming> ){
      print $fh $_; }
    close $fh;
    #open my $fh, "<", $tmp_fname
    #  or die "AmiGO::Reader, couldn\'t open: $tmp_fname: $!";

    ## Actually parse.
    $self->{PARSER}->parse($tmp_fname);
    #$graph = $self->{PARSER}->handler->graph;
    $term_l = $self->{PARSER}->handler->graph->get_all_nodes;
    #my $term_l = $graph->get_all_nodes;
    #die "check: " . $graph . ', (' . scalar(@$term_l) . ')';

    my $ret = unlink $tmp_fname;
    die "AmiGO::Reader.pm, couldn\'t remove scratch file: $tmp_fname" if ! $ret;
  }

  return $term_l;
}


## Return 1 or 0 for all operations.
sub success {
  my $self = shift;
  return $self->{SUCCESS};
}


## Returns the reason for the above error.
sub error_message {
  my $self = shift;
  return $self->{ERROR_MESSAGE};
}


1;
