#!/usr/bin/perl -w

##
## CONTACT: sjcarbon@berkeleybop.org
##
## COMMENT CONVERTER RULES:
##
##   1) Comments to be converted must be dash-style.
##   2) After the start of a comment, a word that appears after '@@'
##      will be considered the subject of the following comment.
##      2.1) If the subject is a table, it must be of the form <table>.
##      2.2) If a columnn, it must be of the form <table>.<column>.
##   3) Comments and code cannot be mixed on the same line.
##   4) Do not talk about rule number four.
##
## NOTES:
##
##   None.
##

use strict;

use Getopt::Std;
use vars qw(
	    $opt_h
	    $opt_v
	    $opt_i
	    $opt_s
	    $opt_p
	   );

## Sane and easy to modify defaults.
my %local = (
	     BAR => 'foo'
	    );

getopts('hvi:s:p:');

if ( ! $opt_h ) {

  ## Check our options and set variables accordingly.
  if ( $opt_v ) {
    print "Will be verbose.\n"; }
  if ( $opt_i ) {
    $local{BAR} = $opt_i;
    print "BAR is now $opt_i.\n" if $opt_v; }
  if ( $opt_s ) {
      print "\nCREATE SCHEMA $opt_s;\n"; }
  if ( $opt_s ) {
      print "SET search_path TO $opt_p;\n\n"; }

  ## TODO: Get file and open for read.
  foreach my $arg (@ARGV){

    ## Open the file for reading.
    open(INFILE, "< $arg")
      or die "No such file: $!";;

    ## We start in the header.
    my @comment_cache = ();
    my $comment_name = '';
    my $comments = '';
    my $current_state = 'IN_CODE';
    while( <INFILE> ){

      # added by cjm:
      ## Nuke all experimental code.
      if (/^\s*--+\s*BEGIN EXPERIMENTAL/) {
	while( <INFILE> ) {
	  last if /END EXPERIMENTAL/;
	}
	$_ = <INFILE>;
      }

      s/DROP TABLE IF EXISTS.*//;

      ## 
      if( ( /\s*---?\s*\@\@\s*(\w+)\s*$/ ||
	    /\s*---?\s*\@\@\s*(\w+\.\w+)\s*$/ )
	  && $current_state eq 'IN_CODE' ){
	## Grab the name of a table.

	$comment_name = $1;
	$current_state = 'IN_COMMENT';

      }elsif( /\s*---?/ && $current_state eq 'IN_CODE' ){
	## Pass all other code comments, only changing the syntax to pg.

	s/---/--/;
	print $_;
	$current_state = 'IN_CODE';

      #}elsif( /\s*---?\s*(.*?$)/ && $current_state eq 'IN_COMMENT' ){
      }elsif( /\s*---?(\s*.*?$)/ && $current_state eq 'IN_COMMENT' ){
	## Accumulate comment once we've found the comment tag. Hang
	## out in comment mode.

 	$comments .= $1;
 	$current_state = 'IN_COMMENT';

      }elsif( ! /\s*---?/ && $current_state eq 'IN_COMMENT' ){
	## Dump comment when we get to a meaty line and jump back to
	## code mode.

 	chomp($comments) if $comments;
        $comments =~ s/\'/\\\'/g; # Escape quotes w/in a comment.

	## Switch on whether or not a we're dealing with a table or
	## column.
	if( $comment_name =~ /\w+\.\w+/ ){
	  push @comment_cache,
	    "COMMENT ON COLUMN $comment_name IS \'$comments\';";
	}else{

	  ## If the current thing we saw is a CREATE TABLE, dump a
	  ## TABLE comment; otherwise VIEW.
	  if( /^\s*CREATE\s+.*VIEW.+$/ ){
	    push @comment_cache,
	      "COMMENT ON VIEW $comment_name IS \'$comments\';";
	  }else{
	    push @comment_cache,
	      "COMMENT ON TABLE $comment_name IS \'$comments\';";
	  }

	}
 	print $_;

	## Reset.
 	$comment_name = '';
 	$comments = '';
 	$current_state = 'IN_CODE';

      }else{
	print $_;
      }
    }
    close(INFILE);

    ## Dump the stored comments.
    my $all_comments = join "\n", @comment_cache;
    print $all_comments;
  }

} else {

  print <<EOC;

  Usage:
     amigo-test-install [-h] [-v] [-i <arg>] [-s <arg>] [-p <arg>]

  Options:
     -h               Print this message.
     -v               Verbose messages.
     -i <foo>         The is a useless option.
     -s <schema>      Name of PostgreSQL SCHEMA
     -p <path>        Name of PostgreSQL SCHEMA

EOC

}


#    #my $comment_type = '';
#    #my $current_state = 'IN_HEADER';
#       if( /\s*---?/ && $current_state eq 'IN_HEADER' ){
# 	## Pass all header comments, only changing the syntax to pg.
# 	s/---/--/;
# 	print $_;
# 	$current_state = 'IN_HEADER';
#       }elsif( /^\s+$/ && $current_state eq 'IN_HEADER' ){
# 	## Transition from header to top level.
# 	print $_;
# 	$current_state = 'IN_CODE';
#      }elsif( /\s*---?\s*\@\@\s*(.*?)$/ && $current_state eq 'IN_CODE' ){


#       }elsif( /^--/ && $current_state eq 'IN_TOP_LEVEL' ){
# 	## Eat top level comments after the header.

# 	s/---\s*//;
# 	s/--\s*//;
# 	$comments .= $_;
# 	#print $_;
# 	$current_state = 'IN_TOP_LEVEL';

#       }elsif( ( /\w+\s+(\w+)\s+(\w+)[\w\s]+\(.*\)\;$/ ||
# 		/\w+\s+\w+\s+(\w+)\s+(\w+)[\w\s]+\(.*\)\;$/ ) &&
# 	      $current_state eq 'IN_TOP_LEVEL' ){
# 	## Dump accumulated comments and reset if we cross a singleton.

# 	chomp($comments);
# 	print "COMMENT ON { $1 $2 } IS \'$comments\';\n";
# 	$comments = '';
# 	print $_;
# 	$current_state = 'IN_TOP_LEVEL';

#       }elsif( /\w+\s+(\w+)\s+(\w+)\s+\($/ && $current_state eq 'IN_TOP_LEVEL' ){
# 	## Dump accumulated comments and reset if we open a block.

# 	chomp($comments);
# 	print "COMMENT ON { $1 $2 } IS \'$comments\';\n";
# 	$comments = '';
# 	print $_;
# 	$current_state = 'IN_BLOCK';

#       }elsif( /\)\;$/ && $current_state eq 'IN_BLOCK' ){
# 	## There should be nothing to see here.

# 	die "comment for nothing appears: $!" if $comments;
# 	print $_;
# 	$current_state = 'IN_TOP_LEVEL';

#       }elsif( /\s*--(.*?$)/ && $current_state eq 'IN_BLOCK' ){
# 	## Get comments in a block.

# 	$comments .= $1;
# 	$current_state = 'IN_BLOCK';

#       }elsif( /\w+/ && $current_state eq 'IN_BLOCK' ){
# 	## Dump comments before a non-empty in-block line.

# 	chomp($comments);
# 	print "COMMENT ON { $1 $2 } IS \'$comments\';\n";
# 	$comments = '';
# 	print $_;
# 	$current_state = 'IN_TOP_LEVEL';
