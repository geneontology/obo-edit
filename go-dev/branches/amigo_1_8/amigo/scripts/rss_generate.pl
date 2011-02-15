#!/usr/bin/perl -w
####
#### Usage:
####    ./rss_generate.pl rss_generate.example.txt > /srv/www/htdocs/rss.xml
####    Aim browser at: "http://localhost/rss.xml"
####

use XML::RSS;
my $rss = new XML::RSS (version => '2.0');


###
### Add meta-information for RSS.
###


my $datetime = localtime();

## Main channel.
my %main_go_channel_info =
  (
   title          => 'Gene Ontology Consortium',
   link           => 'http://geneontology.org',
   language       => 'en',
   description    => 'News from the Gene Ontology Consortium.',
   #rating => '...',
   copyright      => 'Copyright 2010, The GO Consortium',
   pubDate        => $datetime,
   lastBuildDate  => $datetime,
   #docs => 'http://...',
   managingEditor => 'gohelp@geneontology.org',
   webMaster      => 'webmaster@geneontology.org',
  );
$rss->channel(%main_go_channel_info);

## A GO image.
my %go_image_info =
  (
   title       => 'geneontology.org',
   url         => 'http://geneontology.org/images/GOsplash.png',
   link        => 'http://geneontology.org',
   width       => 90,
   height      => 60,
   description => 'The Gene Ontology logo.',
  );
$rss->image(%go_image_info);


###
### Parse and read in file(s). Remember, titles must be unique...
###


my $temp_title = undef;
my $temp_url = undef;
my $temp_desc = undef;
my @item_queue = ();
while(<>){

  ## Fill temp variables with goodies.
  if( /^TITLE\:\s+(.*)$/ ){
    $temp_title = $1;
  }elsif( /^URL\:\s+(.*)$/ ){
    $temp_url = $1;
  }elsif( /^DESCRIPTION\:\s+(.*)$/ ){
    $temp_desc = $1;
  }

  ## If the variables are filled, push it onto the queue.
  if( defined $temp_title &&
      defined $temp_url &&
      defined $temp_desc ){
    push @item_queue, {
		       title => $temp_title,
		       url => $temp_url,
		       description => $temp_desc,
		      };
    $temp_title = undef;
    $temp_url = undef;
    $temp_desc = undef;
  }
}


###
### Emit individual items.
###


##
foreach my $queue_item (@item_queue){

  my $title =  $queue_item->{title};
  my $url = $queue_item->{url};
  my $desc = $queue_item->{description};

  my %item =
    (title => $title,
     # creates a guid field with permaLink=true
     permaLink  => $url,
     # alternately creates a guid field with permaLink=false
     # guid     => "gtkeyboard-0.85"
     #enclosure  => {url=>'http://foo.com', type=>"application/x-bittorrent"},
     description => $desc,
    );
  $rss->add_item(%item);
}


###
### Check text and output.
###


my $rss_text = $rss->as_string;

## TODO: check RSS text.

## Output RSS text.
print $rss_text;
