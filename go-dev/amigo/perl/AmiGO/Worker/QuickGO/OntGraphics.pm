=head1 AmiGO::Worker::QuickGO::OntGraphics

Mostly just a single-call simplification of
AmiGO::Cache::QuickGO::OntGraphics,
AmiGO::External::QuickGO::OntGraphics, and the filesystem.

=cut

package AmiGO::Worker::QuickGO::OntGraphics;

## Use a slightly different base...
use base ("AmiGO::JavaScript");

use AmiGO::Cache::QuickGO::OntGraphics;
use AmiGO::External::QuickGO::OntGraphics;


=item new

Constructor.

=cut
sub new {

  ##
  my $class = shift;
  my $self  = $class->SUPER::new();
  my $args = shift || {};

  bless $self, $class;
  return $self;
}


=item internal_file

Given an acc, will return the internal file path for the image. Image
may not exist there though...

=cut
sub internal_file {

  my $self = shift;
  my $input_term_acc = shift || die "need an acc as input $!";
  $input_term_acc =~ tr/\:/\_/;
  return $self->amigo_env('AMIGO_TEMP_IMAGE_DIR') .
    '/quickgo_ont_' . $input_term_acc . '.png';
}


=item internal_url

Given an acc, will return the internal url for the image. Image may
not exist there though...

=cut
sub internal_url {

  my $self = shift;
  my $input_term_acc = shift || die "need an acc as input $!";
  $input_term_acc =~ tr/\:/\_/;
  return $self->amigo_env('AMIGO_TEMP_IMAGE_URL') .
    '/quickgo_ont_' . $input_term_acc . '.png';
}


=item image_path

Given an acc, will return a filesystem string for the image (or undef
if something was impossible/failed).

=cut
sub image_path {

  my $self = shift;
  my $input_term_acc = shift || die "need an acc as input $!";

  ## This is what we want the final path to be.
  my $internal_file = $self->internal_file($input_term_acc);

  $self->kvetch("file: $internal_file");

  ## First, lets check if it is on the file system, if it is, we are
  ## done.
  if( -r $internal_file ){
    $self->kvetch("on FS");
  }else{
    $self->kvetch("not on FS");

    ## Ready cache and try and pull the image out of it.
    my $cache = AmiGO::Cache::QuickGO::OntGraphics->new();
    my $data_ball = $cache->get_data($input_term_acc);
    if( defined $data_ball ){
      $self->kvetch("in cache");
    }else{
      $self->kvetch("not in cache");

      ## Looks like it wasn't in the cache; go get it off of the
      ## internet.
      my $external_getter = AmiGO::External::QuickGO::OntGraphics->new();
      my $img_data = $external_getter->get_graph_image($input_term_acc);

      ## Looks like we can't get it there either. undef our return
      ## value and slink out.
      if( ! defined $img_data ){
	$self->kvetch("just not anywhere");
	return undef;
      }

      ## We successfully got the image data, cache it for future use.
      $data_ball =
	{
	 key => $input_term_acc,
	 image => $img_data,
	};

      ## Put it into the cache. No return check--should die if
      ## something goes wrong.
      $cache->cache_data($data_ball);
    }

    ## One way or another, $data_ball is defined and our cache is
    ## full. Let's put the data onto the filesystem.
    open(IMAGE, ">$internal_file") or
      die("Error opening QG image file $internal_file: $!");
    print IMAGE $data_ball->{image};
    close(IMAGE);
  }

  ## At this point, we have either returned undef or $internal_file
  ## points to a legit image on the filesystem--out work is done here.
  return $self->internal_url($input_term_acc);
}



1;
