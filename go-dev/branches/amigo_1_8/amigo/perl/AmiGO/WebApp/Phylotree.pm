package AmiGO::WebApp::Phylotree;
use warnings;
use strict;
use Memoize;
use POSIX qw/ceil/;
#use IO::Scalar;
use File::Basename;

use Cairo;
use AmiGO::Worker::PANTHERTree; # I'll have to change the name... -Seth
use AmiGO::Worker::Phylotree;

=head1 NAME

AmiGO::WebApp::Phylotree - Interact with GO's C<phylotree> table.

=head1 DESCRIPTION



=over

=cut

use base 'AmiGO::WebApp';
#use AmiGO;
use CGI::Application::Plugin::Session;
use CGI::Application::Plugin::TT;

#use AmiGO::WebApp::Input;

use Data::Dumper;
#use IO::Scalar;
my $core = $AmiGO::Worker::Phylotree::core;

#use GOBO::DBIC::GODBModel;
use GOBO::DBIC::GODBModel::Query;

##
sub setup {
  my $self = shift;

  $self->tt_config
    (TEMPLATE_OPTIONS =>
     {INCLUDE_PATH => $core->amigo_env('GO_DEV_ROOT') .'/amigo/amigo/templates'}
    );

  $self->mode_param('mode');
  #$self->start_mode('sets');
  $self->start_mode('index');

#  $self->error_mode('mode_fatal');
  $self->run_modes
    (
     sets => 'mode_cluster_sets',
     index => 'mode_cluster_index',
     cluster => 'mode_cluster',
     dist => 'mode_dist_image',

     box => 'mode_id_entry',

     paint => 'mode_paint_ajax',
    );
}



=item mode_cluster_sets

This can be used as a start mode.  Displays the output from an
AmiGO::Worker::Phylotree call to C<$c-E<gt>sets()> with links to view
them.

=cut
sub mode_cluster_sets{
    my $c = shift;
    my $o = AmiGO::Worker::Phylotree->new();
    my @r = $o->sets();

    return '<ul>' . join('', map {
	sprintf '<li><a href="%s">%s</a></li>',
	  $_->url, $_->{dbname};
    } @r) . '</ul>';
}


=item mode_cluster_index

If this mode is used as the start mode it will assume you are looking
items where AmiGO::Worker::Phylotree C<dbname> value is S<PantherDB>,
otherwise it will seek itmes from C<dbname> fetched from the CGI
query.

It will display links to all C<key> values that have a C<dbname> of
the given value along with some summary information.

=cut
my $default_dbname = 'PantherDB';
my %sort =
  (
   name_asce => 'xref_key',
   name_desc => { -desc => 'xref_key' },
   memb_asce => 'members',
   memb_desc => { -desc => 'members' },
   anno_asce => 'last_anno',
   anno_desc => { -desc => 'last_anno' },
  );

sub mode_cluster_index{
    my $c = shift;
    my $q = $c->query();
    my $o = AmiGO::Worker::Phylotree->new
      (dbname => ($q->param('dbname') || $default_dbname));

    my @id = map { split(m/\s+/, $_) } $q->param('id');
    my @key = map { split(m/\s+/, $_) } $q->param('key');

    my @r;
    if (scalar(@id) || scalar(@key)) {
	@r = $o->id2phylotree(@id);
	push @r, $o->key2phylotree(@id);
    } else {
	my $rows = abs(int($q->param('rows') || 10));
	my $page = abs(int($q->param('page') || 1));

	my $order = $q->param('order') || 'name_asce';

	my $submit = lc($q->param('submit'));
	if ($submit eq 'next') {
	    $page++;
	} elsif ($submit eq 'prev') {
	    $page-- if ($page > 1);
	} elsif ($submit eq 'home') {
	    $page = 1;
	} else {
	    for my $k (keys %sort) {
		if ($q->param($k)) {
		    $order = $k;
		    last;
		}
	    }
	}
	$c->set_template_parameter(order => $order);

	@r = $o->id2($rows, $page, $sort{$order});

	if ($page > 1){
	    $c->set_template_parameter
	      (prev => $o->url(rows => $rows, page => ($page-1)));
	    $c->set_template_parameter
	      (home => $o->url(rows => $rows, page => 1));
	}
	$c->set_template_parameter
	  (next => $o->url(rows => $rows, page => ($page+1)));

	$c->set_template_parameter(form => $o->url);
	$c->set_template_parameter(rows => $rows);
	$c->set_template_parameter(page => $page);

	$c->set_template_parameter(offset => ($page - 1) * $rows);
    }


    $c->set_template_parameter(dbname => $o->{dbname});
    $c->set_template_parameter(clusters => \@r);
    $c->add_template_content('html/main/phylotree_cluster_index.tmpl');
    $c->set_template_parameter(page_title => "phylotree: $o->{dbname}");

    $c->generate_template_page();
}

sub mode_id_entry{
    my $c = shift;
    my $q = $c->query();
    my $dbname = $q->param('dbname') || $default_dbname;
    my $o = AmiGO::Worker::Phylotree->new(dbname => $dbname);
    my $action = $o->url(mode => 'index');

    $c->add_template_content(<<"FORM");
<h1>$dbname</h1>
<form method="post" action="$action" style="margin:auto;width:50ex">
<textarea name="id" rows="20" cols="50"></textarea>
<input type="submit" />
</form>
FORM
    return $c->generate_template_page();
}

=item mode_cluster

Displays the members in a single cluster along with summary
information.

=cut
sub mode_cluster{
    my $c = shift;
    my $q = $c->query();
    my $o = AmiGO::Worker::Phylotree->new
      (
       dbname => $q->param('dbname'),
       key    => $q->param('key'),
      );

    ## Check and see if there is a nice visual available and add it.
    my $pid = $q->param('dbname') . ':' . $q->param('key');
    my $ptree = AmiGO::Worker::PANTHERTree->new();
    my $raw_data = $ptree->get_tree($pid);
    my $viz_link = undef;
    if( defined($raw_data) and scalar(@$raw_data) ){
	$viz_link = $c->{CORE}->get_interlink
	  ({
	    mode => 'display_tree',
	    arg => {id => $pid},
	 });
      $c->{CORE}->kvetch('link: ' . $viz_link);
      $c->set_template_parameter('viz_link', $viz_link);
      $c->set_template_parameter('viz_img', $c->{CORE}->get_image_resource('ptree'));
    }

    $o->{dist} = $o->url(mode => 'dist');
    $c->set_template_parameter(cluster => $o);
    $c->set_template_parameter(gene_products => [ $o->gene_products() ]);
    $c->set_template_parameter(page_title => $pid);
    $c->set_template_parameter(concurrent => join(', ', map {
	$_->property_val;
    } $o->properties('concurrent')));
    $c->set_template_parameter(missing => [ sort {
	$a->property_val cmp $b->property_val;
    } $o->properties('missing') ]);

    $c->add_template_bulk({ javascript_library => ['com.jquery', 'com.jquery.tablesorter'] });
    if ($default_dbname eq $o->{dbname}) {
     	$c->set_template_parameter(paint_ajax => $o->url(mode => 'paint'));
    }


    $c->add_template_content('html/main/phylotree_cluster.tmpl');
    return $c->generate_template_page();
}


=item mode_dist_image

Returns a distribution of if given C<dbname> C<key> cluster.

=back

=cut
sub mode_dist_image{
    my $c = shift;
    my $q = $c->query();
    my $o = AmiGO::Worker::Phylotree->new
      (
       dbname => $q->param('dbname'),
       key    => $q->param('key'),
      );

    # set to 'svg' if you wish to output a png
    my $mime = $q->param('mime') || '';
    my $ref = $q->param('ref'); # only view refgenome

    my @o = $o->species_dist($ref);
    my $columns = 12; # 16
    my $diameter = $q->param('diameter') || 100;
    my $width = $columns * $diameter;
    my $height = ceil(scalar(@o)/$columns) * $diameter;
    my $padding = $diameter*.08;  # 8;

    my $lfont = $diameter*.30; # 30;
    my $sfont = $diameter*.17; # 15;

    my $out;
    my $surface;
    if ($mime eq 'svg') {
	$surface = Cairo::SvgSurface->
	  create_for_stream(sub {
				my $o = shift;
				$$o .= shift;
			    }, \$out, $width, $height);
    } else {
	$surface = Cairo::ImageSurface->create('argb32', $width, $height);
    }

    my $cr = Cairo::Context->create($surface);

    my $radius = $diameter / 2;
    my $cx = $radius;
    my $cy = $radius;
    my $rr = $radius - $padding; # real radius

    my $step = $sfont;
    my $loop = 0;
    for my $oo (@o) {

	$cr->arc($cx, $cy, $rr, 0, 360);
	if ($oo->{color} and
	    ($oo->{color} =~ m/([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i)) {
	    my @rgb = map { hex($_)/255 } ($1, $2, $3);

	    ##########
	    # For solid circles use:

	    $cr->set_source_rgb(@rgb);

	    ##########
	    # For spheres use:

	    # my $pat = Cairo::RadialGradient->create
	    #   (
	    #    $cx-($sfont/2), $cy-($lfont/2), $radius-$rr,
	    #    $cx, $cy, $rr,
	    #   );
	    # $pat->add_color_stop_rgb(0,1,1,1);
	    # $pat->add_color_stop_rgb(1, @rgb);
	    # $cr->set_source($pat);

	    #
	    ##########

	    $cr->fill;

	} else {
	    $cr->set_source_rgb(1, 1, 1);
	    $cr->fill;
	    $cr->arc($cx, $cy, $rr, 0, 360);
	    $cr->set_source_rgb(0, 0, 0);
	    $cr->stroke;
	}

	if ($oo->{count}) {
	    $cr->set_source_rgb(0, 0, 0);
	} else {
	    $cr->set_source_rgb((.5) x 3);
	}

	for (my $loop = 0; exists $oo->{display}->[$loop]; $loop++) {
	    my $text = $oo->{display}->[$loop];

	    $cr->set_font_size($sfont);
	    my $ext = $cr->text_extents($text);
	    $cr->move_to($cx - ($ext->{width}/2), $cy + ($step * ($loop + 1)));
	    $cr->show_text($text);
	    $cr->stroke;
	}

	$cr->set_font_size($lfont);
	my $ext = $cr->text_extents($oo->{count});
	$cr->move_to($cx - ($ext->{width}/2), $cy - ($step/2));
	$cr->show_text($oo->{count});
	$cr->stroke;


    } continue {
	if ($columns == (($loop % $columns) + 1)) {
	    $cx = $radius;
	    $cy += $diameter;
	} else {
	    $cx += $diameter;
	}
	$loop++;
    }

    $cr->show_page;

    if ($mime eq 'svg') {
	$c->header_add( -type => 'image/svg+xml' );
	#$c->header_add( -type => 'text/plain' );
	$surface->finish;
    } else {
	$c->header_props(-type=>'image/png',-expires=>'+3d');
	$surface->write_to_png_stream(sub { shift, $out .= shift });
    }

    return $out;
}

sub mode_paint_ajax{
    my $c = shift;
    my $q = $c->query();
    my $o = AmiGO::Worker::Phylotree->new
      (
       dbname => $q->param('dbname'),
       key    => $q->param('key'),
      );

    my ($msg, %url) = $o->paint_files();

    return "<ul><li>$msg</li>" . join('', map {
	my $url = $_;
	my $txt = $_;
	$txt =~ s(.*/)();
	"<li><a href=\"$url\">$txt</a>&nbsp;</li>";
    } keys %url) . '</ul>';
}



##
sub _header_wrap(){
  my $self = shift;
  if( $self->{TYPE} eq 'json' ){
    #$self->header_add( -type => 'application/json' );
    #$self->header_add( -type => 'plain/text' );
    #$self->header_add( -type => '' );
    $self->header_add( -type => 'text/html' );
  }else{
    $self->header_add( -type => 'text/html' );
  }
}




###
###
###

## Last called before the lights go out.
sub teardown {
  my $self = shift;

  # Disconnect when we're done, (Although DBI usually does this automatically)
  #$self->dbh->disconnect();
}
