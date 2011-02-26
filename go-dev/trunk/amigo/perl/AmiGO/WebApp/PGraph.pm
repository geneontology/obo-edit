=head1 AmiGO::WebApp::PGraph

...

=cut
package AmiGO::WebApp::PGraph;

use base 'AmiGO::WebApp';

use CGI::Application::Plugin::Session;
use CGI::Application::Plugin::TT;

use AmiGO::WebApp::Input;
use AmiGO::Worker::PGraph;


##
sub setup {

  my $self = shift;

  $self->{STATELESS} = 0;

  ## Configure how the session stuff is going to be handled when and
  ## if it is necessary.
  $self->session_config(
			CGI_SESSION_OPTIONS =>
			[
			 "driver:File",
			 $self->query,
			 {
			  Directory =>
			  $self->{CORE}->amigo_env('AMIGO_SESSIONS_ROOT_DIR'),
			 }
			],
			COOKIE_PARAMS =>
			{
			 -path  => '/',
			},
			SEND_COOKIE => 1,
		       );

  $self->tt_config(TEMPLATE_OPTIONS =>
		   {INCLUDE_PATH =>
		    $self->{CORE}->amigo_env('GO_DEV_ROOT') .
		    '/amigo/amigo/templates'});

  $self->mode_param('mode');
  $self->start_mode('exp');
  $self->error_mode('mode_fatal');
  $self->run_modes(
		   'exp'    => 'mode_exp',
		   'AUTOLOAD' => 'mode_exception'
		  );
}


## Maybe how things should look in this framework?
sub mode_exp {

  my $self = shift;

  ## Input handling.
  my $i = AmiGO::WebApp::Input->new();
  #my $params = $i->input_profile('???');
  my $params = $i->input_profile();
  ## TODO: input stuff, see GOOSE.pm

  ###
  ### JS prep.
  ###

  ##
  my $prep =
    {
     css_library =>
     [
      'standard', # basic GO-styles
     ],
     javascript_library =>
     [
      'com.raphael',
      'com.raphael.graffle',
      'org.bbop.graph.core',
      'org.bbop.graph.core',
      'org.bbop.graph.model',
      'org.bbop.graph.tree',
      'org.bbop.graph.render.phylo',
      'exp'
     ]
    };
  $self->add_template_bulk($prep);

  ## Initialize javascript app.
  ## NOTE: passing on init now.
  #my $jsinit ='GOOSEInit();';
  #$self->add_template_javascript($self->{JS}->initializer_jquery($jsinit));

  ## Juggle onto absolute version of header template.
  $self->set_template_parameter('page_name', 'amigo'); # menu bar okay
  $self->set_template_parameter('STANDARD_YUI', 'no');
  $self->add_template_content('html/main/pgraph.tmpl');
  return $self->generate_template_page({header=>0});
}



1;
