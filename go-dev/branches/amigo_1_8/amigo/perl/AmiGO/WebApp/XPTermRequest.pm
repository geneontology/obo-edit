package AmiGO::WebApp::XPTermRequest;
use base 'AmiGO::WebApp';

####
####
####

use AmiGO::Aid;
use AmiGO::WebApp::Input;
use AmiGO::External::JSON;

use Data::Dumper;
use File::Basename;

use CGI::Application::Plugin::Session;
use CGI::Application::Plugin::TT;

use Email::Simple;
use Email::Send;

##
#my $core = AmiGO::Aid->new();
my $core = AmiGO::JavaScript->new();

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
			 {Directory =>
			  $core->amigo_env('AMIGO_SESSIONS_ROOT_DIR')}
			],
			COOKIE_PARAMS =>
			{
			 -path  => '/',
			},
			SEND_COOKIE => 1,
		       );

  $self->tt_config(TEMPLATE_OPTIONS =>
		   {INCLUDE_PATH =>
		    $core->amigo_env('GO_DEV_ROOT') .
		    '/amigo/amigo/templates'});

  $self->mode_param('mode');
  $self->start_mode('xp_term_request');
  $self->error_mode('mode_fatal');
  $self->run_modes(
		   'xp_term_request' => 'mode_xp_term_request_top', # chooser
		   # 'regulation' => 'mode_request_regulation', # input
		   # 'part' => 'mode_request_part', # input
		   'regulation' => 'mode_request_input', # input
		   'part' => 'mode_request_input', # input
		   'process' => 'mode_request_input', # input
		   'do' => 'mode_request_do', # processor
		   'AUTOLOAD'        => 'mode_exception'
		  );
}


## 
sub mode_xp_term_request_top {

  my $self = shift;

  ##
  my $possible_templates =
    [
     {
      name => 'Regulation request',
      mode => 'regulation',
     },
     {
      name => 'Part-specific subtype requests',
      mode => 'part',
     },
     {
      name => 'Processes involved in other processes',
      mode => 'process',
     }
    ];
  $self->set_template_parameter('XPTEMPLATES', $possible_templates);

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please

  ## Templates and output.
  my $prep =
    {
     css_library => ['standard'],
     javascript_library => [ 'com.jquery', 'org.bbop.amigo']
    };
  $self->add_template_bulk($prep);
  $self->add_template_content('html/main/xp_term_request_client.tmpl');
  #return $self->generate_template_page({lite => 1});
  return $self->generate_template_page();
}


##
sub mode_request_input {

  my $self = shift;

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please

  ## Templates and output.
  my $prep =
    {
     css_library =>
     [
      'standard',
      'org.bbop.amigo.ui.autocomplete'
     ],
     javascript_library =>
     [
      'com.jquery',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.opensearch',
      'org.bbop.amigo.ui.autocomplete'
     ]
    };
  $self->add_template_bulk($prep);

  ## Switch template and js on runmode.
  my $current = $self->get_current_runmode();
  $core->kvetch('XPTR::mode_request_input: rm: ' . $current);
  if( $current eq 'regulation' ){
    $self->add_template_content('html/main/xp_term_request/xp_regulation.tmpl');
    $self->add_template_javascript($self->{JS}->initializer_jquery('new org.bbop.amigo.ui.autocomplete({id:"target", narrow:"true", search_type:"term", ontology: "biological_process", completion_type:"completion"});'));
  }elsif( $current eq 'process' ){
    $self->add_template_content('html/main/xp_term_request/xp_process.tmpl');
    $self->add_template_javascript($self->{JS}->initializer_jquery('new org.bbop.amigo.ui.autocomplete({id:"genus", narrow:"true", search_type:"term", ontology: "biological_process", completion_type:"completion"});new org.bbop.amigo.ui.autocomplete({id:"target", narrow:"true", search_type:"term", ontology: "biological_process", completion_type:"completion"});'));
  }elsif( $current eq 'part' ){
    $self->add_template_content('html/main/xp_term_request/xp_part.tmpl');
    $self->add_template_javascript($self->{JS}->initializer_jquery('new org.bbop.amigo.ui.autocomplete({id:"genus", narrow:"true", search_type:"term", ontology: "cellular_component", completion_type:"completion"});new org.bbop.amigo.ui.autocomplete({id:"target", narrow:"true", search_type:"term", ontology: "cellular_component", completion_type:"completion"});'));
  }else{
    die "Where did you get something like \"$current\"?";
  }

  return $self->generate_template_page();
}


##
sub mode_request_do {

  my $self = shift;

  ###
  ### Collect input.
  ###

  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('xp_term_request');
  my $genera = $self->to_array_ref($params->{genus});
  my $targets = $self->to_array_ref($params->{target});
  my $relations = $self->to_array_ref($params->{relation});
  my $name = $params->{name} || ''; # Optional.

  ###
  ### Generation.
  ###

  my @temp_terms = ();
  if( scalar(@$relations) && scalar(@$genera) && scalar(@$targets) ){

    ## Only ready external if we get here (possible future overhead?).
    my $ext = AmiGO::External::JSON->new();

    ##
    foreach my $relation (@$relations) {
      foreach my $genus (@$genera) {
	foreach my $target (@$targets) {

	  ## Assemble partial for storage.
	  my $assembly =
	    {
	     name => $name,
	     genus => $genus,
	     differentia => [{relation => $relation, target => $target}],
	    };

	  ## Grab new id from external toy id server.
	  my $data_to_send = $core->uri_safe($core->make_js($assembly));
	  my $url = $core->get_interlink({mode => 'id_request',
					  arg => {data => $data_to_send},
					  optional => {full => 1}});
	  my $jstr = $ext->get_external_data($url);
	  $core->kvetch("XPTR::mode_request_do: jstr: " . $jstr);

	  ## Make sure that it is legit and add it to the displayables.
	  my $jsblob = $ext->try();
	  if( $jsblob && $jsblob->{results} && $jsblob->{results}{new_id} ){

	    ## Assemble final for display.
	    my $new_id = $jsblob->{results}{new_id};
	    $core->kvetch("XPTR::mode_request_do: adding term id: " . $new_id);
	    $assembly->{id} = $new_id;
	    push @temp_terms, $assembly;

	    ## Email info.
	    ## TODO: see _email
	    #$self->_email($new_id . " : "  . $data_to_send);
	  }else{
	    die "KABLEWY--JSON RETURNED A DUD!";
	  }
	}
      }
    }
    $self->set_template_parameter('PROVISIONAL_TERMS', \@temp_terms);
    $self->add_template_content('html/main/xp_term_request/success.tmpl');
  }else{
    die "IMPROPER FORM INPUT WAS SUBMITTED!";
  }

  ###
  ### Templates and output.
  ###

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please
  my $prep =
    {
     css_library =>
     [
      'standard',
      'org.bbop.amigo.ui.autocomplete'
     ],
     javascript_library =>
     [
      'com.jquery',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.opensearch',
      'org.bbop.amigo.ui.autocomplete'
     ]
    };
  $self->add_template_bulk($prep);
  return $self->generate_template_page();
}


## TODO/BUG: if we're going to be serious here, the addresses need to
## be moved into config.pl.
sub _email {

  my $self = shift;
  my $body = shift || '';

  ## Try but who knows what machine we're on. Hopefully just ash.
  eval {
    my $email = Email::Simple->new('');
    $email->header_set('From', 'sjcarbon@ash.lbl.gov');
    $email->header_set('To', 'sjcarbon@lbl.gov');
    $email->header_set('Subject', 'XP Term Request');
    $email->body_set($body);

    my $message = $email->as_string;
    $core->kvetch("XPTR::_email: " . $message);

    #my $sender = Email::Send->new({mailer => 'Sendmail'});
    my $sender = Email::Send->new({mailer => 'SMTP'});
    $sender->mailer_args([Host => 'smtp.lbl.gov']);
    $sender->send($message);
  };
  if($@){
    ## Not gunna fight here.
  }
}



1;
