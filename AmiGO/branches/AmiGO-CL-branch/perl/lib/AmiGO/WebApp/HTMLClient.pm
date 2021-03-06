####
#### TODO/BUG: session_id needs to be stored as a cookie, caching reasons, etc.
####
#### TODO: replace internal $core calls with the one saved in
#### AmiGO::WebApp::$self as much as possible (save on things like
#### species caching, etc.)
####

package AmiGO::WebApp::HTMLClient;
use base 'AmiGO::WebApp';

##
use AmiGO::WebApp::Input;
use CGI::Application::Plugin::Session;
use CGI::Application::Plugin::TT;
use CGI::Application::Plugin::Redirect;
use Data::Dumper;

## Real external workers.
use AmiGO::Worker::GOlr::Term;
use AmiGO::Worker::GOlr::GeneProduct;
use AmiGO::External::QuickGO::Term;
use AmiGO::External::XML::GONUTS;
#use AmiGO::External::Raw;


##
sub setup {

  my $self = shift;

  $self->{STATELESS} = 0;

  ## Configure how the session stuff is going to be handled when and
  ## if it is necessary.
  $self->session_config(CGI_SESSION_OPTIONS =>
			["driver:File",
			 $self->query,
			 {Directory=>
			  $self->{CORE}->amigo_env('AMIGO_SESSIONS_ROOT_DIR')}
			],
			COOKIE_PARAMS => {-path  => '/'},
			SEND_COOKIE => 1);

  ## Templates.
  $self->tt_include_path($self->{CORE}->amigo_env('AMIGO_ROOT') .
			 '/templates/html');

  $self->mode_param('mode');
  $self->start_mode('software_list');
  $self->error_mode('mode_fatal');
  $self->run_modes(
		   'visualize'           => 'mode_visualize',
		   'software_list'       => 'mode_software_list',
		   'subset_summary'      => 'mode_subset_summary',
		   'live_search_gold'    => 'mode_live_search_gold',
		   'golr_term_details'   =>  'mode_golr_term_details',
		   'golr_gene_product_details' =>
		   'mode_golr_gene_product_details',
		   'css'                 => 'mode_dynamic_style',
		   'AUTOLOAD'            => 'mode_exception'
		  );
}


## This is just a very thin pass-through client.
## TODO/BUG: not accepting "inline" parameter yet...
sub mode_visualize {

  my $self = shift;
  my $output = '';

  ##
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('visualize');
  my $format = $params->{format};
  my $input_term_data_type = $params->{term_data_type};
  my $input_term_data = $params->{term_data};

  ## Cleanse input data of newlines.
  $input_term_data =~ s/\n/ /gso;

  ## If there is no incoming data, display the "client" page.
  ## Otherwise, forward to render app.
  if( ! defined $input_term_data ){

    ##
    $self->_common_params_settings({title=>'AmiGO: Visualization',
				    'amigo_mode' => 'visualize'});
    $self->add_template_content('pages/visualize.tmpl');
    $output = $self->generate_template_page();

  }else{

    ## Check to see if this JSON is even parsable...that's really all
    ## that we're doing here.
    if( $input_term_data_type eq 'json' ){
      eval {
	JSON::decode_json($input_term_data);
      };
      if ($@) {
	my $str = 'Your JSON was not formatted correctly...please go back and retry. Look at the "advanced format" documentation for more details.';
	#return $self->mode_die_with_message($str . '<br />' . $@);
	return $self->mode_die_with_message($str);
      }
    }

    ## TODO: Until I can think of something better...
    if( $format eq 'navi' ){

      ## BETA: Just try and squeeze out whatever I can.
      my $in_terms = $self->{CORE}->clean_term_list($input_term_data);
      my $jump = $self->{CORE}->get_interlink({mode=>'layers_graph',
				       arg => {
					       terms => $in_terms,
					      }});
      return $self->redirect($jump, '302 Found');
    }else{
      my $jump = $self->{CORE}->get_interlink({mode=>'visualize',
				       #optional => {url_safe=>1, html_safe=>0},
				       #optional => {html_safe=>0},
				       arg => {
					       format => $format,
					       data_type =>
					       $input_term_data_type,
					       data => $input_term_data,
					      }});
      #$self->{CORE}->kvetch("Jumping to: " . $jump);
      ##
      #$output = $jump;
      return $self->redirect($jump, '302 Found');
    }
  }

  return $output;
}


##
sub mode_software_list {

  my $self = shift;

  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile();

  $self->_common_params_settings({title=>'AmiGO: Software List'});

  ## Where would the ancient demos page hide...?
  my $foo = $self->{CORE}->amigo_env('AMIGO_CGI_PARTIAL_URL');
  $self->set_template_parameter('OLD_LOC', $foo);

  $self->add_template_content('pages/software_list.tmpl');
  return $self->generate_template_page();
}


## TODO/BUG: get new info to need this.
sub mode_dynamic_style {

  my $self = shift;
  $self->header_add( -type => 'text/css' ); #,-expires=>'+7d');
  my @dstack = ();

  ## TODO:
  #my $rg = $aid->species_information($aid->species_list({num_p => 1}));
  my $rg = {};
  foreach my $spc (keys %$rg){
    push @dstack, sprintf('.taxid_%s { background-color: %s }',
			  $spc, $rg->{$spc}{species_color});
  }

  return join("\n", @dstack);
}


## A committed client based on the jQuery libraries and GOlr. The
## future.
sub mode_live_search_gold {

  my $self = shift;

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_CSS', 'no');

  ## Grab resources we want.
  $self->set_template_parameter('STAR_IMAGE',
				$self->{CORE}->get_image_resource('star'));

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      'standard',
      #'com.jquery.redmond.custom',
      'com.jquery.jqamigo.custom',
      'bbop.amigo.ui.widgets'
      #'bbop.amigo.ui.interactive'
     ],
     javascript_library =>
     [
      'com.jquery',
      'com.jquery-ui',
      'bbop.core',
      'bbop.amigo',
      'bbop.amigo.go_meta',
      #'bbop.amigo.live_search',
      'bbop.amigo.ui.widgets',
      'bbop.amigo.ui.interactive'
     ],
     javascript =>
     [
      $self->{JS}->get_lib('LiveSearchGOlr.js')
     ],
     javascript_init =>
     [
      'LiveSearchGOlrInit();'
     ],
     content =>
     [
      'pages/live_search_gold.tmpl'
     ]
    };
  $self->add_template_bulk($prep);

  return $self->generate_template_page();
}


## Experimental try at the term details page, in perl, backed by the
## solr index.
sub mode_golr_term_details {

  my $self = shift;

  ##
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('term');
  my $input_term_id = $params->{term};
  $self->_common_params_settings({'title' =>
				  'AmiGO: Term Details for ' . $input_term_id});

  ## Input sanity check.
  if( ! $input_term_id ){
    return $self->mode_die_with_message("Term acc could not be found! Is it".
					" possible that what you're looking".
					" for is not a term acc?");
  }

  ###
  ### Get full term info.
  ###

  my $term_worker = AmiGO::Worker::GOlr::Term->new($input_term_id);
  my $term_info_hash = $term_worker->get_info();
  if( ! defined($term_info_hash) ||
      $self->{CORE}->empty_hash_p($term_info_hash) ){
    return $self->mode_die_with_message("Term acc could not be found" .
					" in the index!");
  }

  $self->{CORE}->kvetch('solr docs: ' . Dumper($term_info_hash));

  ## Should just be one now, yeah?
  #my $foo = (keys %$term_info_hash)[0];
  #$self->{CORE}->kvetch('$term_info: ' . Dumper($term_info->{$foo}));
  $self->set_template_parameter('TERM_INFO',
				$term_info_hash->{$input_term_id});

  ## First switch on term vs. subset.
  my $is_term_acc_p = $self->{CORE}->is_term_acc_p($input_term_id);
  my $acc_list_for_gpc_info = [];
  my $input_term_id_list = [];
  if( $is_term_acc_p ){

    $self->{CORE}->kvetch('Looks like a term acc...');

    #   ## Even if just a single acc, put it into list form--that's what
    #   ## we'll be using.
    #   $input_term_id_list = [$input_term_id];
    #   push @$acc_list_for_gpc_info, $input_term_id;

  }else{

    $self->{CORE}->kvetch('Looks like a subset acc...');

    #   ## Convert input subset acc to term accs.
    #   my $sget = AmiGO::Worker::Subset->new();
    #   my @subset_term_list = keys(%{$sget->get_term_accs($input_term_id)});
    #   foreach my $k (@subset_term_list){
    #     push @$input_term_id_list, $k;
    #     push @$acc_list_for_gpc_info, $k;
    #   }
  }

  # ###
  # ### Get neighborhood below term.
  # ###

  # ## Note: won't be included in subset case (too messy), so don't
  # ## push.
  # if( $is_term_acc_p ){
  #   my $sorted_child_chunks = $term_q->get_child_info($input_term_id_list);
  #   #$self->{CORE}->kvetch('scc: ' . Dumper($sorted_child_chunks));
  #   foreach my $cinfo (@$sorted_child_chunks){ 
  #     push @$acc_list_for_gpc_info, $cinfo->{acc};
  #   }
  #   $self->set_template_parameter('CHILD_CHUNKS', $sorted_child_chunks);
  # }

  # ###
  # ### Get term ancestor information.
  # ###

  # #$self->{CORE}->kvetch("input_term_id_list" . Dumper($input_term_id_list));

  # ##
  # my $anc_info = undef;
  # if( $is_term_acc_p ){
  #   $anc_info = $term_q->get_ancestor_info($input_term_id_list);
  # }else{
  #   ## We want to include self in ancestors in this case.
  #   $anc_info = $term_q->get_ancestor_info($input_term_id_list, {reflexive=>1});
  # }
  # $self->set_template_parameter('MAX_DEPTH', $anc_info->{max_depth});
  # $self->set_template_parameter('MAX_DISPLACEMENT',
  # 				$anc_info->{max_displacement});
  # $self->set_template_parameter('PARENT_CHUNKS_BY_DEPTH',
  # 				$anc_info->{parent_chunks_by_depth});
  # push @$acc_list_for_gpc_info, @{$anc_info->{seen_acc_list}};

  # ## Now that we have all accs that we want counts for, create a
  # ## mapping between terms and a random address.
  # my $rand_to_acc = {};
  # my $acc_to_rand = {};
  # for( my $i = 0; $i < scalar(@$acc_list_for_gpc_info); $i++ ){
  #   my $acc = $acc_list_for_gpc_info->[$i];
  #   my $rand = $self->{CORE}->unique_id();
  #   $rand_to_acc->{$rand} = $acc;
  #   $acc_to_rand->{$acc} = $rand;
  # }

  # $self->set_template_parameter('ACC_TO_RAND', $acc_to_rand);
  # $self->set_template_parameter('RAND_TO_ACC', $rand_to_acc);

  # ###
  # ### Pull gene_product_count info.
  # ###

  # #print STDERR "<<TIME_START>>\n";
  # ## TODO/BUG: If nothing explodes, memoize this sucker a la Visualize:
  # my $gpc_q = AmiGO::Worker::GeneProductCount->new($input_term_id,
  #                                                  $acc_list_for_gpc_info);
  # #print STDERR "<<TIME_MID>>\n";
  # my $gpc_info = $gpc_q->get_info();

  # ## Get total counts for all terms (to use in fallback cases where JS
  # ## is not enabled).
  # my $gpc_total_count = {};
  # foreach my $acc (@$acc_list_for_gpc_info){
  #   $gpc_total_count->{$acc} = $gpc_q->get_count($acc);
  # }
  # $self->set_template_parameter('GPA_COUNTS', $gpc_total_count);
  # $self->set_template_parameter('GENE_PRODUCT_ASSOCIATIONS_COUNT',
  # 				$gpc_total_count->{$input_term_id});

  ## Bridge variables from old system.
  #$self->set_template_parameter('cgi', 'term-details');
  $self->set_template_parameter('cgi', 'browse');
  $self->set_template_parameter('vbridge', 'term=' . $input_term_id);

  # ## These things are of limited use to subsets.
  # if( $is_term_acc_p ){

  ###
  ### External links.
  ###

  $self->set_template_parameter('GENE_PRODUCT_ASSOCIATIONS_LINK',
				$self->{CORE}->get_interlink({mode => 'term-assoc',
							      arg =>
							      {acc =>
							       $input_term_id}}));
  $self->set_template_parameter('VIZ_STATIC_LINK',
				$self->{CORE}->get_interlink({mode => 'visualize',
							      arg =>
							      {data => $input_term_id,
							       format => 'png'}}));
  $self->set_template_parameter('VIZ_DYNAMIC_LINK',
				$self->{CORE}->get_interlink({mode => 'visualize',
							      arg =>
							      {data => $input_term_id,
							       format => 'svg'}}));
  $self->set_template_parameter('NAVIGATION_LINK',
				$self->{CORE}->get_interlink({mode => 'layers_graph',
							      arg =>
							      {terms =>
							       $input_term_id}}));

  $self->set_template_parameter('VIZ_QUICKGO_LINK',
				$self->{CORE}->get_interlink({mode=>'visualize_simple',
							      arg =>
							      {engine=>'quickgo',
							       term =>
							       $input_term_id}}));

  my $qg_term = AmiGO::External::QuickGO::Term->new();
  $self->set_template_parameter('QUICKGO_TERM_LINK',
				$qg_term->get_term_link($input_term_id));

  $self->set_template_parameter('QUICKGO_ENGINE_P',
				$self->{CORE}->amigo_env('AMIGO_GO_ONLY_GRAPHICS'));

  ###
  ### GONUTs
  ###

  ## TODO: I'd like to be able to set this up for some trivial GONUTS
  ## kappa tests.
  ## GONuts query.
  ## Cutoff a year ago (in seconds).
  ## TODO: we should compact this into a worker now that we have a chance.
  my $gonuts = AmiGO::External::XML::GONUTS->new({cutoff_time => 31536000});
  my $answer_p = $gonuts->query_term($input_term_id);
  $self->set_template_parameter('GONUTS_SUCCESS', 0);
  if( $answer_p ){
    $self->set_template_parameter('GONUTS_SUCCESS', 1);
    $self->set_template_parameter('GONUTS_TOTAL_COUNT',
				  $gonuts->get_total_count());
    $self->set_template_parameter('GONUTS_RECENT_COUNT',
				  $gonuts->get_recent_count());
    $self->set_template_parameter('GONUTS_PAGE_TITLE',
				  $gonuts->get_page_title());
    $self->set_template_parameter('GONUTS_PAGE_URL',
				  $gonuts->get_page_url());
    $self->set_template_parameter('GONUTS_DATE_STRING',
				  $gonuts->get_date_string());

      # ## DEBUG
      # $gonuts->kvetch('GONUTS: got an answer:');
      # $gonuts->kvetch("\t" . $gonuts->get_total_count());
      # $gonuts->kvetch("\t" . $gonuts->get_recent_count());
      # $gonuts->kvetch("\t" . $gonuts->get_page_title());
      # $gonuts->kvetch("\t" . $gonuts->get_page_url());
  }

  # }else{

  #   ## It'll be good to differentiate subset stuff from the οἱ πολλοί.
  #   my %in_term_hash = map { $_ => 1 } @$input_term_id_list;
  #   $self->set_template_parameter('SUBSET_TERMS', \%in_term_hash);

  #   $self->set_template_parameter('VIZ_STATIC_LINK',
  #     $self->{CORE}->get_interlink({mode => 'visualize_subset',
  # 				    arg => {subset => $input_term_id}}));
  # }

  ###
  ### Standard setup.
  ### TODO: We see this a lot--should this be abstracted out too? No?
  ###

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      # 'standard', # basic GO-styles
      # 'bbop.amigo.ui.autocomplete'
      'standard', # basic GO-styles
      'com.jquery.jqamigo.custom',
      #'com.jquery.tablesorter',
      'bbop.amigo.ui.widgets'
     ],
     javascript_library =>
     [
      'com.jquery',
      'com.jquery-ui',
      'com.jquery.tablesorter',
      'bbop.core',
      'bbop.amigo',
      'bbop.amigo.go_meta',
      'bbop.amigo.ui.widgets'
     ],
     javascript =>
     [
      # $self->{JS}->make_var('global_count_data', $gpc_info),
      # $self->{JS}->make_var('global_rand_to_acc', $rand_to_acc),
      # $self->{JS}->make_var('global_acc_to_rand', $acc_to_rand),
      $self->{JS}->make_var('global_acc', $input_term_id)
     ]
    };
  $self->add_template_bulk($prep);

  ## Initialize javascript app.
  $self->add_template_javascript($self->{JS}->get_lib('TermDetails.js'));
  $self->add_template_javascript($self->{JS}->initializer_jquery('TermDetailsInit();'));

  ##
  ## These things are of limited use to subsets.
  # if( $is_term_acc_p ){
    # if( $type eq 'compact' ){
    #   $self->add_template_content('html/main/term_details_compact.tmpl');
    # }else{
      $self->add_template_content('pages/term_details.tmpl');
    # }
  # }else{
  #   $self->add_template_content('html/main/subset_details.tmpl');
  # }

  return $self->generate_template_page();
}


## Experimental try at the gp details page, in perl, backed by the
## solr index.
sub mode_golr_gene_product_details {

  my $self = shift;

  ##
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('gp');
  my $input_gp_id = $params->{gp};
  $self->_common_params_settings({'title' =>
				  'AmiGO: Gene Product Details for ' .
				  $input_gp_id});

  ## Input sanity check.
  if( ! $input_gp_id ){
    return $self->mode_die_with_message("GP acc could not be found! Is it".
					" possible that what you're looking".
					" for is not a GP acc?");
  }

  ###
  ### Get full gp info.
  ###

  my $gp_worker = AmiGO::Worker::GOlr::GeneProduct->new($input_gp_id);
  my $gp_info_hash = $gp_worker->get_info();
  if( ! defined($gp_info_hash) || $self->{CORE}->empty_hash_p($gp_info_hash) ){
    return $self->mode_die_with_message("GP acc could not be found" .
					" in the index!");
  }

  $self->{CORE}->kvetch('solr docs: ' . Dumper($gp_info_hash));
  $self->set_template_parameter('GP_INFO', $gp_info_hash->{$input_gp_id});

  ###
  ### TODO: pull in additional annotation, etc. info.
  ###

  ###
  ### Standard setup.
  ###

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      # 'standard', # basic GO-styles
      # 'bbop.amigo.ui.autocomplete'
      'standard', # basic GO-styles
      'com.jquery.jqamigo.custom',
      #'com.jquery.tablesorter',
      'bbop.amigo.ui.widgets'
     ],
     javascript_library =>
     [
      'com.jquery',
      'com.jquery-ui',
      'com.jquery.tablesorter',
      'bbop.core',
      'bbop.amigo',
      'bbop.amigo.go_meta',
      'bbop.amigo.ui.widgets'
     ],
     javascript =>
     [
      # $self->{JS}->make_var('global_count_data', $gpc_info),
      # $self->{JS}->make_var('global_rand_to_acc', $rand_to_acc),
      # $self->{JS}->make_var('global_acc_to_rand', $acc_to_rand),
      $self->{JS}->make_var('global_acc', $input_gp_id)
     ]
    };
  $self->add_template_bulk($prep);

  # ## Initialize javascript app.
  # $self->add_template_javascript($self->{JS}->get_lib('GPDetails.js'));
  # $self->add_template_javascript($self->{JS}->initializer_jquery('GPDetailsInit();'));

  $self->add_template_content('pages/gene_product_details.tmpl');

  return $self->generate_template_page();
}



1;
