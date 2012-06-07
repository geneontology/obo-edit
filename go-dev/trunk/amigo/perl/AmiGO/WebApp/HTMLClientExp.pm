package AmiGO::WebApp::HTMLClientExp;
use base 'AmiGO::WebApp';

####
#### Place to keep experimntal things without polluting the main
#### app. Also makes installation of main and exp separate.
####

## Bring in necessaries.
use strict;
use utf8;
use JSON;
use AmiGO::JSON;
use AmiGO::JavaScript;
use AmiGO::External::XML::GONUTS;
use AmiGO::External::Raw;
use AmiGO::External::QuickGO::Term;
use AmiGO::External::JSON::LiveSearch::Term;
use AmiGO::External::JSON::LiveSearch::GeneProduct;
use AmiGO::External::LEAD::Query;
#use mapscript;

# ## Take SuGR from a test drive.
# use SuGR::Render;
# use SuGR::Partition;
# use Graph::Directed;
# use SuGR::BaryMatrix;
# use SuGR::Sugiyama;

use Data::Dumper;

use File::Basename;

#use CGI::Application::Plugin::DBH (qw/dbh_config dbh/);
use CGI::Application::Plugin::Session;
use CGI::Application::Plugin::TT;

use AmiGO::WebApp::Input;
use GOBO::DBIC::GODBModel;
use GOBO::DBIC::GODBModel::Graph;
use GOBO::DBIC::GODBModel::Query;
use GOBO::DBIC::GODBModel::Schema;

use AmiGO::Aid::ReferenceGenome;

## Real external workers.
#use AmiGO::Worker::HomolsetGraph;
#use AmiGO::Worker::HomolsetGraph2;
#use AmiGO::Worker::ExpSearch;
#use AmiGO::Worker::Matrix;
#use AmiGO::Worker::LexicalSearch;
#use AmiGO::Worker::Intersection;
use AmiGO::Cache::ReferenceGenome;
use AmiGO::Worker::NMatrix;

use AmiGO::Worker::Term;
use AmiGO::Worker::GeneProduct;
use AmiGO::Worker::GeneProductCount;
use AmiGO::Worker::PANTHERTree;

use AmiGO::Worker::Solr::Term;
use AmiGO::Worker::Solr::GeneProduct;

# use Cache::Memcached; # TODO: can't go bigger than 1MB (still,
#                       # probably best to explore);
#use Cache::FileCache; # can't do complex objects.
#use FreezeThaw qw(freeze thaw); # infinite recur?

## Ghost workers--just templates.
#use AmiGO::Worker::SoftwareList;
#use AmiGO::Worker::ORBClient;

## Helper helping.
my $rg_aid = AmiGO::Aid::ReferenceGenome->new();


##
sub setup {

  my $self = shift;

  $self->{STATELESS} = 0;

  ## Configure how the session stuff is going to be handled when and
  ## if it is necessary.
  $self->session_config(
			CGI_SESSION_OPTIONS => [
						"driver:File",
						$self->query,
						{Directory=> $self->{CORE}->amigo_env('AMIGO_SESSIONS_ROOT_DIR')}
					       ],
			COOKIE_PARAMS       => {
						-path  => '/',
					       },
			SEND_COOKIE         => 1,
 );

  $self->tt_config(TEMPLATE_OPTIONS =>
		   {INCLUDE_PATH =>
		    $self->{CORE}->amigo_env('GO_DEV_ROOT') .
		    '/amigo/amigo/templates'});

  $self->mode_param('mode');
  $self->start_mode('kick_to_main');
  $self->error_mode('mode_fatal');
  #$self->tt_include_path('templates/html/inc');
  $self->run_modes(
		   ## Client apps.
		   'matrix'              => 'mode_matrix',
		   'nmatrix'             => 'mode_nmatrix',
		   'live_search_term'    => 'mode_live_search_term',
		   'live_search_gene_product'=>'mode_live_search_gene_product',
		   'ntree'               => 'mode_ntree',
		   'ptree'               => 'mode_ptree',
		   #'term'                => 'mode_term',
		   'scratch'             => 'mode_scratch',
		   'gp_with_2_terms'     => 'mode_gp_with_2_terms',
		   'autocomplete_client' => 'mode_autocomplete_client',
		   #'exp_search'          => 'mode_exp_search',
		   'knomigo'             => 'mode_knomigo',
		   'workspace_client'    => 'mode_workspace_client',
		   'layers_graph'        => 'mode_layers_graph',
		   'hierarchical'        => 'mode_hierarchical',
		   'heavy_client_ext'    => 'mode_heavy_client_ext',
		   'heavy_client_jquery' => 'mode_heavy_client_jquery',
		   'homolset_summary'    => 'mode_homolset_summary',

		   'golr_term_details'   =>  'mode_golr_term_details',
		   'golr_gene_product_details' =>
		   'mode_golr_gene_product_details',

		   #'report_1'            => 'mode_report_1',
		   #'lexical_search'      => 'mode_lexical_search',
		   #'exp_lexical_search'  => 'mode_exp_lexical_search',
		   #'orb'                 => 'mode_orb',
		   #'coannot_matrix'      => 'mode_coannot_matrix',
		   #'orb_client'          => 'mode_orb_client',
		   'exhibit'             => 'mode_exhibit_exp',

		   'report_slimmerish_1' => 'mode_report_slimmerish_1',

		   ## Replacements.
		   'front_page'          => 'mode_front_page',

		   ## Service apps.
		   'workspace'           => 'mode_workspace',

		   ## System apps.
		   'kick_to_main'        => 'mode_kick_to_main',
		   'AUTOLOAD'            => 'mode_exception'
		  );
}


##
sub mode_live_search_term {

  my $self = shift;

  ## This bit is (and should be) a direct lift from Services. Since
  ## we're low speed, packets will not be needed.
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('live_search_term');
  my $query = $params->{query};
  my $index = $params->{index} + 0; # coerce to int?
  my $count = $params->{count} + 0; # coerce to int?
  #my $packet = $params->{packet} + 0; # coerce to int?
  my $ontology = $params->{ontology};
  $self->{CORE}->kvetch("query: ". $query);
  $self->{CORE}->kvetch("index: ". $index);
  $self->{CORE}->kvetch("count: ". $count);
  #$self->{CORE}->kvetch("packet: ". $packet);
  $self->{CORE}->kvetch("ontology: ". $ontology);

  #$self->{CORE}->kvetch("page: ". $page);
  $self->set_template_parameter('SEARCHED_P', 0);

  if( $query && length($query) < 3 ){
    $self->mode_die_with_message('You need a query of at least' .
				 ' three characters.');
  }elsif( $query ){

    my $args_hash =
      {
       query => $query,
       index => $index,
       count => $count,
       ontology => $ontology,
      };
    my $tq = AmiGO::External::JSON::LiveSearch::Term->new();
    my $results = $tq->query($args_hash);
    my $next_url = $tq->next_url($args_hash);
    my $prev_url = $tq->previous_url($args_hash);

    ## Flag to let the template know that we got results.
    $self->set_template_parameter('SEARCHED_P', 1);

    $self->{CORE}->kvetch(Dumper($results));

    ## Add them into the parameters.
    $self->set_template_parameter('RESULTS', $results);
    $self->set_template_parameter('RESULTS_LIST', $results->{results}{hits});
    $self->set_template_parameter('RESULTS_TOTAL',
				  $results->{results}{meta}{total});
    $self->set_template_parameter('RESULTS_FIRST',
				  $results->{results}{meta}{first});
    $self->set_template_parameter('RESULTS_LAST',
				  $results->{results}{meta}{last});

    $self->set_template_parameter('NEXT_LINK', $next_url);
    $self->set_template_parameter('PREV_LINK', $prev_url);
  }

  ###
  ###
  ###

  ##
  $self->set_template_parameter('selected_ontology_hash',
				$self->{CORE}->to_hash($ontology));
  $self->set_template_parameter('ontology_hash', $self->{CORE}->ontology());

  ## 
  $self->set_template_parameter('query', $self->{CORE}->html_safe($query));
  $self->add_template_content('html/main/live_search_term.tmpl');
  return $self->generate_template_page();
}


## NOTE/WARNING: We're just going to ignore the homolset args for this
## one (unlike the real live version)--I'm not sure they really add
## much here.
sub mode_live_search_gene_product {

  my $self = shift;

  ## This bit is (and should be) a direct lift from Services. Since
  ## we're low speed, packets will not be needed.
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('live_search_gene_product');
  my $query = $params->{query};
  my $index = $params->{index} + 0; # coerce to int?
  my $count = $params->{count} + 0; # coerce to int?
  my $species = $params->{species};
  my $source = $params->{source};
  my $gptype = $params->{gptype};
  $self->{CORE}->kvetch("query: ". $query);
  $self->{CORE}->kvetch("index: ". $index);
  $self->{CORE}->kvetch("count: ". $count);
  $self->{CORE}->kvetch("species: ". $species);
  $self->{CORE}->kvetch("source: ". $source);
  $self->{CORE}->kvetch("gptype: ". $gptype);

  #$self->{CORE}->kvetch("page: ". $page);
  $self->set_template_parameter('SEARCHED_P', 0);

  if( $query && length($query) < 3 ){
    $self->mode_die_with_message('You need a query of at least' .
				 ' three characters.');
  }elsif( $query ){

    ## Flag to let the template know that we got results.
    $self->set_template_parameter('SEARCHED_P', 1);
    ## Flag to say that we didn't barf...until proven otherwise.
    $self->set_template_parameter('SUCCESS_P', 1);

    my $args_hash =
      {
       query => $query,
       index => $index,
       count => $count,
       species => $species,
       source => $source,
       gptype => $gptype,
      };
    my $tq = AmiGO::External::JSON::LiveSearch::GeneProduct->new();
    my $results = $tq->query($args_hash);

    ## Try and describe a fail mode, but let everything else run as
    ## normal. Work it out in the template.
    if( ! $results->{success}){
      my $error = 'There was a problem: please try again or modify your query,';
      ## To get a non-generic error, cut things out after any newline.
      if( $results->{errors} && scalar(@{$results->{errors}}) > 0 ){
	my $raw_doc = $results->{errors}[0];
	my @raw_doc_split = split(/\n+/, $raw_doc);
	$error = $raw_doc_split[0];
	chomp($error);
      }
      $self->{CORE}->kvetch("error message: " . $error);
      $self->set_template_parameter('SUCCESS_P', 0);
      $self->set_template_parameter('ERROR_MESSAGE', $error);
    }

    ##
    my $next_url = $tq->next_url($args_hash);
    my $prev_url = $tq->previous_url($args_hash);

    # $self->{CORE}->kvetch(Dumper($results));

    ## Add them into the parameters.
    $self->set_template_parameter('RESULTS', $results);
    $self->set_template_parameter('RESULTS_LIST', $results->{results}{hits});
    $self->set_template_parameter('RESULTS_TOTAL',
				  $results->{results}{meta}{total});
    $self->set_template_parameter('RESULTS_FIRST',
				  $results->{results}{meta}{first});
    $self->set_template_parameter('RESULTS_LAST',
				  $results->{results}{meta}{last});

    $self->set_template_parameter('NEXT_LINK', $next_url);
    $self->set_template_parameter('PREV_LINK', $prev_url);
  }

  ###
  ###
  ###

  ## Selected hashes (what came in).
  $self->set_template_parameter('selected_species_hash',
				$self->{CORE}->to_hash($species));
  $self->set_template_parameter('selected_source_hash',
				$self->{CORE}->to_hash($source));
  $self->set_template_parameter('selected_gptype_hash',
				$self->{CORE}->to_hash($gptype));

  ## Form hashes.
  $self->set_template_parameter('species_hash', $self->{CORE}->species());
  $self->set_template_parameter('source_hash', $self->{CORE}->source());
  $self->set_template_parameter('gptype_hash', $self->{CORE}->gptype());

  ## 
  $self->set_template_parameter('query', $self->{CORE}->html_safe($query));
  $self->add_template_content('html/main/live_search_gene_product.tmpl');
  return $self->generate_template_page();
}


# ## Maybe how things should look in this framework?
# sub mode_exp_search {

#   my $self = shift;

#   my $i = AmiGO::WebApp::Input->new();
#   my $params = $i->input_profile('exp_search');

#   ## TODO: add error checking for invalid inputs.
#   my $query = $params->{query};
#   my $type = $params->{type};
#   my $page = $self->{CORE}->atoi($params->{page});

#   $self->{CORE}->kvetch("page: ". $page);
#   $self->set_template_parameter('SEARCHED_P', 0);

#   if( $query && $type){

#     my $search = AmiGO::Worker::ExpSearch->new($type);
#     my $results = $search->query($query, $page);

#     ##
#     # my ($search_total,$search_first, $search_current, $search_last) =
#     #   $search->page_info();
#     # $self->set_template_parameter('SEARCH_TOTAL', $search_total);
#     # $self->set_template_parameter('SEARCH_FIRST', $search_first);
#     # $self->set_template_parameter('SEARCH_CURRENT', $search_current);
#     # $self->set_template_parameter('SEARCH_LAST', $search_last);

#     $self->set_template_parameter('NEXT_LINK',
# 				  $self->{CORE}->get_interlink({mode=>'exp_search',
# 							arg=>{
# 							      type=>'gp',
# 							      page=>$page +1,
# 							      query=>$query,
# 							     }}));
#     $self->set_template_parameter('PREV_LINK',
# 				  $self->{CORE}->get_interlink({mode=>'exp_search',
# 							arg=>{
# 							      type=>'gp',
# 							      page=>$page -1,
# 							      query=>$query,
# 							     }}));

#     if( ! $results ){
#       $self->mode_die_with_message('We seem to have gotten bad input!');
#     }else{

#       ## Flag to let the template know that we got results.
#       $self->set_template_parameter('SEARCHED_P', 1);

#       ## Add them into the parameters.
#       $self->set_template_parameter('SEARCHED_RESULTS', $results);
#     }
#   }

#   $self->set_template_parameter('query', $self->{CORE}->html_safe($query));
#   $self->set_template_parameter('title',
# 				'AmiGO: Term Search');
#   $self->add_template_content('html/main/exp_search.tmpl');
#   return $self->generate_template_page();
# }


## While this is really a service and should may exist with aserve, it
## does do sessioning, so needs the STATELESS = 0, so I'll keep it
## here until fully tested and then spin it out into a stateful
## session service or something.
## TODO: This will actually evolve into the session and piping module.
sub mode_workspace {

  my $self = shift;
  my $json_resp = AmiGO::JSON->new('workspace');

  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('workspace');

  my $retstruct = {};

  ## Attempt all operations and save failure messages for later use.
  my $ws = $params->{workspace};
  my $action = $params->{action};
  if( $action eq 'add_workspace' ){

    $ws =~ s/\ /\_/g; # spaces to underscore
    $self->session_db_add_workspace($ws)
      || $json_resp->add_error('could not add workspace');

  }elsif( $action eq 'copy_workspace' ){

    ## Non-trivial operation. Only perform if everything is defined.
    my $ws_from = $params->{workspace} || undef;
    my $ws_to = $params->{copy_to_workspace} || undef;
    if( ! defined $ws_from ){
      $json_resp->add_error('using an undefined source workspace');
    }elsif( ! defined $ws_to ){
      $json_resp->add_error('using an undefined destination workspace');
    }else{
      $self->session_db_copy_workspace($ws_from, $ws_to)
	|| $json_resp->add_error('could not copy workspace');
    }

  }elsif( $action eq 'clear_workspace' ){

    $self->session_db_clear_workspace($ws)
      || $json_resp->add_error('could not clear workspace');

  }elsif( $action eq 'remove_workspace' ){

    $self->session_db_remove_workspace($ws)
      || $json_resp->add_error('could not remove workspace');

  }elsif( $action eq 'add_item' ){

    ## Non-trivial operation. Only perform if everything is defined.
    my $key = $params->{key};
#     my $type = $params->{type};
    my $name = $params->{name} || '';
    if( ! defined $key || ! $key ){
      $json_resp->add_error('key not defined');
    }elsif( ! defined $ws || ! $ws ){
      $json_resp->add_error('type not defined');
#     }elsif( ! defined $type || ! $type ){
#       $json_resp->add_error('undefined type');
    }else{
      $self->session_db_add_item({
				  key => $key,
# 				  type => $type,
				  name => $name,
				 },
				 $ws)
	|| $json_resp->add_error('could not add item');
    }

  }elsif( $action eq 'remove_item' ){

    ## Non-trivial operation. Only perform if everything is defined.
    my $key = $params->{key};
    if( ! defined $key || ! $key ){
      $json_resp->add_error('undefined key');
    }elsif( ! defined $ws || ! $ws ){
      $json_resp->add_error('undefined workspace');
    }else{
      $self->session_db_remove_item($key, $ws)
	|| $json_resp->add_error('could not remove item');
    }

  }elsif( $action eq 'list_workspaces' ){

    ##
    foreach my $ws_name (@{$self->session_db_list_workspaces()}){
      $retstruct->{$ws_name} = [];
    }

  }elsif( $action eq 'list_items' ){

    ##
    $retstruct->{$ws} = [];
    foreach my $item (@{$self->session_db_list_workspace_items($ws)}){
      push @{$retstruct->{$ws}}, $item;
    }

  }else{

    ## Our action is probably list/status then...
    foreach my $ws_name (@{$self->session_db_list_workspaces()}){
      $retstruct->{$ws_name} = [];
      foreach my $item (@{$self->session_db_list_workspace_items($ws_name)}){
	push @{$retstruct->{$ws_name}}, $item;
      }
    }

  }

  ##
  $json_resp->set_results($retstruct);

  ##
  $self->header_add( -type => 'application/json' );
  return $json_resp->make_js();
}


##
sub mode_front_page {

  my $self = shift;

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please
  $self->set_template_parameter('page_name', 'front'); # mimic front page

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      'standard', # basic GO-styles
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

  ## Initialize javascript app.
  $self->add_template_javascript($self->{JS}->initializer_jquery('new org.bbop.amigo.ui.autocomplete({id:"query", search_type:"general", completion_type:"acc", jump: true});'));

  ##
  $self->add_template_content('html/main/front_page.tmpl');
  return $self->generate_template_page();
}


##
sub mode_hierarchical {

  my $self = shift;

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please
  $self->set_template_parameter('page_name', 'Hierarchical');

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      'standard' # basic GO-styles
     ],
     javascript_library =>
     [
      'com.jquery',
#      'com.jquery.treeview',
#      'com.jquery.treeview.async',
      'org.bbop.amigo',
      'org.bbop.amigo.service.ontology',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.ui.tree'
     ]
    };
  $self->add_template_bulk($prep);

  ## Initialize javascript app.
  $self->add_template_javascript($self->{JS}->initializer_jquery('new org.bbop.amigo.ui.tree({});'));

  ##
  $self->add_template_content('<div id="tree"></div>');

  return $self->generate_template_page({lite => 1});
}


##
sub mode_workspace_client {

  my $self = shift;

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      'standard', # basic GO-styles
      #'org.bbop.amigo.ui.autocomplete',
      'org.bbop.amigo.ui.widgets',
      'com.jquery.redmond.custom'
     ],
     ## Our AmiGO services JSS.
     javascript_library =>
     [
      'com.jquery',
      'com.jquery-ui',
      'com.jquery-layout',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      #'org.bbop.amigo.opensearch',
      #'org.bbop.amigo.ui.autocomplete',
      'org.bbop.amigo.workspace',
      'org.bbop.amigo.ui.widgets'
     ],
     content =>
     [
      '<div class="ui-layout-north">',
      'includes/header.tmpl',
      '</div>',
      '<div class="ui-layout-center">',
      'html/main/workspace_client.tmpl',
      '</div>',
      '<div class="ui-layout-south">',
      'includes/footer.tmpl',
      '</div>'
     ],
    };
  $self->add_template_bulk($prep);

  ## Our client JS, and init.
  $self->add_template_javascript($self->{JS}->get_lib('WorkspaceClient.js'));

  ## Initialize javascript app.
  $self->add_template_javascript($self->{JS}->initializer_jquery('WorkspaceClientInit();'));

  ##
  return $self->generate_template_page();
}


##
sub mode_heavy_client_ext {

  my $self = shift;

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please

  ## Our AmiGO services CSS.
  my $prep = 
    {
     css_library =>
     [
      'standard', # basic GO-styles
      'com.ext.resources.ext-all'
      #  $self->{CORE}->amigo_env('AMIGO_HTML_URL') .
      #       '/js/com/ext/resources/css/xtheme-slate.css';
     ],
     javascript_library =>
     [
      'com.ext',
      'com.ext-core',
      'com.ext-all',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.live_search.gene_product',
      'org.bbop.amigo.workspace'
     ],
     content =>
     [
      '<div id="wrapping_north_header">',
      'includes/header.tmpl',
      '</div>',
      'html/main/heavy_client_ext.tmpl',
      'includes/footer.tmpl'
     ],
    };
  $self->add_template_bulk($prep);

  ## Our client JS, and init.
  $self->add_template_javascript($self->{JS}->get_lib('HeavyClientExt.js'));

  ## Initialize javascript app.
  $self->add_template_javascript($self->{JS}->initializer_ext('HeavyClientInit();'));

  ## TODO: make north the standard AmiGO header.
  return $self->generate_template_page();
}


## A client based on the jQuery libraries. So far, the front runner.
sub mode_heavy_client_jquery {

  my $self = shift;

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please
  $self->set_template_parameter('page_content_layout', 'wide');

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      'standard',
      'com.jquery.redmond.custom',
      'org.bbop.amigo.ui.widgets'
     ],
     javascript_library =>
     [
      'com.jquery',
      'com.jquery-ui',
      'com.jquery-layout',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.live_search',
      'org.bbop.amigo.workspace',
      'org.bbop.amigo.item',
      'org.bbop.amigo.ui.widgets',
      'org.bbop.amigo.ui.cart'
     ],
     javascript =>
     [
      $self->{JS}->get_lib('HeavyClientJQuery.js')
     ],
     javascript_init =>
     [
      'HeavyClientInit();'
     ],
     content =>
     [
      '<div class="ui-layout-north">',
      'includes/header.tmpl',
      '</div>',
      'html/main/heavy_client_jquery.tmpl',
      '<div class="ui-layout-south">',
      'includes/footer.tmpl',
      '</div>'
     ]
    };
  $self->add_template_bulk($prep);

  return $self->generate_template_page();
}


##
sub mode_homolset_summary {

  my $self = shift;

  ## Deal with input.
  my $i = AmiGO::WebApp::Input->new();
  # my $params = $i->input_profile('homolset_summary');
  my $params = $i->input_profile();
  $self->set_template_parameter('STANDARD_YUI', 'no');
  $self->set_template_parameter('title',
				'AmiGO: Experimental Homolog Set Summary');

  ###
  ### Bring in all of the data.
  ###

  ## Since most of what we want is in the cache, be careful with it.
  my $cache = undef;
  eval{
    $cache = AmiGO::Cache::ReferenceGenome->new();
  };
  if( $@ ){
    $self->mode_fatal_with_message("Couldn't find the RG data cache: $@");
  }elsif( ! defined $cache ){
    $self->mode_fatal_with_message("The RG data cache appears to be null.");
  }elsif( ! $cache->test() ){
    $self->mode_fatal_with_message("The RG data cache appears to be bad.");
  }else{

    my $summary = $cache->summary();
    my $species = $cache->species();
    my $reference_genome_sets = $cache->reference_genome_sets();
    my $gene_products = $cache->gene_products();

    ## Add links to the summary (not going to put them into the cache
    ## to keep transportability).
    # $self->{CORE}->kvetch(Dumper($gene_products));
    # $self->{CORE}->kvetch(Dumper(['foo']));
    foreach my $gp (keys %$gene_products){
      my $link = $self->{CORE}->get_interlink({mode=>'gp-details', arg => {gp => $gp}});
      $gene_products->{$gp}{link} = $link;
      # $self->{CORE}->kvetch($link);
    }

    ## Add additional information to RG info.
    foreach my $rgs (keys %$reference_genome_sets){

      my $detail_link = $self->{CORE}->get_interlink({mode=>'homolset_annotation',arg =>
					      {set => $rgs}});
      $reference_genome_sets->{$rgs}{detail_link} = $detail_link;

      my $svg_link = $self->{CORE}->get_interlink({mode=>'homolset_graph', arg =>
					   {set => $rgs, format => 'svg'}});
      $reference_genome_sets->{$rgs}{svg_link} = $svg_link;

      my $png_link = $self->{CORE}->get_interlink({mode=>'homolset_graph', arg =>
					   {set => $rgs, format => 'png'}});
      $reference_genome_sets->{$rgs}{png_link} = $png_link;
    }

    $self->set_template_parameter('SUMMARY', $summary);
    $self->set_template_parameter('SPECIES', $species);
    $self->set_template_parameter('RG_SETS', $reference_genome_sets);
    $self->set_template_parameter('GPS', $gene_products);
    $self->set_template_parameter('RG_ORDERED_SPECIES_LIST',
				  #$rg_aid->species_list({num_p=>0,safely=>1}));
				  $rg_aid->species_list({num_p=>1}));

    ## Breaking RG information down alphabetically....
    my @alphabet = qw(a b c d e f g h i j k l m n o p q r s t u z w x y z);
    $self->set_template_parameter('ALPHABET', \@alphabet);
    my $alphabetical_data = {};
    foreach my $rg_id ( keys %{$reference_genome_sets} ) {
      my $sym = $reference_genome_sets->{$rg_id}{symbol};
      my $letter = lc(substr($sym, 0, 1));
      if ( ! defined $alphabetical_data->{$letter} ) {
	$alphabetical_data->{$letter} = [];
      }
      push @{$alphabetical_data->{$letter}}, $rg_id;
    }
    ## Now, within each letter group, let's get the ordering correct.
    foreach my $letter (keys %$alphabetical_data) {
      my @new_inner_array = sort {
	my $ai = $reference_genome_sets->{$a}{symbol};
	my $bi = $reference_genome_sets->{$b}{symbol};
	#      $self->{CORE}->kvetch("     $a vs $b and $ai vs $bi");
	return $ai cmp $bi;
      } @{$alphabetical_data->{$letter}};
      $alphabetical_data->{$letter} = \@new_inner_array;
    }
    $self->set_template_parameter('ALPHABETICAL_DATA', $alphabetical_data);

    ## Bring in the coloration that we want.
    $self->set_template_parameter('STATUS_COLORS', $rg_aid->get_status_colors());

    ##
    $self->add_template_content('html/main/homolset_summary_exp.tmpl');

    return $self->generate_template_page();
  }
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

  my $term_worker = AmiGO::Worker::Solr::Term->new();
  my $term_info_hash = $term_worker->get_info($input_term_id);
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

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please
  # $self->set_template_parameter('STANDARD_YUI', 1);

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      # 'standard', # basic GO-styles
      # 'org.bbop.amigo.ui.autocomplete'
      'standard', # basic GO-styles
      'com.jquery.jqamigo.custom',
      #'com.jquery.tablesorter',
      'org.bbop.amigo.ui.widgets'
     ],
     javascript_library =>
     [
      'com.jquery',
      'com.jquery-ui',
      'com.jquery.tablesorter',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.ui.widgets'
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
      $self->add_template_content('html/main/term_details.tmpl');
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

  my $gp_worker = AmiGO::Worker::Solr::GeneProduct->new();
  my $gp_info_hash = $gp_worker->get_info($input_gp_id);
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

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      # 'standard', # basic GO-styles
      # 'org.bbop.amigo.ui.autocomplete'
      'standard', # basic GO-styles
      'com.jquery.jqamigo.custom',
      #'com.jquery.tablesorter',
      'org.bbop.amigo.ui.widgets'
     ],
     javascript_library =>
     [
      'com.jquery',
      'com.jquery-ui',
      'com.jquery.tablesorter',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.ui.widgets'
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

  $self->add_template_content('html/main/gene_product_details.tmpl');

  return $self->generate_template_page();
}


##
sub mode_layers_graph {

  my $self = shift;

  ## Deal with input.
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('gander');
  my $lon = $params->{lon};
  my $lat = $params->{lat};
  my $focus = $params->{focus};
  my $zoom = $params->{zoom};
  my $input_term_list = $self->{CORE}->clean_term_list($params->{terms});

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please
  $self->set_template_parameter('page_title', 'AmiGO: Navi');
  $self->set_template_parameter('page_content_layout', 'wide');
  $self->set_template_parameter('IE8_COMPAT_MODE', 'yes');

  ## Basic ordering.
  my $prep =
    {
     css_library =>
     [
      'standard',
      'org.bbop.amigo.ui.widgets',
      'GONavi'
     ],
     javascript_library =>
     [
      #'org.openlayers'
      'com.jquery',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.ui.widgets'
     ]
    };
  $self->add_template_bulk($prep);

  ## Prepare for more links and libs.
  my $open_layers =
    $self->{JS}->acquire_source('http://openlayers.org/api/OpenLayers.js');
  my $navi_data_args =
    {mode => 'navi_js_data',
     optional => {full => 1},
     arg => {
	     lon => $lon,
	     lat => $lat,
	     focus => $focus,
	     zoom => $zoom,
	     terms => $input_term_list,
	    }};
  my $navi_data =
    $self->{JS}->acquire_source($self->{CORE}->get_interlink($navi_data_args));
  my $navi_main = $self->{JS}->get_lib('GONavi');
  my $navi_plugins = $self->{JS}->get_lib('GONaviPluginExamples.js');
  my $start = $self->{JS}->initializer_jquery("(function(){ jQuery.noConflict(); GONavi.Init([RandomTermHiliteLayer, RandomPathHiliteLayer, BookInit, PlugInit]); })();");

  ##
  $self->add_template_javascript($open_layers);
  $self->add_template_javascript($navi_main);
  $self->add_template_javascript($navi_data);
  $self->add_template_javascript($navi_plugins);
  $self->add_template_javascript($start);
  $self->add_template_content('html/main/layers_graph.tmpl');

  return $self->generate_template_page();
}


##
sub mode_autocomplete_client {

  my $self = shift;

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please

  ## Our AmiGO services CSS.
  my @css_libs =
    (
     'standard', # basic GO-styles
     'org.bbop.amigo.ui.autocomplete'
    );
  ## Our AmiGO services JSS.
  my @js_libs =
    (
     'com.jquery',
     'org.bbop.amigo',
     'org.bbop.amigo.go_meta',
     'org.bbop.amigo.opensearch',
     'org.bbop.amigo.ui.autocomplete'
    );
  my @content_lines =
    (
     '<div>',
     '<form>',
     '<p><input id="ac1" class="textBox" type="text" value="" width=30 /> (term, acc)</p>',
     '<p><input id="ac2" class="textBox" type="text" value="" width=30 /> (term, completion)</p>',
     '<p><input id="ac3" class="textBox" type="text" value="" width=30 /> (gene product, acc)</p>',
     '<p><input id="ac4" class="textBox" type="text" value="" width=30 /> (general, acc)</p>',
     '</form>',
     '</div>'
    );

  ## Add 'em.
  foreach my $css_lib (@css_libs){
    $self->add_template_css( $self->{CSS}->get_css($css_lib) );
  }
  foreach my $js_lib (@js_libs){
    $self->add_template_javascript( $self->{JS}->get_lib($js_lib) );
  }
  foreach my $c (@content_lines){
    $self->add_template_content( $c );
  }

  ##
  $self->add_template_javascript($self->{JS}->initializer_jquery('new org.bbop.amigo.ui.autocomplete({id:"ac1", search_type:"term", completion_type:"acc"});new org.bbop.amigo.ui.autocomplete({id:"ac2", search_type:"term", completion_type:"completion"});new org.bbop.amigo.ui.autocomplete({id:"ac3", search_type:"gene_product", completion_type:"acc"});new org.bbop.amigo.ui.autocomplete({id:"ac4", search_type:"general", completion_type:"acc"});'));

  return $self->generate_template_page();
}


##
sub mode_gp_with_2_terms {

  #$ENV{DBIC_TRACE} = 1;
  #$Data::Dumper::Sortkeys = 1;

  my $self = shift;
  my $output;
  my $template = 'includes/coannotation.tmpl';

  ##
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('matrix');
  my $input_term_list = $self->{CORE}->clean_term_list($params->{terms});

  $self->_common_params_settings({title=>'AmiGO: Co-annotation'});

  ## Error handling.
  my $error_msg = {};
  my $result_set = {};

  ## Switch on whether the input looks right.
  if( scalar(@$input_term_list) != 2 ) {
    $error_msg->{MSG_CODE} = 'n_ids_wrong';
    $self->set_template_parameter('error_msg', $error_msg);
    $self->add_template_content($template);
    return $self->generate_template_page();
  } else {

    ## Get results.
    my $query = GOBO::DBIC::GODBModel::Query->new({type=>'gene_product_two_terms'});

    ## Let's get us some terms.
    my $terms = $query->{SCHEMA}->resultset('Term')->search([ map { { acc => $_ } } @$input_term_list ]);
    my @term_set = $terms->all;

    ## check we've got the correct number of terms
    if (scalar @term_set != 2){
      if (! @term_set ){
	$error_msg->{MSG} =
	  'Neither of the IDs you entered could be found. Please try again.';
      }elsif (scalar @term_set == 1){
	# would be better to make this more extensible...
	my @missing = grep {
	  $_ ne $term_set[0]->acc
	} @$input_term_list;

	$result_set->{input_term_list} =
	  [ { name => $term_set[0]->name,
	      acc => $term_set[0]->acc,
	      namespace => $term_set[0]->term_type } ];
	$error_msg->{MSG} =
	  'The GO ID '.$missing[0].' could not be found. Please try again.';
	$self->set_template_parameter('show_input_form', 1);
      }
      $self->set_template_parameter('result_set', $result_set);
      $self->set_template_parameter('error_msg', $error_msg);
      $self->add_template_content($template);
      return $self->generate_template_page();
    }else{
      foreach (@term_set){
	$output->{term_data}{ $_->acc } = { name => $_->name, acc => $_->acc, namespace => $_->term_type, is_obsolete => $_->is_obsolete, id => $_->id };
      }
      foreach (@$input_term_list){
	push @{$output->{input_term_list}},
	  $output->{term_data}{ $_ };
      }
    }

    $query->{QUERY_JOIN} = 
      [{'association' =>
	[	'graph_path_relations',
		'term',
	],
       },
       {'association' =>
	[	'graph_path_relations',
		'term',
	],
       }];

    my $prefetch =
      [{'association' => 'term',
	#      	{ 'term' => [ 'name', 'acc' ] }, 
       },
       {'association' => 'term',
	#      	{ 'term' => [ 'name', 'acc' ] }, 
       }];


    $query->{QUERY_JOIN} = 
      [
       {	'association' =>
		[	'graph_path_relations',
			'term', 
		],
       },
       {	'association' =>
		[	'graph_path_relations',
			'term', 
		],
       },
      ];
    $query->{QUERY_RESULT_SET} = 'GeneProduct';

    my $all_results;

    my $stupid_sql = '!= association_2.term_id';
    my $results =
      $query->{SCHEMA}->resultset($query->{QUERY_RESULT_SET})->search(
	      {-and=>[
		      {'graph_path_relations.term1_id' =>
		       $output->{input_term_list}[0]{id}},
		      {'graph_path_relations_2.term1_id' =>
		       $output->{input_term_list}[1]{id}},
		     ]},
	      {	distinct => 'me',
		join => $query->{QUERY_JOIN},
		#	prefetch => $prefetch,
		'+select' =>['term.acc','term.name','term_2.acc','term_2.name'],
		'+as' => ['term_acc','term_name','term_2_acc','term_2_name' ],
	      });

    ## TODO: can toss some error checking in here for return values.
    @$all_results = $results->all;

    # return if there are no results
    if (!$all_results || !@$all_results){
      $error_msg->{MSG_CODE} = 'no_results';
      $self->set_template_parameter('show_input_form', 1);
      $self->set_template_parameter('result_set', $output);
      $self->set_template_parameter('error_msg', $error_msg);
      $self->add_template_content($template);
      return $self->generate_template_page();
    }

    $output->{n_total_results} = scalar @$all_results;

    foreach my $gp (@$all_results) {
      my $data;

      $data->{gene_product_id} = $gp->id;

      ### general GP data
      if (!$output->{gp_data}{$gp->id}){
	$output->{gp_data}{$gp->id}{gpxref} =
	  $gp->dbxref->xref_dbname . ":" . $gp->dbxref->xref_key;
	$output->{gp_data}{$gp->id}{symbol} = $gp->symbol;
	$output->{gp_data}{$gp->id}{full_name} = $gp->full_name || $gp->symbol;
	$output->{gp_data}{$gp->id}{species} =
	  [ $gp->species->genus, $gp->species->species ];
	$output->{gp_data}{$gp->id}{ncbi_taxa_id} = $gp->species->ncbi_taxa_id;
	$output->{gp_data}{$gp->id}{type} = $gp->type->name;

	### links
	$output->{gp_data}{$gp->id}{gp_details_link} =
	  $self->{CORE}->get_interlink({
				mode=>'gp-details',
				arg=>{ gp =>
				       $gp->dbxref->xref_dbname .
				       ":" .
				       $gp->dbxref->xref_key }});
      }

      ### terms attached
      foreach my $t ( 'term', 'term_2' ){
	push @{$data->{terms}}, $gp->get_column( $t.'_acc');

	# see if we have the term data yet
	if (! $output->{term_data}{ $gp->get_column( $t.'_acc' ) }){
	  $output->{term_data}{ $gp->get_column( $t.'_acc' ) } =
	    {
	     acc => $gp->get_column($t.'_acc'),
	     name => $gp->get_column($t.'_name'),
	     term_details_link =>
	     $self->{CORE}->get_interlink({mode => 'term-details',
				   arg=> {acc => $gp->get_column($t.'_acc')}}),
	    };
	}
      }

      $output->{t2t_matrix}{$data->{terms}[0]}{$data->{terms}[1]}{$gp->id}++;
      $output->{inv_t2t_matrix}{$data->{terms}[1]}{$data->{terms}[0]}{$gp->id}++;

      ### add a string for sorting
      ### current sort params:
      # gp->{symbol},
      # gp->{full_name},
      # is term 1->acc in the query list?
      # term 1->acc
      # is term 2->acc in the query list?
      # term 2->acc
      my @array = (
		   lc($gp->symbol),
		   ( lc($gp->full_name) || lc($gp->symbol) ) );
      if ( grep { $data->{terms}[0] eq $_ } @$input_term_list){
	push @array, 'A', $data->{terms}[0];
      }else{
	push @array, 'B', $data->{terms}[0];
      }

      if ( grep { $data->{terms}[1] eq $_ } @$input_term_list){
	push @array, 'A', $data->{terms}[1];
      }else{
	push @array, 'B', $data->{terms}[1];
      }

      $data->{sort_string} = join("\0", @array);

      push @{$output->{coassoc_list}}, $data;
    }

    ### Let's sort our output
    my @sorted = map {
      $_ = $_->[1];
    } sort { $a->[0] cmp $b->[0] }
      map {
	[ $_->{sort_string}, $_ ]
      } @{$output->{coassoc_list}};

    $output->{sorted_coassoc_list} = [ @sorted ];

    # query for number of distinct terms assoc'd to
    my $gpc = $query->{SCHEMA}->resultset('Association')->search(
	 [
	  map {
	    { 'gene_product_id' => $_ }
	  } keys %{$output->{gp_data}}
	 ],
	 {
	  'select' => [
		       { count => { distinct => 'term_id' } },
		       'gene_product_id',
		      ],
	  group_by => [ 'gene_product_id', ],
	  'as' => [ 'count',  'gene_product_id', ],
	 });

    foreach ( $gpc->all ){
      $output->{gp_data}{ $_->gene_product_id }{n_term_assocs} =
	$_->get_column('count');
    }

    $self->set_template_parameter('result_set', $output);

    ## Emit output
    # $self->set_template_parameter('error_msg', $error_msg); # need this?
    $self->add_template_content($template);
    return $self->generate_template_page();
  }
}


## Reworking of the above, but with a new "backend" and new
## frontend". Uh...if you take a piece of wood from a boat to build a
## new boat...
sub mode_nmatrix {

  my $self = shift;

  ## Incoming template.
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('nmatrix');
  $self->_common_params_settings($params);

  ###
  ### Calculations.
  ###

  # my $insect = AmiGO::Worker::Intersection->new(10090, 2);
  # my $res = $insect->get_information(['GO:0043073', 'GO:0045944']);
  # $self->{CORE}->kvetch("_a_" . Dumper($res));

  # die "Yay!";

  ## Get the information matrix if the arguments are there.
  my $matrix = undef;
  my $term_info = undef;
  my $gp_info = undef;
  my $id_to_gps_map = {}; # also, map the unique point ids to gps.
  if($params->{species} &&
     $params->{term_set_1} && # need at least two for axes (set 3+ optional)
     $params->{term_set_2} &&
     $params->{graph_type} ){

    #$self->{CORE}->kvetch("_params_terms_" . $params->{terms});

    ## Collect defined incoming sets into an arrayref of
    ## arayrefs. Clean and order them while we're at it (makes
    ## bookkeeping much easier later).
    my $term_sets = [];
    for( my $i = 1; $i <= 4; $i++ ){
      my $set = 'term_set_' . $i;
      if( defined $params->{$set} && $params->{$set} ){
	my $go_id_list = $self->{CORE}->clean_term_list($params->{$set});
	if( scalar(@$go_id_list) > 0 ){
	  my @ordered_ts = sort { $a cmp $b } @$go_id_list;
	  push @$term_sets, \@ordered_ts;
	}
      }
    }
    my $set_count = scalar(@$term_sets);
    die "wasn\'t able to parse out enough sets" if $set_count < 2;

    $self->{CORE}->kvetch("_term_sets_:" . Dumper(@$term_sets));

    ## Easy by comparison.
    my $spc = $params->{species};

    #$self->{CORE}->kvetch("_go_id_list_(scalar)" . scalar(@$go_id_list));

    #$output = $matrix->make_matrix($spc, $go_id_list);

    my $m_by_n = AmiGO::Worker::NMatrix->new($spc,
					     $set_count,
					     $params->{graph_type});
    $matrix = $m_by_n->get_matrix($term_sets);
    #$self->{CORE}->kvetch('($spc)' . $spc);
    #$self->{CORE}->kvetch('($set_count)' . $set_count);
    #$self->{CORE}->kvetch('($params->{graph_type})' . $params->{graph_type});
    #$self->{CORE}->kvetch('($m_by_n)' . Dumper($m_by_n));

    #die "Yay!";

    ###
    ### Information chewing, gathering, and simplification.
    ###

    ## The core bit of information.
    $self->set_template_parameter('matrix', $matrix);

    ## Get axis lists (as much as possible).
    $self->set_template_parameter('axis_1_list', $$term_sets[0] || []);
    $self->set_template_parameter('axis_2_list', $$term_sets[1] || []);
    $self->set_template_parameter('axis_3_list', $$term_sets[2] || []);

    ## Get all term information for display and cache it to produce
    ## the JS variable.
    my $term_q = AmiGO::Worker::Term->new();
    my @seen_terms = keys %{$m_by_n->get_terms()};
    $term_info = $term_q->get_info(\@seen_terms);
    $self->set_template_parameter('term_info', $term_info);

    ## Get all gp information and cache it to produce the JS variable.
    my $gp_q = AmiGO::Worker::GeneProduct->new();
    my @seen_gps = keys %{$m_by_n->get_gene_products()};
    #$self->{CORE}->kvetch('(\@seen_gps)' . Dumper(\@seen_gps));
    $gp_info = $gp_q->get_info(\@seen_gps);
    #$self->{CORE}->kvetch('h3');
  }

  ###
  ### Common page prep.
  ###

  ## Handle graph type selection variables.
  my $graph_type_list =
    {
     'all' => 'include all relations',
     'no_regulates' => 'exclude regulates (waiting for database change...)',
    };
  $self->set_template_parameter('graph_type_list', $graph_type_list);
  $self->set_template_parameter('graph_type_selected', $params->{graph_type});

  ## For memory and chained incoming.
  $self->set_template_parameter('term_set_1_text', $params->{term_set_1} || '');
  $self->set_template_parameter('term_set_2_text', $params->{term_set_2} || '');
  $self->set_template_parameter('term_set_3_text', $params->{term_set_3} || '');
  $self->set_template_parameter('graph_type', $params->{graph_type});

#   ## Cart image.
#   $self->set_template_parameter('cart_image',
# 				$self->{CORE}->amigo_env('IMAGE_URL') . '/cart.png');

  ## Tie species and selected species (if any) into the templates.
  $self->set_template_parameter('species_hash',  $self->{CORE}->species());
  $self->set_template_parameter('species_selected',
				$params->{'species'} || undef);

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please
  $self->set_template_parameter('page_title', 'N-Matrix');

  ## Our AmiGO JS and CSS.
  my $prep =
    {
     css_library =>
     [
      'standard', # basic GO-styles
      'org.bbop.amigo.ui.standard',
      'org.bbop.amigo.ui.widgets',
      'com.jquery.redmond.custom'
     ],
     ## Our AmiGO services JSS.
     javascript_library =>
     [
      'com.jquery',
      'com.jquery-ui',
      'com.jquery-layout',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.item',
      'org.bbop.amigo.workspace',
      'org.bbop.amigo.ui.standard',
      'org.bbop.amigo.ui.widgets',
      'org.bbop.amigo.ui.cart',
      'org.bbop.amigo.ui.shield'
     ],
     javascript =>
     [
      $self->{JS}->get_lib('NMatrix.js'),
      $self->{JS}->make_var('global_cart_map', $id_to_gps_map),
      ## Also, we want to turn the base matrix hash into a JS resource
      ## for creating the shield later on.
      $self->{JS}->make_var('global_matrix', $matrix),
      $self->{JS}->make_var('global_term_info', $term_info),
      $self->{JS}->make_var('global_gene_product_info', $gp_info),
      ## Cart image.
      $self->{JS}->make_var('global_cart_image',
			    $self->{CORE}->amigo_env('IMAGE_URL') . '/cart.png')
     ],
     javascript_init =>
     [
      'NMatrixInit();'
     ]
    };
  $self->add_template_bulk($prep);

  $self->add_template_content('html/main/nmatrix.tmpl');

  return $self->generate_template_page();
}


## Generic interface for grabbing Newick tree external resources and
## displaying them.
sub mode_ntree {

  my $self = shift;

  ## Incoming template.
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('external_resource');
  $self->_common_params_settings($params);

  ## Was there input? Could we successfully get data from somewhere?
  my $rsc = $params->{external_resource} || '';
  my $raw_data = undef;
  my $external_status = 'todo';
  if( $rsc ){
    my $external = AmiGO::External::Raw->new();
    $raw_data = $external->get_external_data($rsc);
    if( defined $raw_data && $raw_data && length($raw_data) > 0 ){
      $external_status = 'success';
    }else{
      $external_status = 'failure';
    }
  }
  $self->set_template_parameter('external_resource', $rsc);
  $self->set_template_parameter('external_status', $external_status);
  $self->set_template_parameter('raw_data', $raw_data);

  ###
  ### Page settings.
  ###

  ## Grab all the *.tree files from somewhere.
  my $pdir = $self->{CORE}->amigo_env('AMIGO_HTDOCS_ROOT_DIR') . '/panther';
  my $ppath = $self->{CORE}->amigo_env('AMIGO_HTML_URL') . '/panther';
  my @full_tree_files = glob("$pdir/*.tree");
  my @tree_files = map { [fileparse($_)]->[0] } @full_tree_files;
  $self->set_template_parameter('tree_path', $ppath);
  $self->set_template_parameter('tree_files', \@tree_files);

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      'standard', # basic GO-styles
      'org.bbop.amigo.ui.widgets'
     ],
     javascript_library =>
     [
      'newick.json',
      'newick.tree',
      'newick.tree_utils',
      'newick.phylo',
      'com.jquery',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.ui.widgets'
     ],
     javascript =>
     [
      $self->{JS}->make_var('global_raw_data', $raw_data)
     ]
    };
  $self->add_template_bulk($prep);

  ## Our client JS.
  $self->add_template_javascript($self->{JS}->get_lib('PhyloTreeClient.js'));

  ## Initialize javascript app, but only when we have appropriate
  ## downloaded data.
  if( $external_status eq 'success' ){
    #$self->{CORE}->kvetch('bar:' . $external_status);
    $self->add_template_javascript($self->{JS}->initializer_jquery('PhyloTreeBuilder();'));
  }

  $self->{CORE}->kvetch('resource: ' . $rsc);
  $self->{CORE}->kvetch('status: ' . $external_status);
  $self->{CORE}->kvetch('raw: ' . $raw_data);

  ##
  $self->add_template_content('html/main/phylo_ntree.tmpl');
  return $self->generate_template_page();
}


## TODO/BUG: some highly unsafe/unchecked calls in here--need better
## sanitation.
sub mode_ptree {

  my $self = shift;

  ## Incoming template.
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('id');
  $self->_common_params_settings($params);

  ## Was there input? Could we successfully get data from somewhere?
  my $id = $params->{id} || '';
  my $raw_data = undef;
  #my $raw_data_name = undef;
  if( defined $id && $id ){
    my $pprop = AmiGO::Worker::PANTHERTree->new();
    $raw_data = $pprop->get_tree($id);
    ## Hopefully just the one for now.
    if( ! defined $raw_data || ! $raw_data || scalar(@$raw_data) != 1 ){
      die "this does not appear to be an appropriate panther id";
    }else{
      ## TODO/BUG: too tired to deal will multiple data, etc. right
      ## now, just peel off the last tree and use the ntree
      ## template...
      $raw_data = $raw_data->[0]{tree};
      #$raw_data_name = $raw_data->[0]{name};
    }
  }

  ## TODO: Temporarily reuse the ntree template.
  $self->set_template_parameter('raw_data', $raw_data);
  $self->set_template_parameter('tree_name', $id);
  $self->set_template_parameter('external_status', 'success');
  $self->set_template_parameter('no_controls', 1);

  ###
  ### Page settings.
  ###

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      'standard', # basic GO-styles
      'org.bbop.amigo.ui.widgets'
     ],
     javascript_library =>
     [
      'newick.json',
      'newick.tree',
      'newick.tree_utils',
      'newick.phylo',
      'com.jquery',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.ui.widgets'
     ],
     javascript =>
     [
      $self->{JS}->make_var('global_raw_data', $raw_data)
     ]
    };
  $self->add_template_bulk($prep);

  ## Our client JS.
  $self->add_template_javascript($self->{JS}->get_lib('PhyloTreeClient.js'));
  $self->add_template_javascript($self->{JS}->initializer_jquery('PhyloTreeBuilder();'));

  $self->{CORE}->kvetch('raw: ' . $raw_data);

  ##
  $self->add_template_content('html/main/phylo_ntree.tmpl');
  return $self->generate_template_page();
}


##
sub mode_scratch {

  my $self = shift;

  ## Incoming template.
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile();
  $self->_common_params_settings($params);

  ###
  ### Page settings.
  ###

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      'standard', # basic GO-styles
      'org.bbop.amigo.ui.standard',
      'org.bbop.amigo.ui.widgets',
      'com.jquery.redmond.custom'
     ],
     javascript_library =>
     [
      'com.jquery',
      'com.jquery-ui',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.workspace',
      'org.bbop.amigo.ui.workspace',
      'org.bbop.amigo.ui.widgets',
      'org.bbop.amigo.ui.cart',
      'org.bbop.amigo.ui.shield',
      'org.bbop.amigo.ui.wait',
      'org.bbop.amigo.ui.shopping'
     ],
     javascript => []
    };
  $self->add_template_bulk($prep);

  ## Make sure that the cart image is in the mix.
  ## TODO: make this less stilted.
  my $cart_png = $self->{CORE}->amigo_env('IMAGE_URL') . '/cart.png';
  $self->add_template_javascript($self->{JS}->make_var('global_cart_image', $cart_png));
  $self->set_template_parameter('cart_image', $cart_png);

  $self->set_template_parameter('filler_n', 500);

  ## Our client JS.
  $self->add_template_javascript($self->{JS}->get_lib('ScratchClient.js'));
  ## Initialize javascript app.
  $self->add_template_javascript($self->{JS}->initializer_jquery('ScratchClientInit();'));

  ##
  $self->add_template_content('html/main/scratch.tmpl');
  return $self->generate_template_page();
}


##
sub mode_exhibit_exp {

  my $self = shift;

  ###
  ### Page settings.
  ###

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please
  $self->set_template_parameter('STANDARD_CSS', 'no');

  ## Our AmiGO services CSS.
  my $prep =
    {
     css_library =>
     [
      'standard', # basic GO-styles
      #'org.bbop.amigo.ui.standard',
      #'org.bbop.amigo.ui.widgets',
      'com.jquery.redmond.custom'
     ],
     javascript_library =>
     [
      'com.jquery',
      'com.jquery-ui',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'as.core.core',
      'as.core.abstractmanager',
      'as.managers.jquery',
      'as.core.parameter',
      'as.core.parameterstore',
      'as.core.abstractwidget',
      'as.helpers.jquery.ajaxsolr.theme',
      'as.widgets.jquery.pagerwidget',
      'as.core.abstractfacetwidget'
      #'org.bbop.amigo.workspace',
      #'org.bbop.amigo.ui.workspace',
      #'org.bbop.amigo.ui.widgets',
      #'org.bbop.amigo.ui.cart',
      #'org.bbop.amigo.ui.shield',
      #'org.bbop.amigo.ui.wait',
      #'org.bbop.amigo.ui.shopping'
     ],
     javascript =>
     [
      $self->{JS}->get_lib('LiveSearchAS.js')
     ],
     javascript_init =>
     [
      'LiveSearchASInit();'
     ],
     content =>
     [
      'html/main/exhibit_exp.tmpl'
     ]
    };
  $self->add_template_bulk($prep);

  return $self->generate_template_page();
}


## Go to main.
sub mode_kick_to_main {

  my $self = shift;

  $self->set_template_parameter('page_title', 'AmiGO: No Page Here');
  $self->add_template_content('html/main/forward_to_main.tmpl');

  return $self->generate_template_page();
}


###
### Bit-rotted code that may still be useful...
###


sub mode_report_slimmerish_1 {

  my $self = shift;

  ## Incoming template and input.
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('slimmerish'); # remove slimmerish from input
  $self->_common_params_settings($params);
  my $in_term_str = $params->{term} || '';
  my $in_gp_str = $params->{gene_product} || '';
  my $load = $params->{load} || '';

  ## Clean input.
  $self->{CORE}->kvetch("in term str: " . $in_term_str);
  $self->{CORE}->kvetch("in gp str: " . $in_gp_str);

  if( $in_term_str ){

    ## TODO/BUG: Probably needs to be tighter if we go to general
    ## production on this.
    my $props =
      {
       #'login' => $self>amigo_env('') || '',
       #'password' => $mirror->{password} || '',
       'host' => $self->{CORE}->amigo_env('GO_DBHOST') || undef,
       #'port' => $mirror->{port} || '3306',
       'database' => $self->{CORE}->amigo_env('GO_DBNAME') || undef,
      };
    my $q = AmiGO::External::LEAD::Query->new($props, 10000000, 10000000);

    ## NOTE/BUG: If it turns out that there are problems with DBI and
    ## getting submitting
    my $single_get = sub {

      ## 
      my $sym = shift || 'n/a';
      my $get_sql = shift;

      ## Output var.
      my $get_ids = [];

      ##
      $self->{CORE}->kvetch("$sym try: " . $get_sql);
      my $results = $q->try($get_sql);
      if( ! $q->ok() ){
	$self->{CORE}->kvetch("$sym badness: " . $q->error_message());
      }elsif( ! defined $results ){
	$self->{CORE}->kvetch("no defined $sym results");
      }else{
	## Unfold terms ids into arrayref.
	if( scalar(@$results) > 0 ){
	  $get_ids = [];
	  foreach my $row (@$results){
	    push @$get_ids, $row->[0];
	  }
	}else{
	  $self->{CORE}->kvetch("$sym no results");
	}
	$self->{CORE}->kvetch("$sym okay: " . Dumper($get_ids));
      }

      return $get_ids;
    };

    ## Get root term ids.
    my $root_sql = "select term.id from term where acc in ('GO:0008150', 'GO:0005575', 'GO:0003674')";
    my $root_ids = &$single_get('root', $root_sql);

    ## Get term ids for all terms and their children (complete
    ## coverage of slim).
    my $in_term_accs = $self->{CORE}->clean_term_list($in_term_str);
    my $in_term_string = "('" . join("', '", @$in_term_accs) . "')";
    my $term_sql = "select distinct aterm.id from term as sterm inner join graph_path on (sterm.id = graph_path.term1_id) inner join term as aterm on (graph_path.term2_id = aterm.id) where sterm.acc not in ('GO:0008150', 'GO:0005575', 'GO:0003674') and sterm.acc in $in_term_string";
    my $term_ids = &$single_get('term', $term_sql);

    ## Convert accs into a gp id list. Sort out the found and the
    ## unfounds.
    my $gp_ids = [];
    my $unknown_gp_ids = [];

    ## Choose between one of the pre-selected ones and the user input.
    if( defined $load && $load ){

      $self->{CORE}->kvetch("Use DB input.");

      ## Get all of the RG species if 'all' comes in, otherwise just
      ## go with everything that is in 'load' as an ncbi taxa id.
      my $gp_sql = undef;
      if( $load eq 'all' ){
	my $rglist = $rg_aid->species_list({num_p=>1});
	my $sqlrglist = join(', ', @$rglist);
      	$gp_sql = "select gp.id from gene_product as gp, species, dbxref where gp.species_id = species.id and gp.dbxref_id = dbxref.id and species.ncbi_taxa_id in ($sqlrglist)";
      }else{
	$gp_sql = "select gp.id from gene_product as gp, species, dbxref where gp.species_id = species.id and gp.dbxref_id = dbxref.id and species.ncbi_taxa_id in ($load)";
      }

      ## Get them and pack them in.
      my $gids = &$single_get('gp', $gp_sql);
      foreach my $foo (@$gids){
	push @$gp_ids, $foo;
      }

    }elsif( $in_gp_str ){

      $self->{CORE}->kvetch("Use user input.");

      my $in_gp_accs = $self->{CORE}->clean_list($in_gp_str);
      foreach my $gp_acc (@$in_gp_accs){
	my ($db, $key) = $self->{CORE}->split_gene_product_acc($gp_acc);
	my $gp_sql = "select distinct gp.id from dbxref inner join gene_product as gp on (dbxref.id = gp.dbxref_id) where dbxref.xref_dbname = '$db' and dbxref.xref_key = '$key'";
	my $gp_id = &$single_get('gp', $gp_sql);
	if(  defined $gp_id && $gp_id ){
	  my $foo = $$gp_id[0];
	  if( defined $foo && $foo ){
	    push @$gp_ids, $foo;
	  }else{
	    push @$unknown_gp_ids, $gp_acc;
	  }
	}
      }
    }else{
      ## TODO: Need to error on no gp str and no load.
      #$self->mode_fatal_with_message("Need to somehow specify GPs.");
      $self->{CORE}->kvetch("Need to somehow specify GPs.");
    }

    ###
    ### Combine everything into one query: see what's in and use that to
    ### mark what's out.
    ###

    ## Combine roots and terms and turn them into an appropriate string.
    my @all_ids = (@$term_ids, @$root_ids);
    my $term_string = "(" . join(', ', @all_ids) . ")";

    ## Turn gps them into an appropriate string.
    my $gp_string = "(" . join(', ', @$gp_ids) . ")";

    $self->{CORE}->kvetch("scalar term ids: " . scalar(@all_ids));
    $self->{CORE}->kvetch("scalar gp ids: " . scalar(@$gp_ids));

    ##
    #my $good_sql = "select distinct gene_product.id from gene_product inner join association on (association.gene_product_id = gene_product.id) left outer join term on (association.term_id = term.id) where gene_product.id in $gp_string and term.id in $term_string";
    my $good_sql = "select distinct gene_product.id from gene_product inner join association on (association.gene_product_id = gene_product.id) inner join term on (association.term_id = term.id) where gene_product.id in $gp_string and term.id in $term_string";
    #$self->{CORE}->kvetch("good sql: " . $good_sql);
    my $good_ids = &$single_get('good', $good_sql);
    $self->{CORE}->kvetch("scalar good ids: " . scalar(@$good_ids));

    ## Compare the good ids to the total lit--the remainder should be
    ## the ones that fell through.
    my @bad_ids = ();
    {
      my $info_hash = {};
      ## Get *all* ids in.
      foreach my $id (@$gp_ids){ $info_hash->{$id} = 1; }
      ## Remove the ones that were found in the good list.
      foreach my $id (@$good_ids){
	delete $info_hash->{$id} if defined $info_hash->{$id};
      }
      ## This leaves us with the gp ids not found associated to a
      ## term--the fall-throughs.
      @bad_ids = keys %$info_hash;
    }
    $self->{CORE}->kvetch("scalar bad ids: " . scalar(@bad_ids));

    ## Get the final (displayable) data from the gp_ids not on the good
    ## list.
    if( scalar(@bad_ids) == 0 ){
      $self->{CORE}->kvetch("apparently no bad ids");
      $self->set_template_parameter('no_bad_ids', 1);
    }else{
      my $info_string = "(" . join(', ', @bad_ids) . ")";
      my $info_sql = "select * from gene_product, dbxref where gene_product.dbxref_id = dbxref.id and gene_product.id in $info_string";
      my $results = $q->try($info_sql);
      if( ! $q->ok() ){
	$self->{CORE}->kvetch("final badness: " . $q->error_message());
      }elsif( ! defined $results ){
	$self->{CORE}->kvetch("no defined final results");
      }else{

	my $headers = $q->headers();
	my $count = $q->count();

	#$self->{CORE}->kvetch("final okay (headers): " . Dumper($count));
	#$self->{CORE}->kvetch("final okay (headers): " . Dumper($headers));
	#$self->{CORE}->kvetch("final okay (results): " . Dumper($results));

	$self->set_template_parameter('results_count', $count);
	$self->set_template_parameter('results_headers', $headers);
	$self->set_template_parameter('results', $results);
	$self->set_template_parameter('unknowns', $unknown_gp_ids);
	$self->set_template_parameter('unknowns_count',
				      scalar(@$unknown_gp_ids));
      }
    }
  }else{
    ## TODO: Need to error on no term str.
    #$self->mode_fatal_with_message("Need to somehow specify terms.");
    $self->{CORE}->kvetch("Need to somehow specify terms.");
  }

  ## Get RG info for form.
  my $rg_info_for_form =
    $rg_aid->species_information($rg_aid->species_list({num_p=>1}));
  $self->set_template_parameter('RG_INFO', $rg_info_for_form);

  ## Settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please
  $self->set_template_parameter('page_title', 'Slimmer-type Report');
  my $prep =
    {
     css_library =>
     [
      'standard',
     ],
    };
  $self->add_template_bulk($prep);
  $self->add_template_content('html/main/report/slimmerish_1.tmpl');
  return $self->generate_template_page();
}


## A toy to figure out exactly what AmiGO thinks about an identifier
## in the database.
sub mode_knomigo {

  my $self = shift;
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('id');
  my $raw_id = $params->{id} || "";
  $self->_common_params_settings({'title' => 'KnomiGO',
				  'amigo_mode' => 'knomigo'});

  use GO::CGI::Utilities;

  ## Try and collect the different ID variants.
  my $know_sets = [$raw_id];
  if( index($raw_id, ':') != -1 ){
    my $cdr = substr($raw_id, index($raw_id, ':') +1);
    if( $cdr ){
      push @$know_sets, $cdr;
      if( index($cdr, ':') != -1 ){
	my $cddr = substr($cdr, index($cdr, ':') +1);
	if( $cddr ){
	  push @$know_sets, $cddr;
	}
      }
    }
  }

  my $know_p = 0;
  my $know_results = [];
  if( defined $raw_id and $raw_id ){

    $self->{CORE}->kvetch("will try and resolve id: ". $raw_id);
    $know_p = 1;

    ## Setup dbh.
    my $apph = GO::CGI::Utilities::create_apph;
    my $dbh = $apph->dbh;

    ###
    ### Collect info.
    ### The format is [true|false, message, count (not shown if 0), sql]
    ###

    my $sql = undef;
    for(my $i = 0; $i < scalar(@$know_sets); $i++ ){

      my $set_results = [];
      my $id = $$know_sets[$i];

      ## dbxref
      $sql = _q_dbxref($id);
      if( my $cnt = _wrap_sql_count($dbh, $sql) ){
	push @$set_results, [1, "Found in dbxref table", $cnt, $sql];

	## gp dbx
	push @$set_results, _simple_probe_runner($dbh, _q_gp_dbxacc($id),
						"Found GP acc dbxref",
						"Not found as GP acc dbxref");
	## seq dbx
	push @$set_results, _simple_probe_runner($dbh, _q_seq_dbxacc($id),
						"Found sequence dbxref",
						"Not found as sequence dbxref");
	## ev dbx
	push @$set_results, _simple_probe_runner($dbh, _q_ev_dbxacc($id),
						"Found evidence dbxref",
						"Not found as evidence dbxref");
      }else{
	push @$set_results, [0, "Not found in dbxref table", 0, $sql];
      }

      ## term acc
      push @$set_results, _simple_probe_runner($dbh, _q_term_acc($id),
					      "Found term acc",
					      "Not found as term acc");
      ## term name
      push @$set_results, _simple_probe_runner($dbh, _q_term_name($id),
					      "Found term name",
					      "Not found as term name");
      ## term syn acc
      push @$set_results, _simple_probe_runner($dbh, _q_term_synonym_acc($id),
					      "Found term acc synonym",
					      "Not found as term acc synonym");
      ## term syn term
      push @$set_results, _simple_probe_runner($dbh, _q_term_synonym_term($id),
					      "Found term term synonym",
					      "Not found as term term synonym");
      ## gp symbol
      push @$set_results, _simple_probe_runner($dbh, _q_gp_symbol($id),
					      "Found GP symbol",
					      "Not found as GP symbol");
      ## gp full_name
      push @$set_results, _simple_probe_runner($dbh, _q_gp_full_name($id),
					      "Found GP full name",
					      "Not found as GP full name");
      ## gp synonym
      push @$set_results, _simple_probe_runner($dbh, _q_gp_synonym($id),
					      "Found GP synonym",
					      "Not found as GP synonym");
      ## Test for TE worthiness.
      my $te1 = _simple_probe_runner($dbh,_q_gp_dbxacc($id), "?", "?");
      my $te2 = _simple_probe_runner($dbh, _q_gp_synonym($id), "?", "?");
      my $te3 = _simple_probe_runner($dbh, _q_gp_symbol($id), "?", "?");
      if( $$te1[0] || $$te2[0] || $$te3[0] ){
	push @$set_results, [1, "Possibly good in term enrichment", 0, "n/a"];
      }else{
	push @$set_results, [0, "Unusable in term enrichment", 0, "n/a"];
      }

      push @$know_results, $set_results;
    }
  }

  ###
  ### Template-y and return-y stuff.
  ###

  $self->set_template_parameter('KNOW_SETS', $know_sets);
  $self->set_template_parameter('KNOW_RESULTS', $know_results);
  $self->set_template_parameter('KNOW_P', $know_p);
  $self->set_template_parameter('RAW_ID', $raw_id);

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please

  ## Our AmiGO services CSS.
  my $prep = {css_library => ['standard'],
	      content => ['html/main/knomigo_summary.tmpl']};
  $self->add_template_bulk($prep);

  ## ...
  return $self->generate_template_page();
}


## Returns the number of results for a query.
sub _simple_probe_runner {

  my $dbh = shift || die "need dbh";
  my $sql = shift || die "need sql";
  my $yes_m = shift || die "need yes message";
  my $no_m = shift || die "need no message";

  my $ret = undef;

  if( my $cnt = _wrap_sql_count($dbh, $sql) ){
    $ret = [1, $yes_m, $cnt, $sql];
  }else{
    $ret = [0, $no_m, 0, $sql];
  }

  return $ret;
}

## Returns the number of results for a query.
sub _wrap_sql_count {

  my $dbh = shift || die "need dbh";
  my $sql = shift || die "need sql arg";

  #$core->kvetch($complete_query);
  my $sth = $dbh->prepare($sql)
    or die "Couldn't prepare statement: " . $dbh->errstr;
  $sth->execute()
    or die "Couldn't execute statement: " . $sth->errstr;

  #my @row = $sth->fetchrow_array();
  my $rows = $sth->fetchall_arrayref({});
  return scalar(@$rows) || 0;
}


## Returns...
sub _q_dbxref {
  my $id = shift || die "need id";
  my $query = "select * from dbxref where dbxref.xref_key = '$id'";
  return $query;
}


## Returns...
sub _q_term_acc {
  my $id = shift || die "need id";
  my $query = "select * from term where term.acc = '$id'";
  return $query;
}

## Returns...
sub _q_term_name {
  my $id = shift || die "need id";
  my $query = "select * from term where term.name = '$id'";
  return $query;
}

## Returns...
sub _q_term_synonym_acc {
  my $id = shift || die "need id";
  my $query = "select * from term_synonym where term_synonym.acc_synonym = '$id'";
  return $query;
}

## Returns...
sub _q_term_synonym_term {
  my $id = shift || die "need id";
  my $query = "select * from term_synonym where term_synonym.term_synonym = '$id'";
  return $query;
}

## Returns...
sub _q_gp_symbol {
  my $id = shift || die "need id";
  my $query = "select * from gene_product where gene_product.symbol = '$id'";
  return $query;
}

## Returns...
sub _q_gp_full_name {
  my $id = shift || die "need id";
  my $query = "select * from gene_product where gene_product.full_name = '$id'";
  return $query;
}

## Returns...
sub _q_gp_synonym {
  my $id = shift || die "need id";
  my $query = "select * from gene_product_synonym where gene_product_synonym.product_synonym = '$id'";
  return $query;
}


## Returns...
sub _q_gp_dbxacc {
  my $id = shift || die "need id";
  my $query = "select * from gene_product, dbxref where gene_product.dbxref_id = dbxref.id and dbxref.xref_key = '$id'";
  return $query;
}


## Returns...
sub _q_seq_dbxacc {
  my $id = shift || die "need id";
  my $query = "select * from seq_dbxref, dbxref where seq_dbxref.dbxref_id = dbxref.id and dbxref.xref_key = '$id'";
  return $query;
}


## Returns...
sub _q_term_dbxacc {
  my $id = shift || die "need id";
  my $query = "select * from term_dbxref, dbxref where term_dbxref.dbxref_id = dbxref.id and dbxref.xref_key = '$id'";

  return $query;
}


## Returns...
sub _q_ev_dbxacc {
  my $id = shift || die "need id";
  my $query = "select * from evidence, dbxref where evidence.dbxref_id = dbxref.id and dbxref.xref_key = '$id'";

  return $query;
}



1;
