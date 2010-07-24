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
use AmiGO::External::JSON::LiveSearch::Term;
use AmiGO::External::JSON::LiveSearch::GeneProduct;
use AmiGO::External::GODB::Query;
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

use Cache::Memcached; # TODO: can't go bigger than 1MB (still,
                      # probably best to explore);
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
		    $self->{CORE}->amigo_env('GO_ROOT') .
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
		   'workspace_client'    => 'mode_workspace_client',
		   'layers_graph'       => 'mode_layers_graph',
		   'hierarchical'        => 'mode_hierarchical',
		   'heavy_client_ext'    => 'mode_heavy_client_ext',
		   'heavy_client_jquery' => 'mode_heavy_client_jquery',
		   'homolset_summary'    => 'mode_homolset_summary',
		   #'report_1'            => 'mode_report_1',
		   #'lexical_search'      => 'mode_lexical_search',
		   #'exp_lexical_search'  => 'mode_exp_lexical_search',
		   #'orb'                 => 'mode_orb',
		   #'coannot_matrix'      => 'mode_coannot_matrix',
		   #'orb_client'          => 'mode_orb_client',

		   'report_slimmerish_1'        => 'mode_report_slimmerish_1',

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
    $self->add_template_css( $self->{JS}->get_css($css_lib) );
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
    my $q = AmiGO::External::GODB::Query->new($props, 1000000, 1000000);

    ## 
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

# ##
# sub mode_orb_client {

#   my $self = shift;

#   #my $i = AmiGO::WebApp::Input->new();
#   #my $params = $i->input_profile('homolset_summary');
#   my $params = $self->_common_params_settings();
#   #my $output = $orb_c->make_page($params->{order}, $params->{show_ev});
#   #my $orb_client = AmiGO::Worker::ORBClient->new();
#   #my $page = $orb_client->make_page();

#   ## TODO/BUG: Somehow set the output to svg/text
#   return $self->_generate_heavy_page($params,
# 				     'html/main/orb_client.tmpl',
# 				     ['html/inc/orb_bootstrap.tmpl']);
# }


# ##
# sub mode_orb {

#   my $self = shift;

#   my $i = AmiGO::WebApp::Input->new();
#   my $params = $i->input_profile('orb');

#   _common_params_settings($params);

#   my $format = $params->{format};
#   my $request = $params->{request};

#   if( $request eq 'trackers' ){
#     $self->header_add( -type => 'text/xml' );

#   }elsif( $request eq 'tracker_information' ){

#   }

#   if( $format eq 'obo' || $format eq 'js' ){
#     $self->header_add( -type => 'text/plain' );
#   }elsif( $format eq 'xml' ){
#     $self->header_add( -type => 'text/xml' );
#   }else{
#     ## Normal html.
#   }

#   ## Get the right header.
#   if( $format && $format eq 'dot' ){

#   }elsif( $format && $format eq 'png' ){
#     $self->header_add( -type => 'image/png' );
#   }else{
#     $self->header_add( -type => 'image/svg+xml' );
#   }

#   ## TODO/BUG: Somehow set the output to svg/text
#   return $self->_generate_heavy_page($params,
# 				     'html/main/orb_client.tmpl',
# 				     ['html/inc/orb_bootstrap.tmpl']);
# }


# ## BUG/TODO: this is currently a one-off for suzi, but I would like to
# ## roll this into a general "plugins" framework pretty soon down the
# ## road.
# sub mode_report_1 {

#   my $self = shift;

#   ## Capture any incoming parameters.
#   my $i = AmiGO::WebApp::Input->new();
#   my $params = $i->input_profile(); # no profile--straight report
#   my $format = $params->{format};
#   $self->_common_params_settings($params);

#   my $query = GOBO::DBIC::GODBModel::Query->new({type=>'association'});
#   my @e_codes = @{$self->{CORE}->experimental_evidence_codes()};
#   #my $results =
#   #  $query->get_all_results({-and=>[
# #				    {'evidence.code'=> {'=', \@e_codes}}
# #				   ]});

#   ## Cycle through the results and process them into something useful.
#   $params->{RESULTS} = [];
#   $params->{COUNT} = 0;
#   while( my $a =
# 	 $query->get_next_result({-and=>[
# 					 {'evidence.code'=> {'=', \@e_codes}}
# 					]}) ){

# #  foreach my $a (@$results){

#     $params->{COUNT}++;

#     print $a->term->acc . "\n";
#     print $a->term->name . "";
#     print $a->gene_product->symbol . "\n";
#     print $a->gene_product->dbxref->xref_dbname . "\n";
#     print $a->gene_product->dbxref->xref_dbname . "\n";

# #     push @{$params->{RESULTS}},
# #       {
# #        is_not => 'a',
# #        pm_id => 'b',
# #        go_id => 'c',
# #        go_name => 'd',
# #        go_ont => 'e',
# #        go_link => 'f',
# #        gp_symbol => 'g',
# #        gp_link => 'h',
# #        #gp_link => $self->get_interlink({mode=>'gp-details',
# #        #		     arg=>{gp=>$gp_ids,
# #        #			   session_id=>$self->{SESSION_ID}}}),
# #       };
#   }

#   my $output = '';
#   if( $format &&
#       ( $format eq 'text' ||
# 	$format eq 'txt' )){
#     $output =
#       $self->_generate_standard_page($params, 'html/main/report_1_text.tmpl');
#   }else{
#     $output =
#       $self->_generate_standard_page($params, 'html/main/report_1.tmpl');
#   }
#   return $output;
# }



1;
