package AmiGO::WebApp::HTMLClient;
use base 'AmiGO::WebApp';

####
#### TODO/BUG: session_id needs to be stored as a cookie, caching reasons, etc.
####
#### TODO: replace internal $core calls with the one saved in
#### AmiGO::WebApp::$self as much as possible (save on things like
#### species caching, etc.)
####
#### NOTE: use gene_product_count table to calculate information.
####

use strict;

use Data::Dumper;

#use CGI::Application::Plugin::DBH (qw/dbh_config dbh/);
use CGI::Application::Plugin::Session;
use CGI::Application::Plugin::TT;
use CGI::Application::Plugin::Redirect;

use AmiGO::WebApp::Input;
use GOBO::DBIC::GODBModel::Schema;
use JSON;

use AmiGO::Aid::ReferenceGenome;

## Real external workers.
use AmiGO::Worker::Term;
use AmiGO::Worker::GeneProductCount;
use AmiGO::External::XML::GONUTS;
use AmiGO::Worker::HomolsetGraph2;
use AmiGO::Worker::HomolsetSummary2;
use AmiGO::Worker::GPInformation::HomolsetInformation;
use AmiGO::Worker::QuickGO::OntGraphics;

#use URI::Escape;
#use CGI qw/escapeHTML/;

#use Utility::Cart;

#use Cache::Memcached; # TODO: can't go bigger than 1MB (still,
#                      # probably best to explore);
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
						{Directory=> $self->{CORE}->amigo_env('AMIGO_CGI_ROOT_DIR') . '/sessions'}
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
  $self->start_mode('software_list');
  $self->error_mode('mode_fatal');
  #$self->tt_include_path('templates/html/inc');
  $self->run_modes(
		   'visualize'           => 'mode_visualize',
		   'software_list'       => 'mode_software_list',
		   'homolset_summary'    => 'mode_homolset_summary',
		   'homolset_graph'      => 'mode_homolset_graph',
		   'homolset_annotation' => 'mode_homolset_annotation',
		   'live_search'         => 'mode_live_search',
		   'term_details'        => 'mode_term_details4',
		   'term_details4'        => 'mode_term_details4',
		   'term_details5'        => 'mode_term_details5',
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
    $self->add_template_content('html/main/visualize.tmpl');
    $output = $self->generate_template_page();

  }else{

    ## What kind of data do we have?
    #     my $data_type = 'string';
    #     if( $input_term_data_type eq 'json' ){
    #       $data_type = 'json';
    #     }

    #$self->{CORE}->kvetch(Dumper($input_term_data_type));
    #$self->{CORE}->kvetch(Dumper($input_term_data));
    #print STDERR Dumper($input_term_data_type);
    #print STDERR Dumper($input_term_data);
    #print STDERR $input_term_data;

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
  $self->add_template_content('html/main/software_list.tmpl');
  return $self->generate_template_page();
}


## TODO: Maybe the caching stuff should be pushed out to AmiGO.pm?
sub mode_homolset_summary {

  my $self = shift;

  ## TODO: Would these actually be needed?
  #   $self->{CACHE}=
  #     new Cache::FileCache->new({'namespace' => 'AmiGO',
  # 			       'cache_root' =>
  # 			       '/srv/www/cgi-bin/amigo/sessions'});
#  my $cache =
#    new Cache::Memcached->new({'servers' => ["127.0.0.1:11211"]});
  # $self->{SCHEMA} = GOBO::DBIC::GODBModel::Schema->connect($self->db_connector());
  #   $self->{SCHEMA} =
  #     GOBO::DBIC::GODBModel::Schema->connect($self->db_connector(),
  # 				  { cursor_class =>
  # 				    'DBIx::Class::Cursor::Cached' });
  #   $self->{SCHEMA}->default_resultset_attributes({
  # 						 cache_object =>
  # #						 Cache::FileCache->new({ namespace => 'SchemaClass' })});
  # 						 Cache::Memcached->new({'servers' => ["127.0.0.1:11211"]})});;

  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('homolset_summary');
  my $cache = $params->{cache};
  $self->_common_params_settings({title=>'AmiGO: Homolog Set Summary',
				  'amigo_mode' => 'homolset_summary'});
  ## Try cache, and if not, render.
  #my $output =
  #  $self->{CORE}->file_pre_render($self->{CORE}->get_interlink({mode=>'homolset_summary'}));
#  my $key = $self->{CORE}->get_interlink({mode=>'homolset_summary'});
  #  my $output = $cache->get($key . 'foo');
  my $output =
    $self->{CORE}->file_pre_render($self->{CORE}->get_interlink({mode=>'homolset_summary'}));
  if( defined $output && $cache eq 'yes' ){
    $self->{CORE}->kvetch("_will USE cache");
  }else{
    $self->{CORE}->kvetch("_will NOT use cache");

    my $summary = AmiGO::Worker::HomolsetSummary2->new();
    my $summary_data = $summary->get_summary_data();

    $self->set_template_parameter('RESULTS_MATRIX', $summary_data);
    $self->set_template_parameter('STATUS_COLORS', $rg_aid->get_status_colors());

    #$self->{CORE}->kvetch(Dumper($summary_data), 1);

    my @alphabet = qw(a b c d e f g h i j k l m n o p q r s t u z w x y z);
    $self->set_template_parameter('ALPHABET', \@alphabet);
    $self->set_template_parameter('SPECIES_LIST',
				  $rg_aid->species_list({num_p=>0,safely=>1}));

    ## Create an ordering for the data...
    my $hs_info = $summary->get_homolset_information();
    $self->set_template_parameter('HS_INFO', $hs_info);
    #map { $self->kvetch("___ $_", 1) }
    #my @ordered_data = sort{ $hs_symbols->{$a} cmp $b } @unordered_data;
    #$params->{ORDERED_DATA} = \@ordered_data;

    ## Create an alpha ordering...
    my $alphabetical_data = {};
    #$self->{CORE}->kvetch("___scalar: " . scalar(keys %$summary_data), 1);
    foreach my $id ( keys %$summary_data ){
      #$self->{CORE}->kvetch("___ $id", 1);
      my $sym = $hs_info->{$id}{symbol};
      my $letter = lc(substr($sym, 0, 1));
      if( ! defined $alphabetical_data->{$letter} ){
	$alphabetical_data->{$letter} = [];
      }
      push @{$alphabetical_data->{$letter}}, $id;
      #$self->{CORE}->kvetch("___ $letter ... $id", 1);
    }

    ## Now, within each letter group, let's get the ordering correct.
    foreach my $letter (keys %$alphabetical_data){

      $self->{CORE}->kvetch("___ sorting within $letter ...");
      my @new_inner_array = sort {
	my $ai = $hs_info->{$a}{symbol};
	my $bi = $hs_info->{$b}{symbol};
	$self->{CORE}->kvetch("     $a vs $b and $ai vs $bi");
	return $ai cmp $bi;
      } @{$alphabetical_data->{$letter}};
      $alphabetical_data->{$letter} = \@new_inner_array;
    }

    $self->set_template_parameter('ALPHABETICAL_DATA', $alphabetical_data);
    $self->add_template_content('html/main/homolset_summary.tmpl');
    $output = $self->generate_template_page();

    #$self->{CORE}->kvetch("_cache stored..." );
    #$self->{CORE}->kvetch("_cache stored badly..." ) if !
    #  $cache->set($key, $output);
  }

  return $output;
}


##
sub mode_homolset_graph {

  my $self = shift;

  ## NOTE: memcache still doesn't like the image sizes...
  #   my $cache =
  #     new Cache::Memcached->new({'servers' => ["127.0.0.1:11211"]});

  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('homolset_graph');

  my $hgraph = AmiGO::Worker::HomolsetGraph2->new();

  ## Try for prerender, but if not found, go to renergation.
  my $format = $params->{format};
  my $set = $params->{set};
  my $cache = $params->{cache};
  my $output =
    $self->{CORE}->file_pre_render($self->{CORE}->get_interlink({mode=>'homolset_graph',
  						 arg=>{format=>$format,
  						       set=>$set}}));
  #   my $key = $self->{CORE}->get_interlink({mode=>'homolset_graph',
  # 				  arg=>{format=>$format,
  # 					set=>$set}});
  #   my $output = $cache->get($key);
  if( defined $output && $cache eq 'yes' ){
    $self->{CORE}->kvetch("_will USE cache");
  }else{
    $self->{CORE}->kvetch("_will NOT use cache");

    ## Generate it ourselves.
    if( $format && $format eq 'dot' ){
      $output = $hgraph->make_graph($set, 'dot'); # if ! defined $output;
    }elsif( $format && $format eq 'png' ){
      $output = $hgraph->make_graph($set, 'png'); # if ! defined $output;
    }else{
      $output = $hgraph->make_graph($set, 'svg'); # if ! defined $output;
    }

    #     $self->{CORE}->kvetch("_cache stored..." );
    #     $self->{CORE}->kvetch("_cache stored badly..." ) if !
    #       $cache->set($key, $output);
  }

  ## Get the right header.
  if( $format && $format eq 'dot' ){
    $self->header_add( -type => 'text/plain' );
  }elsif( $format && $format eq 'png' ){
    $self->header_add( -type => 'image/png' );
  }else{
    $self->header_add( -type => 'image/svg+xml' );
  }

  return $output;
}


##
sub mode_homolset_annotation {

  my $self = shift;
  my $output = '';

  ## Input handling.
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('homolset_annotation');
  my $set = $params->{set};
  my $order = $params->{order};

  my $rg_info =
    AmiGO::Worker::GPInformation::HomolsetInformation->new({skip_roots=>1});
  if( ! $rg_info->calculate($set) ){
    $output = $self->mode_die_with_message('Seem to have bad set input!');
  }else{

    $self->_common_params_settings({title=>'AmiGO: Homolog Set Details'});

    ##
    #$rg_info->remove_association_duplicates();
    my $assoc_matrix = $rg_info->get_matrix();
    my $seen_terms = $rg_info->get_terms();
    my $seen_species = $rg_info->get_species();
    my $seen_ontologies = $rg_info->get_ontologies();
    my $seen_species_by_ontology = $rg_info->get_species_by_ontology();
    my $rg_symbol = $rg_info->get_symbol();

    ### Sort out all of the species.
    my $species = $rg_aid->species_list({num_p=>1,safely=>1});
    my $species_lookup = $rg_aid->species_information($species);
    $self->set_template_parameter('LOOKUP_FOR_SPECIES', $species_lookup);
    $self->set_template_parameter('SEEN_SPECIES', $seen_species);
    $self->set_template_parameter('ORDERED_LIST_FOR_SPECIES', $species);
    $self->set_template_parameter('SEEN_SPECIES_BY_ONTOLOGY',
				  $seen_species_by_ontology);

    ### Sort terms into different ontologies. By using a hash, we'll
    ### compress out multiple annotations by species for the purpose of
    ### rendering.

    my %term_hash_by_ont = (); # we'll be using this one below as well
    foreach my $t (keys %$seen_terms){

      my $term = $seen_terms->{$t};
      my $ont = $term->term_type;

      $term_hash_by_ont{$ont} = {}
	if ! defined( $term_hash_by_ont{$ont});
      $term_hash_by_ont{$ont}{$t} = 1;
    }

    ## Add them into the parameters.
    my @ordered_ontologies =
      sort {
	## A little GO hardwiring...fall through to cmp if not in GO.
	if( $a eq 'molecular_function' || $b eq 'cellular_component' ){
	  return -1;
	}elsif( $b eq 'molecular_function' || $a eq 'cellular_component' ){
	  return 1;
	}else{
	  return $a cmp $b;
	}
      } keys %$seen_ontologies;
    $self->set_template_parameter('ONTOLOGIES', \@ordered_ontologies);
    my %ontologies_readable =
      map{ $_ => $rg_aid->readable($_) } @ordered_ontologies;
    $self->set_template_parameter('ONTOLOGIES_READABLE', \%ontologies_readable);

    ###
    ### Ordering logic.
    ### Order terms within the ontology separation hash.
    ###

    ##
    #$order = 'name' if $order eq 'default';
    $order = 'information' if $order eq 'default';
    $self->set_template_parameter('ORDER', $order);
    $self->set_template_parameter('POSSIBLE_ORDERS',
      {
       'name' => $self->{CORE}->get_interlink({mode=>'homolset_annotation',
				       arg=>{set=>$set,
					     order=>'name'}}),
       'depth' => $self->{CORE}->get_interlink({mode=>'homolset_annotation',
					arg=>{set=>$set,
					      order=>'depth'}}),
       'information' => $self->{CORE}->get_interlink({mode=>'homolset_annotation',
					      arg=>{set=>$set,
						    order=>'information'}}),
      });

    ###
    ### Define the sorting algoritms (can't really factor these out
    ### unless I resort to dynaamic variables).
    ###

    ## GP count for a term.
    my $gp_count_for_term = sub {

      my $term = shift;
      my $acc = $term->acc;

      #$self->{CORE}->kvetch("\ttrying: " . $acc . ' ' . $term);

      my $gp_count = 0;
      if( defined $term->gene_product_count ){
	foreach my $gpc ($term->gene_product_count->all){
	  if( defined $gpc->speciesdbname ){
	    #$self->{CORE}->kvetch("\t" . $gpc->speciesdbname .
	    #		  ' (' . $gpc->product_count . ')');
	    $gp_count += $gpc->product_count;
	  }
	}
      }
      return $gp_count;
    };

    ## Fill the information content cache, which we'll need for
    ## sorting and display.
    ## GP count for roots added up...
    my $hgraph = GOBO::DBIC::GODBModel::Graph->new();
    my $roots = $hgraph->get_roots();
    my $total_gp_count = 0;
    foreach my $acc (keys %{$roots}){
      $total_gp_count += &$gp_count_for_term($roots->{$acc});
      #$self->{CORE}->kvetch("___" . $total_gp_count, 1);
    }

    ## IC(t) = -1 * logn((|annot(t)| / total_annotated_entities), 2)
    my $info_cont_cache = {};
    foreach my $acc (keys %{$seen_terms}){
      ## TODO: add full info content.
      my $annot = &$gp_count_for_term($seen_terms->{$acc});
      $info_cont_cache->{$acc} =
	sprintf("%.2f", -1 * (log($annot / $total_gp_count) / log(2)));
      #$self->{CORE}->kvetch("___" . $info_cont_cache->{$acc}, 1);
    }
    $self->set_template_parameter('INFORMATION_BY_TERM', $info_cont_cache);

    ## Sort by term name.
    my $name_sort = sub {
      my $term_a = $seen_terms->{$a};
      my $term_b = $seen_terms->{$b};
      $term_a->name cmp $term_b->name;
    };

    ## Sort by term depth (taken from Graph info).
    my $depth_sort = sub {
      my $a_depth = $rg_info->get_term_depth($a);
      my $b_depth = $rg_info->get_term_depth($b);
      $a_depth <=> $b_depth;
    };

    ## Sort terms by information content.
    my $info_sort = sub {
      my $a_count = $info_cont_cache->{$a};
      my $b_count = $info_cont_cache->{$b};
      $b_count <=> $a_count;
    };

    ## Choose the ordering function.
    my $order_fun = undef;
    if( $order eq 'depth' ){
      $order_fun = $depth_sort;
    }elsif( $order eq 'information' ){
      $order_fun = $info_sort;
    }elsif( $order eq 'name' ){
      $order_fun = $name_sort;
    }else{
      $order_fun = $info_sort;
    }

    ## Sort terms in lists.
    my %ordered_term_array_by_ont = ();
    foreach my $ont (keys %term_hash_by_ont){
      my @foo = sort $order_fun keys %{$term_hash_by_ont{$ont}};
      $ordered_term_array_by_ont{$ont} = \@foo;
    }
    $self->set_template_parameter('TERMS_BY_ONTOLOGY',
				  \%ordered_term_array_by_ont);

    ###
    ###
    ###

    $self->set_template_parameter('STATUS_COLORS', $rg_aid->get_status_colors());
    $self->set_template_parameter('LOOKUP_FOR_ASSOC', $assoc_matrix);

    my @foo = map{ $seen_terms->{$_} } keys %$seen_terms;
    $self->set_template_parameter('LOOKUP_FOR_TERM',
				  $rg_aid->term_information(\@foo));

    ## Get some important internal links for the page.
    $self->set_template_parameter('SYMBOL', $rg_info->get_symbol());
    $self->set_template_parameter('GRAPH_LINK_SVG',
				  $self->{CORE}->get_interlink({mode=>'homolset_graph',
							arg=>{format=>'svg',
							      set=>$set}}));
    $self->set_template_parameter('GRAPH_LINK_PNG',
				  $self->{CORE}->get_interlink({mode=>'homolset_graph',
							arg=>{format=>'png',
							      set=>$set}}));
    my $core_terms = $rg_info->get_direct_terms();
    $self->set_template_parameter('AMIGO_BROWSER_LINK',
				  $self->{CORE}->get_interlink({mode=>'browse',
							arg=>{action=>'set-tree',
							      terms=>$core_terms}}));
    #terms=>['GO:0000437', 'GO:0000430']}});
    $self->set_template_parameter('SUMMARY_LINK_MAIN',
				  $self->{CORE}->get_interlink({mode=>'homolset_summary'}));
    $self->set_template_parameter('SUMMARY_LINK_DETAILED',
				  $self->{CORE}->get_interlink({mode=>'homolset_summary', arg=>{jump=>$set}}));

    $self->add_template_content('html/main/homolset_annotation2.tmpl');
    $output = $self->generate_template_page();
  }

  return $output;
}


## A committed client based on the jQuery libraries. Attempt at
## production based off of mode_heavy_client_jquery.
sub mode_live_search {

  my $self = shift;

  ## Non-standard settings.
  $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please
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
      'org.bbop.amigo.ui.widgets'
     ],
     javascript_library =>
     [
      'com.jquery',
      'com.jquery-ui',
      'org.bbop.amigo',
      'org.bbop.amigo.go_meta',
      'org.bbop.amigo.live_search',
      'org.bbop.amigo.ui.widgets'
     ],
     javascript =>
     [
      $self->{JS}->get_lib('LiveSearch.js')
     ],
     javascript_init =>
     [
      'LiveSearchInit();'
     ],
     content =>
     [
      'html/main/live_search.tmpl'
     ]
    };
  $self->add_template_bulk($prep);

  return $self->generate_template_page();
}

## TODO/BUG: Temporary muxing while we get the style figured out.
sub mode_term_details4 {
  my $self = shift;
  return mode_term_details($self, 'type4');
}
sub mode_term_details5 {
  my $self = shift;
  return mode_term_details($self, 'type5');
}


##
## Nice example: amigo/term-details.cgi?term=GO:0022008
## TODO: things we need to fish out in the model:
##   * TODO
sub mode_term_details {

  my $self = shift;
  my $type = shift || undef; # TODO/BUG: temporary muxing
  my $acc_list_for_gpc_info = [];

  ##
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('term');
  my $input_term_id = $params->{term};
  $self->_common_params_settings({'title' =>
				  'AmiGO: Term Details for ' . $input_term_id});

  ## TODO/BUG: these need to go later.
  my $graph_type = $params->{graph_type};

  ## Input sanity check.
  if( ! $input_term_id ){
    return $self->mode_die_with_message('Bad, or no, acc!');
  }
  push @$acc_list_for_gpc_info, $input_term_id;

  ###
  ### Get full term info.
  ###

  # my $term_q = AmiGO::Worker::Term->new("excessive");
  my $term_q = AmiGO::Worker::Term->new();
  my $term_info = $term_q->get_info($input_term_id);
  if( ! defined($term_info) || $self->{CORE}->empty_hash_p($term_info) ){
    return $self->mode_die_with_message('Unable to find any such acc!');
  }

  ##
  my $foo = (keys %$term_info)[0];
  $self->{CORE}->kvetch('$term_info: ' . Dumper($term_info->{$foo}));
  $self->set_template_parameter('TERM_INFO', $term_info->{$foo});

  ## TODO: spin into AmiGO::Worker somewhere.
  ## Graph on.
  my $graph = GOBO::DBIC::GODBModel::Graph->new();
  # $graph->verbose(1);

  ###
  ### Separately, get lower term neighborhood.
  ###

  ##
  my $child_chunks = [];

  my $child_rels = $graph->get_child_relationships($input_term_id);
  #$self->{CORE}->kvetch('_a_: ' . $child_rels);
  #$self->{CORE}->kvetch('_b_: ' . scalar(@$child_rels));
  foreach my $child_rel (@$child_rels){

    my $rel = $child_rel->relationship; #->name;
    #my $obj = $child_rel->object;
    my $sub = $child_rel->subject;

    # $self->{CORE}->kvetch('_c.r_: ' . $rel->name);
    # $self->{CORE}->kvetch('_c.s_: ' . $sub->acc);

    # ## Gross node position in graph.
    # my $node_position = 'central';
    # if( $graph->is_root_p($sub->acc) ){
    #   $node_position = 'root';
    # }elsif( $graph->is_leaf_p($sub->acc) ){
    #   $node_position = 'leaf';
    # }
    # $self->{CORE}->kvetch('_d_: ' . $node_position);

    my $sub_acc = $sub->acc;
    push @$child_chunks,
      {
       acc => $sub_acc,
       name => $sub->name,
       rel => $rel->name,
       link => $self->{CORE}->get_interlink({mode => 'term_details',
				     arg => {acc => $sub_acc},
				    }),
       #optional => {frag => 'lineage'}}),
      };
    push @$acc_list_for_gpc_info, $sub_acc;
  }

  ## TODO: absolute ordering (need operator over term)
  my @sorted_child_chunks = sort {
    lc($a->{name}) cmp lc($b->{name})
  } @$child_chunks;
  $self->set_template_parameter('CHILD_CHUNKS', \@sorted_child_chunks);

  ###
  ###
  ###

  #$self->{CORE}->kvetch("Start lineage: " . $input_term_id);
  my($lnodes, $lnode_rel, $lnode_rel_inf, $lnode_depth, $max_ldepth) =
    $graph->lineage($input_term_id);
  # $self->{CORE}->kvetch('lnodes: ' . Dumper($lnodes));
  # $self->{CORE}->kvetch('lnode_rel: ' . Dumper($lnode_rel));
  # $self->{CORE}->kvetch('lnode_depth: ' . Dumper($lnode_depth));
  # $self->{CORE}->kvetch('max_depth: ' . Dumper($max_ldepth));
  #$self->{CORE}->kvetch("Stop lineage");
  #die;

  ###
  ### Climb tree back to root and grab chunks.
  ### The output from this section should an ordered array of arrays,
  ### with chunk information similar to the children below.
  ### '$graph->climb' gets self, so we'll edit that out by hand.
  ### TODO: Can be spun out?
  ###

  ## Grab core information.
  #my($anodes, $aedges, $adesc, $aanc, $adepth) = $graph->climb($input_term_id);
  #$self->{CORE}->kvetch("clumb:\n" . Dumper(\@akeys));

  ## Sort into buckets depending on reported depth.
  my $nodes_by_depth = {};
  # my $max_depth = 0;
  foreach my $acc (keys %$lnodes){
   
    #$self->{CORE}->kvetch("clumb: " . $acc);

    ## Only continue if not self.
    if( $acc ne $input_term_id ){

      ## 
      my $depth = $lnode_depth->{$acc};
      if( ! defined $nodes_by_depth->{$depth} ){
	$nodes_by_depth->{$depth} = [];
      }

      ## Add manufactured struct.
      my $term = $lnodes->{$acc};
      my $rel = $lnode_rel->{$acc};
      my $inf = $lnode_rel_inf->{$acc};
      # my $rel = $graph->get_relationship($acc, $input_term_id);

      ## Looks like it's not a member of this "reduced" graph.
      ## TODO/BUG: this graph_type stupidity will be removed once we're
      ## settled.
      if( defined $rel || $graph_type eq 'all' ){

	##
	if( ! defined $rel ){ $rel = 'fatal'; }

	#$self->{CORE}->kvetch("_rel: " . $rel);
	push @{$nodes_by_depth->{$depth}},
	  {
	   acc => $acc,
	   inferred_p => $inf,
	   name => $term->name,
	   rel => $rel,
	   link => $self->{CORE}->get_interlink({
					 mode => 'term_details',
					 arg => {acc => $acc},
					}),
	  };
	push @$acc_list_for_gpc_info, $acc;
      }else{
	## There probably won't be a need for "fatal"s now...
	# $rel = 'fatal';
      }
    }
  }
  $self->{CORE}->kvetch("nodes_by_depth:\n" . Dumper($nodes_by_depth));
  #$self->{CORE}->kvetch("adepth:\n" . Dumper($adepth));
  #$self->{CORE}->kvetch("_max_depth: " . $max_depth);
  my $nodes_sorted_by_depth = {};
  for( my $depth = 0; $depth < $max_ldepth; $depth++ ){
    #$self->{CORE}->kvetch("_depth: " . $depth);
    if( defined $nodes_by_depth->{$depth} ){
      my @blah = sort {
	lc($a->{name}) cmp lc($b->{name})
      } @{$nodes_by_depth->{$depth}};
      $nodes_sorted_by_depth->{$depth} = \@blah;
    }
    #$self->{CORE}->kvetch("nbd:\n" .Dumper($nodes_by_depth->{$depth}));
    #$self->{CORE}->kvetch("nsbd:\n" .Dumper($nodes_sorted_by_depth->{$depth}));
  }

  ## Create a mapping between terms and a random address.
  my $rand_to_acc = {};
  my $acc_to_rand = {};
  for( my $i = 0; $i < scalar(@$acc_list_for_gpc_info); $i++ ){
    my $acc = $acc_list_for_gpc_info->[$i];
    my $rand = $self->{CORE}->unique_id();
    $rand_to_acc->{$rand} = $acc;
    $acc_to_rand->{$acc} = $rand;
  }
  $self->set_template_parameter('ACC_TO_RAND', $acc_to_rand);
  $self->set_template_parameter('RAND_TO_ACC', $rand_to_acc);
  #$self->set_template_parameter('GENE_PRODUCT_ASSOCIATIONS_COUNT', 'foo');

  # ## 
  # $self->{CORE}->kvetch("nodes_sorted_by_depth:\n" .
  # 			Dumper($nodes_sorted_by_depth));
  # $self->{CORE}->kvetch('lnode_rel_inf: ' . Dumper($lnode_rel_inf));

  # $self->{CORE}->kvetch("max depth: " . $max_ldepth);

  ## Collect path addresses for javascript line to inferred.
  my $node_addresses = {};
  my $total_address_rows = 0;
  if( $nodes_sorted_by_depth && $max_ldepth ){
    my $d = 0;
    while( $d <= $max_ldepth ){
      if( $nodes_sorted_by_depth->{$d} ){
	my $anc_chunks = $nodes_sorted_by_depth->{$d};
        foreach my $anc (@$anc_chunks){
	  #$self->{CORE}->kvetch("address: " . $anc->{acc});
	  if( defined $lnode_rel_inf->{$anc->{acc}} &&
	      $lnode_rel_inf->{$anc->{acc}} == 1 ){
	    # $self->{CORE}->kvetch("addr: " . $anc->{acc} . ', ' .
	    # 			  $total_address_rows . ', ' . $d);
	    $node_addresses->{$anc->{acc}} =
	      {
	       row => $total_address_rows,
	       column => $d,
	      };
	  }
	  $total_address_rows++;
	}
      }
      $d++;
    }
  }
  my $home_address =
    {
     row => $total_address_rows,
     column => scalar( keys %$nodes_sorted_by_depth ),
    };
  # $self->{CORE}->kvetch("addresses:\n" . Dumper($node_addresses));
  # $self->{CORE}->kvetch("home address:\n" . Dumper($home_address));

  ## Add variables to template.
  $self->set_template_parameter('MAX_DEPTH', $max_ldepth);
  $self->set_template_parameter('MAX_DISPLACEMENT', $max_ldepth + 2);
  $self->set_template_parameter('PARENT_CHUNKS_BY_DEPTH',
				$nodes_sorted_by_depth);

  ###
  ### Pull gene_product_count info.
  ###

  #print STDERR "<<TIME_START>>\n";
  my $gpc_q = AmiGO::Worker::GeneProductCount->new($acc_list_for_gpc_info);
  #print STDERR "<<TIME_MID>>\n";
  my $gpc_info = $gpc_q->get_info();
  my $gpc_count = $gpc_q->get_count($input_term_id);
  #print STDERR "<<TIME_END>>\n";
  $self->set_template_parameter('GENE_PRODUCT_ASSOCIATIONS_COUNT', $gpc_count);

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

  ## Bridge variables from old system.
  $self->set_template_parameter('cgi', 'term-details');
  $self->set_template_parameter('vbridge', 'term=' . $input_term_id);

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

  ## Try images from QuickGO.
  my $qg = AmiGO::Worker::QuickGO::OntGraphics->new();
  my $qg_img_url = undef;
  eval {
    $qg_img_url = $qg->image_path($input_term_id);
  };
  if($@){
    $self->{CORE}->kvetch("QuickGO image failed: $@");
  }else{
    $self->{CORE}->kvetch("QuickGO image on FS: $qg_img_url");
  }
  $self->set_template_parameter('QG_IMAGE_URL', $qg_img_url);

  ###
  ### Standard setup.
  ### TODO: We see this a lot--should this be abstracted out too? No?
  ###

  ## Non-standard settings.
  # $self->set_template_parameter('STANDARD_YUI', 'no'); # no YUI please
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
      # 'org.bbop.amigo.opensearch',
      # 'org.bbop.amigo.ui.autocomplete'
     ],
     javascript =>
     [
      $self->{JS}->make_var('global_addresses', $node_addresses),
      $self->{JS}->make_var('global_home_address', $home_address),
      $self->{JS}->make_var('global_count_data', $gpc_info),
      $self->{JS}->make_var('global_rand_to_acc', $rand_to_acc),
      $self->{JS}->make_var('global_acc_to_rand', $acc_to_rand),
      $self->{JS}->make_var('global_acc', $input_term_id)
     ]
    };
  $self->add_template_bulk($prep);

  ## Initialize javascript app.
  $self->add_template_javascript($self->{JS}->get_lib('TermDetails.js'));
  $self->add_template_javascript($self->{JS}->initializer_jquery('TermDetailsInit();'));

  ##
  if( $type eq 'type0' ){
    $self->add_template_content('html/main/term_details.tmpl');
  }elsif( $type eq 'type1' ){
    $self->add_template_content('html/main/term_details1.tmpl');
  }elsif( $type eq 'type2' ){
    $self->add_template_content('html/main/term_details2.tmpl');
  }elsif( $type eq 'type3' ){
    $self->add_template_content('html/main/term_details3.tmpl');
  }elsif( $type eq 'type4' ){
    $self->add_template_content('html/main/term_details4.tmpl');
  }else{
    $self->add_template_content('html/main/term_details5.tmpl');
  }

  return $self->generate_template_page();
}



1;
