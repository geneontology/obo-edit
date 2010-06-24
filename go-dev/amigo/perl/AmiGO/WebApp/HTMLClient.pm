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

##
use AmiGO::WebApp::Input;
use CGI::Application::Plugin::Session;
use CGI::Application::Plugin::TT;
use CGI::Application::Plugin::Redirect;
use AmiGO::Aid::ReferenceGenome;

use GOBO::DBIC::GODBModel::Schema;
use JSON;

use Data::Dumper;


## Real external workers.
use AmiGO::Worker::HomolsetGraph2;
use AmiGO::Worker::HomolsetSummary2;
use AmiGO::Worker::GPInformation::HomolsetInformation;

## Helper helping.
my $rg_aid = AmiGO::Aid::ReferenceGenome->new();


##
sub setup {

  my $self = shift;

  $self->{STATELESS} = 0;

  ## Configure how the session stuff is going to be handled when and
  ## if it is necessary.
  my $sess_dir = $self->{CORE}->amigo_env('AMIGO_SESSIONS_ROOT_DIR');
  $self->session_config(
			CGI_SESSION_OPTIONS =>
			[
			 "driver:File",
			 $self->query,
			 {
			  Directory => $sess_dir,
			 }
			],
			COOKIE_PARAMS => {-path  => '/',},
			SEND_COOKIE => 1,
 );

  $self->tt_config(TEMPLATE_OPTIONS =>
		   {INCLUDE_PATH =>
		    $self->{CORE}->amigo_env('GO_ROOT') .
		    '/amigo/amigo/templates'});

  $self->mode_param('mode');
  $self->start_mode('software_list');
  $self->error_mode('mode_fatal');
  $self->run_modes(
		   'visualize'           => 'mode_visualize',
		   'software_list'       => 'mode_software_list',
		   'homolset_summary'    => 'mode_homolset_summary',
		   'homolset_graph'      => 'mode_homolset_graph',
		   'homolset_annotation' => 'mode_homolset_annotation',
		   'live_search'         => 'mode_live_search',
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

  ## Where would the ancient demos page hide...?
  my $foo = $self->{CORE}->amigo_env('AMIGO_CGI_PARTIAL_URL');
  $self->set_template_parameter('OLD_LOC', $foo);

  $self->add_template_content('html/main/software_list.tmpl');
  return $self->generate_template_page();
}


## TODO: Maybe the caching stuff should be pushed out to AmiGO.pm?
sub mode_homolset_summary {

  my $self = shift;

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



1;
