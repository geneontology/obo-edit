package AmiGO::WebApp::ServicesExp;
use base 'AmiGO::WebApp';

####
#### Very tentative step towards a place for "stateless" AmiGO stuff.
####
#### NOTE: the opensearch and autocomplete stuff has been largely
#### moved to the atomic completion script (for mod_perl) in
#### experimental. Right now, only the "verify list of terms" function
#### remains here.
####

use strict;

## Take SuGR from a test drive.
# use SuGR::Render;
# use SuGR::Partition;
# use SuGR::BaryMatrix;
# use SuGR::Sugiyama;

use Digest::MD5;

#use Data::Dumper;
#use GOBO::DBIC::GODBModel::Schema;
#use AmiGO;
#my $core = AmiGO->new();
#my $core = AmiGO::Aid::ReferenceGenome->new();
use GOBO::DBIC::GODBModel::Graph;
use AmiGO::WebApp::Input;
use AmiGO::JavaScript;
use AmiGO::Aid;
use AmiGO::ReferenceGenome;

use AmiGO::GraphViz::GVMetaData;

use AmiGO::JSON;
use Lucene;
use AmiGO::Lucene;
use AmiGO::Worker::LiveSearch::Term;
use AmiGO::Worker::LiveSearch::GeneProduct;
use AmiGO::Worker::LiveSearch::Association;

use AmiGO::Worker::GONavi;
use AmiGO::Cache::GONavi;

use AmiGO::Cache::ART;

my $aid = AmiGO::Aid->new();


##
sub setup {

  my $self = shift;

  $self->{STATELESS} = 1;

  ## Configure how the session stuff is going to be handled when and
  ## if it is necessary.
#   $self->session_config(
# 			#CGI_SESSION_OPTIONS => [],
# 			#COOKIE_PARAMS       => {},
# 			#SEND_COOKIE         => 0,
# 		       );

#   $self->tt_config(TEMPLATE_OPTIONS =>
# 		   {INCLUDE_PATH =>
# 		    $self->{CORE}->amigo_env('GO_ROOT') .
# 		    '/amigo/amigo/templates'});

  $self->mode_param('mode');
  $self->start_mode('status');
  $self->error_mode('mode_js_fatal');
  #$self->tt_include_path('templates/html/inc');
  $self->run_modes(
		   'status'       => 'mode_js_status', # server alive?
		   'ontology'     => 'mode_ontology', # basic ontology info
		   'term'         => 'mode_term', # basic term info
		   'gene_product' => 'mode_gene_product', # basic gp info
		   'verify'       => 'mode_verify', # batch verify term accs
		   'navi_js_data' => 'mode_navi_js_data',
		   'id_request'   => 'mode_id_request', # a new GO id
		   'AUTOLOAD'     => 'mode_js_exception'
		  );
}


## Another try using GV. Note that we're returning an actual JS file
## and not JSON in this case, so we're going to ignore our usual
## conventions for AmiGO ajax.
sub mode_navi_js_data {

  my $self = shift;

  ## Deal with input.
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('gander');
  my $lon = $params->{lon};
  my $lat = $params->{lat};
  my $focus = $params->{focus};
  my $zoom = $params->{zoom};
  my $input_term_list = $self->{CORE}->clean_term_list($params->{terms});

  ###
  ### GONavi cache handler.
  ###
  ### TODO: this cache handler does inputs rather than leaves. This
  ### reduces cache calculation time, but increases the number of
  ### misses. Should benchmark a comparison of the two systems.
  ###

  ## Generate input key.
  my @sorted_input = sort { lc($a) cmp lc($b) } @$input_term_list;
  my $input_mash = '+' . join('+', @sorted_input);
  my $ctx = Digest::MD5->new();
  $ctx->add($input_mash);
  my $input_key = $ctx->hexdigest();
  $self->{CORE}->kvetch("GONavi graph input mash: " . $input_mash);
  $self->{CORE}->kvetch("GONavi graph input md5: " . $input_key);

  ###
  ### Data delivery.
  ###

  ## These are the three things that we want delivered from the cache.
  my $retstruct = {};
  my $png_file = '';
  my $mini_png_file = '';

  ## Check key against cache.
  my $cache_manager = AmiGO::Cache::GONavi->new();
  my $cache = $cache_manager->get_data($input_key);
  if( defined($cache) ){

    $self->{CORE}->kvetch("GONavi: found cached data (key): " . $cache->{key});

    $retstruct = $self->{CORE}->parse_json_data($cache->{meta_data});
    $png_file = $cache->{image};
    $mini_png_file = $cache->{mini_image};

  }else{

    $self->{CORE}->kvetch("GONavi: no cached data found, generating new data.");

    ##
    my $gnd = AmiGO::Worker::GONavi->new();
    my $data = $gnd->generate($input_term_list);

    ## Get the generated data.
    $retstruct = $data->{meta_data};
    $png_file = $data->{image};
    $mini_png_file = $data->{mini_image};

    ## Store a copy of it for later.
    $cache_manager->cache_data({
				key => $input_key,
				meta_data => $self->{CORE}->make_js($retstruct),
				image => $png_file,
				mini_image => $mini_png_file,
			       });
  }

  ###
  ### Get the needed images out onto the filesystem.
  ###

  ## Figure out the unique identifier for this map by the leaves. Sort
  ## and cat.
  #my $uid = join('_', sort(keys(%$leaves)));
  ## Or not...
  my $ug = Data::UUID->new();
  my $uid = $self->{CORE}->unique_id();

  ## Write image data as file on disk.
  my $graph_map_name = '/graph_map.' . $uid . '.png';
  my $gfname = $self->{CORE}->amigo_env('AMIGO_TEMP_IMAGE_DIR') . $graph_map_name;
  open(GFFILE, ">$gfname") or die $!;
  print GFFILE $png_file;
  close(GFFILE);
  $self->{CORE}->kvetch("Wrote PNG data to: $gfname");

  ## Write mini image data as file on disk.
  my $graph_map_mini_name = '/graph_map.' . $uid . '.mini.png';
  my $gfname_mini =
    $self->{CORE}->amigo_env('AMIGO_TEMP_IMAGE_DIR') . $graph_map_mini_name;
  open(GFFILEM, ">$gfname_mini") or die $!;
  print GFFILEM $mini_png_file;
  close(GFFILEM);
  $self->{CORE}->kvetch("Wrote mini PNG data to: $gfname_mini");

  ###
  ### Add additional information to the meta-data structure.
  ###

  ## Location of image files on the server.
  $retstruct->{map_url} =
    $self->{CORE}->amigo_env('AMIGO_TEMP_IMAGE_URL') . $graph_map_name;
  $retstruct->{map_mini_url} =
    $self->{CORE}->amigo_env('AMIGO_TEMP_IMAGE_URL') . $graph_map_mini_name;

  ## Callback URLs.
  $retstruct->{go_term_service} =
    $self->{CORE}->amigo_env('AMIGO_CGI_URL') . '/aserve_exp?mode=term&terms=';
  $retstruct->{amigo_term_details_service} =
    $self->{CORE}->amigo_env('AMIGO_CGI_URL') . '/term-details.cgi?term=';

  ## The previous position if it is available from the incoming
  ## information.
  my $position = {};
  #   $self->{CORE}->kvetch('_lon_' . $lon);
  #   $self->{CORE}->kvetch('_lat_' . $lat);
  #   $self->{CORE}->kvetch('_lat_' . $focus);
  #   $self->{CORE}->kvetch('_lat_' . $zoom);
  if( defined $lon &&
      defined $lat &&
      defined $focus &&
      defined $zoom ){
    $position = {
		 lon => $lon,
		 lat => $lat,
		 focus => $focus,
		 zoom => $zoom,
		};
  }
  $retstruct->{map_previous_position} = $position;

  #$self->header_add( -type => 'text/plain' );
  $self->header_add( -type => 'application/javascript' );
  return 'GONavi.Data = ' . $self->{CORE}->make_js($retstruct) . ';';
}


## NOTE: association count temporarily disabled for plugin fun.
## TODO: Ignore roots for calculation...
sub mode_term {

  my $self = shift;

  ## Snag inputs: term (single) and full (boolean).
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('term_info');
  my $input_term_list = $self->{CORE}->clean_term_list($params->{term});
  my $use_full_p = 0;
  $use_full_p = 1 if $params->{full} eq 'true';

  ##
  my $json_resp = AmiGO::JSON->new('term');
  my $retstruct = {};
  my $graph = GOBO::DBIC::GODBModel::Graph->new();
  # $graph->verbose(1);
  foreach my $input_term_id (@$input_term_list){

    $self->{CORE}->kvetch('___term_id: ' . $input_term_id);

    if( defined $input_term_id && $input_term_id ne '' ){

      $retstruct->{$input_term_id} = {};

      my $term = $graph->get_term($input_term_id);

      if( defined $term ){

	## Get term info: acc, name, synonyms, etc.
	my $acc = $term->acc;
	my $name = $term->name;
	my $ttype = $term->term_type;
	$retstruct->{$input_term_id}{acc} = $acc;
	$retstruct->{$input_term_id}{name} = $name;
	#$retstruct->{$input_term_id}{term_type} = $ttype;
	$retstruct->{$input_term_id}{term_type} = $aid->readable($ttype);

	## Get basic neighborhood info--children and parents.
	$retstruct->{$input_term_id}{children} = [];
	my $children = $graph->get_children($acc);
	#$self->{CORE}->kvetch("\tnum kids: " . scalar(@$children));
	foreach my $kid (@$children){
	  push @{$retstruct->{$input_term_id}{children}},
	    {acc => $kid->acc, name => $kid->name};
	  #$self->{CORE}->kvetch("\tkid name: " . $kid->name);
	}
	my @foo = sort {
	  return $a->{name} cmp $b->{name};
	} @{$retstruct->{$input_term_id}{children}};
	$retstruct->{$input_term_id}{children} = \@foo;

	###
	### Get basic association information--total direct and indirect.
	###

	## Indirect and direct counting if we want full information.
	my $icount_total = 0;
	my $dcount_total = 0;
	if( $use_full_p && ! $graph->is_root_p($term->acc) ){
	  foreach my $c ( $term->gene_product_count->all() ){
	    $icount_total += $c->product_count;
	  }
	  $dcount_total = scalar($term->association->all());
	}
	$retstruct->{$input_term_id}{indirect_count} = $icount_total;
	$retstruct->{$input_term_id}{direct_count} = $dcount_total;
      }else{
	## Doesn't look termish...
	$json_resp->add_warning($input_term_id . " could not be resolved");
      }
    }
  }

  ##
  $self->header_add( -type => 'application/json' );
  $json_resp->set_results($retstruct);
  return $json_resp->make_js();
}


## Dumb for now--just return roots and any other useful meta-info.
sub mode_ontology {

  my $self = shift;

  ## Snag inputs: term (single) and full (boolean).
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile();

  ##
  my $json_resp = AmiGO::JSON->new('ontology');
  my $rootstruct = {};
  my $graph = GOBO::DBIC::GODBModel::Graph->new();
  my $root_node_hash = $graph->get_roots();
  foreach my $term_id (keys %$root_node_hash){

    $self->{CORE}->kvetch('___term_id: ' . $term_id);
    my $term = $root_node_hash->{$term_id};

    ## Get term info: acc, name, synonyms, etc.
    my $acc = $term->acc;
    my $name = $term->name;
    my $ttype = $term->term_type;
    $rootstruct->{$term_id}{acc} = $acc;
    $rootstruct->{$term_id}{name} = $name;
    $rootstruct->{$term_id}{term_type} = $aid->readable($ttype);
  }

  ## Errors?
  

  ##
  $self->header_add( -type => 'application/json' );
  $json_resp->set_results({roots => $rootstruct});
  return $json_resp->make_js();
}


##
sub mode_gene_product {

  my $self = shift;

  ## Snag inputs: term (single) and full (boolean).
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('gene_product_info');
  my $input_gp_list = $self->{CORE}->clean_gene_product_list($params->{gene_product});
  my $use_full_p = 0;
  $use_full_p = 1 if $params->{full} eq 'true';

  $self->{CORE}->kvetch('_param_gp_: ' . $params->{gene_product});
  $self->{CORE}->kvetch('gp_id_count: ' . scalar(@$input_gp_list));

  my $rg = AmiGO::ReferenceGenome->new();

  ##
  my $json_resp = AmiGO::JSON->new('gene_product');
  my $retstruct = {};
  foreach my $input_gp_id (@$input_gp_list){

    $self->{CORE}->kvetch('gp_id: ' . $input_gp_id);

    if( defined $input_gp_id && $input_gp_id ne '' ){

      ## Split on first colon (that's correct, right?).
      my($dbname, $key) = $self->{CORE}->split_gene_product_acc($input_gp_id);
      $self->{CORE}->kvetch('gp_split: ' . $dbname . ' : ' . $key);

      my $q = GOBO::DBIC::GODBModel::Query->new({type=>'gene_product'});
      my $all_gps =
	$q->get_all_results({-and=>[
				    {'dbxref.xref_dbname' => $dbname},
				    {'dbxref.xref_key' => $key}
				   ]});

      if( $all_gps && @$all_gps ){
	if( scalar(@$all_gps) == 0 ){
	  $json_resp->add_warning($input_gp_id . " was missing");
	}elsif( scalar(@$all_gps) > 1 ){
	  $json_resp->add_warning($input_gp_id . " was ambiguous");
	}else{

	  ##
	  my $gp = $$all_gps[0];
	  #$self->{CORE}->kvetch('___gp_obj: ' . $gp);

	  ## Get core gp info: acc, name, synonyms, etc.
	  my $acc = $input_gp_id;
	  my $full_name = $gp->full_name();
	  my $symbol = $gp->symbol();

	  ## See if we can dig out and RG info.
	  my $rg_res_final = undef;
	  my $rg_res_try = $rg->find_refgen_info({gene_product=>$acc});
	  if( defined $rg_res_try ){
	    $rg_res_final = $rg_res_try;
	  }

	  ## Pack into hash.
	  $retstruct->{$input_gp_id} =
	    {
	     acc => $acc,
	     full_name => $full_name,
	     symbol => $symbol,
	     reference_genome => $rg_res_final,
	    };
	}
      }else{
	$self->{CORE}->kvetch('_failed_to_find_anything_');
	$json_resp->add_warning($input_gp_id . " could not be found");
      }
    }
  }

  ##
  $self->header_add( -type => 'application/json' );
  $json_resp->set_results($retstruct);
  return $json_resp->make_js();
}


## Verify that the incoming ids are legit--just return the legit ones.
## TODO/BUG: this is just for terms right now...
sub mode_verify {

  my $self = shift;

  ##
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('term_info');
  my $input_term_list = $self->{CORE}->clean_term_list($params->{term});

  my $json_resp = AmiGO::JSON->new('verify');

  ###
  ### Ready lucene.
  ###

  ## NOTE: mod_perl leftover with the "ours".
  our $regexp = $ENV{AMIGO_TERM_REGEXP} if ! $regexp;
  our $search_dir = $ENV{AMIGO_INDEX_DIR} if ! $search_dir;
  my $lucy = AmiGO::Lucene->new();

  ## TODO/BUG: hard-wired on term.
  my $type = "term";
  my $spot = $search_dir . '/lucene/general';
  if( $type eq 'term' ){
    $spot = $search_dir . '/lucene/term';
  }elsif( $type eq 'gene_product' ){
    $spot = $search_dir . '/lucene/gene_product';
  }
  my $analyzer = new Lucene::Analysis::Standard::StandardAnalyzer();
  my $store = Lucene::Store::FSDirectory->getDirectory($spot, 0);
  my $searcher = new Lucene::Search::IndexSearcher($store);
  my $parser = new Lucene::MultiFieldQueryParser(['acc'], $analyzer);

  ###
  ### Check loop.
  ###

  ##
  my $retstruct = {};
  foreach my $input_term_id (@$input_term_list){

    $self->{CORE}->kvetch('___term_id: ' . $input_term_id);

    if( defined $input_term_id && $input_term_id ne '' ){

      ## Make sure that things are escaped.
      my $fixed_term_id = $lucy->fix_query($input_term_id);

      ## Get results then number of results.
      my $query = $parser->parse($fixed_term_id);
      my $hits = $searcher->search($query);
      my $num_hits = $hits->length();

      ## If our hit is unique and keys to the input id, then we have a
      ## winner.
      if( $num_hits == 0 ){
	#$json_resp->add_warning($input_term_id . " was missing");
      }elsif( $num_hits > 1 ){
	#$json_resp->add_warning($input_term_id . " was ambiguous");
      }else{
	my $doc = $hits->doc(0);
	my $acc = $doc->get('acc');
	if( $input_term_id eq $acc ){
	  $retstruct->{$acc} = 1;
	}
      }
    }
  }

  ##
  $json_resp->set_results($retstruct);
  $self->header_add( -type => 'application/json' );
  return $json_resp->make_js();
}


##
sub mode_id_request {

  my $self = shift;

  ##
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('id_request');
  my $req_data = $params->{data} || '';

  # ## Random generation.
  # my $new_id_num = 9000000 +
  #   int($self->{CORE}->randomness(6, ['1', '2', '3', '4', '5',
  # 			      '6', '7', '8', '9', '0']));

  ## ART generation.
  my $art = AmiGO::Cache::ART->new();
  my $new_id_num = $art->add_request({data => $req_data});

  ## TODO: mail someone.

  ## Output id.
  my $json_resp = AmiGO::JSON->new('id_request');
  $json_resp->set_results({new_id => 'GO:' . $new_id_num});
  $self->header_add( -type => 'application/json' );
  return $json_resp->make_js();
}



1;
