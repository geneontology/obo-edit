package AmiGO::WebApp::Services;
use base 'AmiGO::WebApp';

####
#### A place for "stateless" AmiGO stuff. Services, etc.
####

use strict;

use GOBO::DBIC::GODBModel::Graph;
use AmiGO::WebApp::Input;
use AmiGO::JavaScript;
use AmiGO::Aid;

use AmiGO::JSON;
use Lucene;
use AmiGO::Lucene;
use AmiGO::Worker::LiveSearch::Term;
use AmiGO::Worker::LiveSearch::GeneProduct;
use AmiGO::Worker::LiveSearch::Association;

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
  # 		    $self->{CORE}->amigo_env('GO_DEV_ROOT') .
  # 		    '/amigo/amigo/templates'});

  $self->mode_param('mode');
  $self->start_mode('status');
  $self->error_mode('mode_js_fatal');
  #$self->tt_include_path('templates/html/inc');
  $self->run_modes(
		   'status'       => 'mode_js_status', # server alive?
		   'live_search_term'    => 'mode_live_search_term',
		   'live_search_gene_product' =>'mode_live_search_gene_product',
		   #'live_search_association' =>'mode_live_search_association',
		   'AUTOLOAD'     => 'mode_js_exception'
		  );
}


## Return a JSON representation of search results; fast and on the
## fly. Should be similar to the separate completion code. Might need
## to be spun out for speed under mod_perl in the end...
sub mode_live_search_term {

  my $self = shift;
  my $json_resp = AmiGO::JSON->new('live_search_term');

  ##
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('live_search_term');
  my $query = $params->{query};
  my $index = $params->{index} + 0; # coerce to int?
  my $count = $params->{count} + 0; # coerce to int?
  my $packet = $params->{packet} + 0; # coerce to int?
  my $ontology = $params->{ontology};
  $self->{CORE}->kvetch("query: ". $query);
  $self->{CORE}->kvetch("index: ". $index);
  $self->{CORE}->kvetch("count: ". $count);
  $self->{CORE}->kvetch("packet: ". $packet);
  $self->{CORE}->kvetch("ontology: ". $ontology);

  ## Irritating FormValidator can return scalar or array
  ## ref. Normalize on array ref.
  my $required_ontology = $self->to_array_ref($ontology);

  ## Get our results...
  my $search = AmiGO::Worker::LiveSearch::Term->new({});
  my $results = $search->query($query, $index, $count,
			       $required_ontology);

  ## We will only be doing JSON out.
  $self->header_add( -type => 'application/json' );
  $json_resp->set_arguments($self->raw_params());
  $json_resp->set_results($results);
  return $json_resp->make_js();
}


## Return a JSON representation of search results; fast and on the
## fly. Should be similar to the separate completion code. Might need
## to be spun out for speed under mod_perl in the end...
sub mode_live_search_gene_product {

  my $self = shift;
  my $json_resp = AmiGO::JSON->new('live_search_gene_product');

  ##
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('live_search_gene_product');
  my $query = $params->{query};
  my $index = $params->{index} + 0; # coerce to int?
  my $count = $params->{count} + 0; # coerce to int?
  my $packet = $params->{packet} + 0; # coerce to int?
  my $species = $params->{species};
  my $scientific = $params->{scientific};
  my $source = $params->{source};
  my $gptype = $params->{gptype};
  my $homolset = $params->{homolset};
  $self->{CORE}->kvetch("query: ". $query);
  $self->{CORE}->kvetch("index: ". $index);
  $self->{CORE}->kvetch("count: ". $count);
  $self->{CORE}->kvetch("packet: ". $packet);
  $self->{CORE}->kvetch("species: ". $species);
  $self->{CORE}->kvetch("scientific: ". $scientific);
  $self->{CORE}->kvetch("source: ". $source);
  $self->{CORE}->kvetch("gptype: ". $gptype);
  $self->{CORE}->kvetch("homolset: ". $homolset);

  ## Irritating FormValidator can return scalar or array
  ## ref. Normalize on array ref.
  my $required_species = $self->to_array_ref($species);
  my $required_source = $self->to_array_ref($source);
  my $required_gptype = $self->to_array_ref($gptype);
  my $required_homolset = $self->to_array_ref($homolset);

  ## Get our results...
  my $search = AmiGO::Worker::LiveSearch::GeneProduct->new({});
  my $results = $search->query($query, $index, $count,
			       $required_species,
			       $required_source,
			       $required_gptype,
			       $required_homolset);

  ## We will only be doing JSON out.
  $self->header_add( -type => 'application/json' );
  $json_resp->set_arguments($self->raw_params());
  $json_resp->set_results($results);
  return $json_resp->make_js();
}


## Return a JSON representation of search results; fast and on the
## fly. Should be similar to the separate completion code. Might need
## to be spun out for speed under mod_perl in the end...
sub mode_live_search_association {

  my $self = shift;
  my $json_resp = AmiGO::JSON->new('live_search_association');

  ##
  my $i = AmiGO::WebApp::Input->new();
  my $params = $i->input_profile('live_search_association');

  ## Standard and bookkeeping.
  my $query = $params->{query};
  my $index = $params->{index} + 0; # coerce to int?
  my $count = $params->{count} + 0; # coerce to int?
  my $packet = $params->{packet} + 0; # coerce to int?

  ## GP-specific.
  my $species = $params->{species};
  my $scientific = $params->{scientific};
  my $source = $params->{source};
  my $gptype = $params->{gptype};

  ## Term-specific.
  my $ontology = $params->{ontology};

  ## Assoc-specific.
  my $evidence = $params->{evidence};

  $self->{CORE}->kvetch("query: ". $query);
  $self->{CORE}->kvetch("index: ". $index);
  $self->{CORE}->kvetch("count: ". $count);
  $self->{CORE}->kvetch("packet: ". $packet);
  $self->{CORE}->kvetch("ontology: ". $ontology);
  $self->{CORE}->kvetch("species: ". $species);
  $self->{CORE}->kvetch("scientific: ". $scientific);
  $self->{CORE}->kvetch("source: ". $source);
  $self->{CORE}->kvetch("gptype: ". $gptype);
  $self->{CORE}->kvetch("evidence: ". $evidence);

  ## Irritating FormValidator can return scalar or array
  ## ref. Normalize on array ref.
  my $required_ontology = $self->to_array_ref($ontology);
  my $required_species = $self->to_array_ref($species);
  my $required_source = $self->to_array_ref($source);
  my $required_gptype = $self->to_array_ref($gptype);
  my $required_evidence = $self->to_array_ref($evidence);

  ## Get our results...
  my $search = AmiGO::Worker::LiveSearch::Association->new({});
  my $results = $search->query($query, $index, $count,
  			       $required_ontology,
  			       $required_species,
  			       $required_source,
  			       $required_gptype,
			       $required_evidence);

  ## We will only be doing JSON out.
  $self->header_add( -type => 'application/json' );
  $json_resp->set_arguments($self->raw_params());
  $json_resp->set_results($results);
  return $json_resp->make_js();
}



1;
