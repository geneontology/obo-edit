////
//// A full take on a production live search for GOlr--try and make it
//// work directly off of the server for giggles/testing.
////

// Make sure there is no problem.
// TODO/BUG: Should be right after jQuery load.
//jQuery.noConflict();

// Bring in the AmiGO core and keep a coder handy.
// TODO/BUG: switch DEBUG to false for release.
org.bbop.amigo.DEBUG = true;
var core = new org.bbop.amigo.core();
var gm = new org.bbop.amigo.go_meta();
// var coder = new core.util.coder();
var last_sent_packet = 0;
var last_received_packet = 0;

// Delay before taking action when typing.
var delay_in_ms = 350;

// Our separate widget and notice object.
var widgets = null;

// Our discrete universal widgets.
var type_model = null;
var type_widget = null;
var evidence_model = null;
var evidence_widget = null;
var source_model = null;
var source_widget = null;
var taxon_model = null;
var taxon_widget = null;
var ip_lc_model = null;
var ip_lc_widget = null;
var document_category_model = null;
var document_category_widget = null;
//var aecl_model = null;
//var aecl_widget = null;
var aecl_closure_model = null;
var aecl_closure_widget = null;

// Find newlines in text.
var newline_finder = new RegExp("\n", "g");

// Server detection watchdog.
// Set a timeout to see if we can find the solr server in a reasonable
// amount of time. If we can't, display an error message.
// TODO: Ideally, this should be done with the error function slot on the
// jQuery ajax or getJSON functions, but after fiddling around, upgraging to 
// 1.5.1, and then 1.6.1, I was never able to get a satisfactory solution for
// all cases. See annoying JSONP behavior at:
//  http://api.jquery.com/jQuery.ajax/
//  http://api.jquery.com/jQuery.getJSON/
//  http://bugs.jquery.com/ticket/1863
//  http://bugs.jquery.com/ticket/3442
// I get the feeling that the complete fix was never added, but I want
// to move on.
// There was also the option of plugins and writing something myself, but 
// this is sufficient for now and can act as a placeholder until a better
// solution is worked out.
var watchdog_solr_is_responding = false;
function _server_response_warning(){
    if( watchdog_solr_is_responding == false ){
	core.kvetch("ERROR: can't seem to find the solr server...");
	//jQuery	
    }
}
// 3 second timeout to find the server.
window.setTimeout(_server_response_warning, 3000);


// TODO/BUG: make work, split out
// Thinking about lessons learned from solr ajax.
// Updatable model that connects to the Solr server.
// Makes no attempt to join to a form--entirely held as an internal model.
// {url: 'http://theplace', facets: ['foo', 'bar']}

// This should act as a model--since we start with a completely open
// query (whether we display it or not), we will have all possible
// facets and can build the initial model off of that.
function SolrManager(in_args){

    // TODO: Block requests from the past from haunting us.
    this.last_sent_packet = 0;
    this.last_received_packet = 0;

    // TODO:
    this.register = function(fun_id, in_function){
    };
    // 
    this.vanish = function(fun_id){
    };
    this.reveal = function(fun_id){
    };
    // TODO?
    this.add_facet = function(){	
    };
    this.remove_facet = function(){	
    };

    // Check args.
    if( ! in_args ){
	core.kvetch('SM: ERROR: no argument');
    }
    // There should be a string url argument.
    if( in_args && ! in_args['url'] ){
	core.kvetch('SM: ERROR: no url argument');
    }
    if( in_args && in_args['url'] && typeof in_args['url'] != 'string' ){
	core.kvetch('SM: ERROR: no url string argument');
    }
    // There should be an array facets argument.
    if( in_args && ! in_args['facets'] ){
	core.kvetch('SM: ERROR: no facets argument');
    }
    if( in_args && in_args['facets'] &&
	( typeof in_args['facets'] != 'object' || 
	  typeof in_args['facets'].length == 'undefined' ||
	  typeof in_args['facets'].length == 0 )){
	      core.kvetch('SM: ERROR: no facets sanely specified');
	  }
    
    // Our default target url.
    this.solr_url = in_args['url'];
    
    // Our default query args, with facet fields plugged in.
    this.query_args =
	{
	    // TODO/BUG? need jsonp things here?
	    qt: 'standard',
	    indent: 'on',
	    wt: 'json',
	    version: '2.2',
	    rows: 10,
	    //start: 1,
	    start: 0, // Solr is offset indexing
	    fl: '*%2Cscore',
	    
	    // Control of facets.
	    facet: 'true',
	    'facet.mincount': 1,
	    // TODO?: 'facet.limit': 20,
	    // TODO?: 'f.???.facet.limit': 50,
	    'facet.field': in_args['facets'],
	    // TODO: 'json.nl': [flat|map|arrarr]
	    'json.nl': 'arrarr',

	    // Facet filtering.
	    // TODO: This needs to be left to a different part of the
	    // model.
	    //fq: [],
	    // Query-type stuff.
	    //q: '*:*', // start by going after everything
	    
	    // Our bookkeeping.
	    packet: 0
	};
    //var final_query_args = _merge(default_query_args, in_args);
		
    // // Ready possible filterable facets.
    // filter_state: a combination of q and fq to produce a result
    // results: 
    // // This should include, set_type: intersection/union
    // this.facet_filters = {};
    // for( var in_facets in in_args['facets'] ){
    // 	this.facet_filters[in_facets] = {};
    // }
    // //var final_filter_args = _merge(default_filter_args, in_args);

    // ...
    this._make_always_needed_section = function(){

	//var resrc = core.api.live_search.golr(all_inputs);
	//var url = gm.golr_base() + '/' + resrc;

	var qbuff = [];	
	var qargs = core.util.get_hash_keys(this.query_args);
	for( var qname_i in qargs ){
	    var qname = qargs[qname_i];
	    var qval = this.query_args[qname];
	    //core.kvetch('SM: qname:' + qname);
	    //core.kvetch('SM: qval:' + qval);

	    if( typeof qval == 'string' ||
		typeof qval == 'number' ){
		    // Is standard name/value pair.
		    var nano_buff = [];
		    nano_buff.push(qname);
		    nano_buff.push('=');
		    nano_buff.push(qval);
		    qbuff.push(nano_buff.join(''));
		}else if( typeof qval == 'object' ){
		    if( typeof qval.length != 'undefined' ){
			// Is array (probably).
			// Iterate through and double on.
			for(var qval_i = 0; qval_i < qval.length ; qval_i++){
			    var nano_buff = [];
			    nano_buff.push(qname);
			    nano_buff.push('=');
			    nano_buff.push(qval[qval_i]);
			    qbuff.push(nano_buff.join(''));
			}
		    }else{
			core.kvetch('SM: ERROR: no hash possible');
			// // Is hash.
			// // Use the a parser to change into
			// // arbitrary sql-like request.
			// core.kvetch('SM: ERROR: hash not done yet');
			// // TODO: The "and" case is pretty much like
			// // the array, the "or" case needs to be
			// // handled carfeully. In both cases, care will
			// // be needed to show which filters are marked.
		    }
		}else{
		    core.kvetch('SM: make link unknown type!');
		}
	}

	return qbuff.join('&');
    };
    
    // The main callback function called after a successful AJAX call
    // in the update function.
    this._rerender = function(json_data){
	core.kvetch('SM: in rerender...');
	
	// // Grab meta information.
	// var total = core.golr_response.total_documents(json_data);
	// var first = core.golr_response.start_document(json_data);
	// var last = core.golr_response.end_document(json_data);
	// var meta_cache = new Array();
	// meta_cache.push('Total: ' + total);
    };
    var _reren = this._rerender;

    // ...
    this.update = function(in_arg){
	
	// TODO?
	// Increment packet.
	// this.query_args['packet'] = this.query_args['packet'] + 1;
	
	// Condintional join of all the parts.
	var qshead = this.solr_url + 'select?';
	var always_needed = this._make_always_needed_section();
	var qurl = qshead + always_needed;
	if( in_arg && in_arg == 'reset' ){
	    // Reset and do completely open query.
	    qurl = qurl + '&q=*:*';
	}else{
	    // TODO: standard assemble with filter and state.
	}
	    
	//qurl = 'http://accordion.lbl.gov:8080/solr/select?qt=standard&indent=on&wt=json&version=2.2&rows=10&start=0&fl=*%2Cscore&facet=true&facet.mincount=1&facet.field=document_category&facet.field=type&facet.field=evidence_type&facet.field=source&facet.field=taxon&facet.field=isa_partof_label_closure&facet.field=annotation_extension_class_label&facet.field=annotation_extension_class_label_closure&q=*:*&packet=1';

	core.kvetch('SM: try: ' + qurl);
	//widgets.start_wait('Updating...');

	// TODO: 
		
	// TODO/BUG: JSONP for solr looks like?
	var argvars = {
	    type: "GET",
	    url: qurl,
	    dataType: 'json',
	    jsonp: 'json.wrf',
	    success: _reren,
	    error: function (result, status, error) {
		
	    	core.kvetch('SM: Failed server request: ' +
			    result + ', ' +
			    status + ', ' +
			    error);
		
		// // Get the error out if possible.
		// var jreq = result.responseText;
		// var req = jQuery.parseJSON(jreq);
		// if( req && req['errors'] &&
		//     req['errors'].length > 0 ){
		// 	var in_error = req['errors'][0];
		// 	core.kvetch('SM: ERROR:' + in_error);
					
		// 	// Split on newline if possible to get
		// 	// at the nice part before the perl
		// 	// error.
		// 	var reg = new RegExp("\n+", "g");
		// 	var clean_error_split =
		// 	    in_error.split(reg);
		// 	var clean_error = clean_error_split[0];
		// 	//widgets.error(clean_error);
		//     }
		
		// // Close wait no matter what.
		// //widgets.finish_wait();
	    }
	};
	//jQuery.ajax(argvars);
    };
}


// Get the layout done and request GO meta-info.
function LiveSearchGOlrInit(){

    core.kvetch('');
    core.kvetch('LiveSearchGOlrInit start.');

    ///
    /// Manager test.
    ///

    var sm = new SolrManager({url: 'http://accordion.lbl.gov:8080/solr/',
			      facets: ['document_category', 'type']});

    sm.update('reset');

    ///
    /// Try and get UI ready.
    ///

    //
    //core.kvetch('Apply tabs...');
    //jQuery("#search-tabs").tabs();
    //jQuery("#search-tabs").tabs('select', 0);

    widgets = new org.bbop.amigo.ui.widgets();

    core.kvetch('LiveSearchGOlr init completed.');

    // Pull in GO meta info.
    //var ontology_data = gm.ontologies();
    var source_data = gm.sources();
    var type_data = gm.gp_types();

    // TODO/BUG: Chris now seems to be including the "NCBIGene:"
    // on top of the unique number--for now, just go through and add that.
    var species_data = gm.species();
    var taxon_set = [];
    for( var si = 0; si < species_data.length; si++ ){
	var slabel = species_data[si][0];
	var skey = species_data[si][1];
	taxon_set.push([slabel, 'NCBIGene:' + skey]); 
    }


    // Fix incoming data.
    var evidence_data = gm.evidence_codes();
    var evcode_set = []; 
    for( var eci = 0; eci < evidence_data.length; eci++ ){
	var ekey = evidence_data[eci];
	evcode_set.push([ekey, ekey]); 
    }

    ///
    /// Create forms and controls.
    ///

    // The hidden count for all forms.
    var hidden_count_text = widgets.form.hidden_input('count', '10');
    var hidden_facet_text = widgets.form.hidden_input('facet', 'true');
    var hidden_facet_field_type_text =
	widgets.form.hidden_input('facet.field', 'type');
    var hidden_facet_field_ev_type_text =
	widgets.form.hidden_input('facet.field', 'evidence_type');
    var hidden_facet_field_source_text =
	widgets.form.hidden_input('facet.field', 'source');
    var hidden_facet_field_taxon_text =
	widgets.form.hidden_input('facet.field', 'taxon');
    var hidden_facet_field_term_label_closure_text =
	widgets.form.hidden_input('facet.field', 'isa_partof_label_closure');
    var hidden_facet_field_document_category_text =
	widgets.form.hidden_input('facet.field', 'document_category');
    // var hidden_facet_field_aecl_text =
    // 	widgets.form.hidden_input('facet.field',
    // 				  'annotation_extension_class_label');
    var hidden_facet_field_aecl_closure_text =
	widgets.form.hidden_input('facet.field',
				  'annotation_extension_class_label_closure');
    
    // Clear the controls' area.
    _clear_app_forms();

    // Create the new form for a GOlr search.
    // var hidden_document_category = // TODO: make dynamic later
    // 	widgets.form.hidden_input('document_category', 'annotation');
    // var hidden_mode_search_text =
    // 	widgets.form.hidden_input('mode', 'live_search_association_golr');
    var query_text =
    	widgets.form.text_input('q', 'q', 25, 
				'Search for<br />');
    // var ontology_text =
    // 	widgets.form.multiselect('ontology', 'ontology', 4,
    // 				 ontology_data, 'Ontology');

    // Get type filter going.
    type_model = new org.bbop.amigo.ui.interactive.multi_model(type_data);
    type_widget =
	new org.bbop.amigo.ui.interactive.multi_widget('type', 'type',
						       4, 'GP type');
    type_widget.update_with(type_model.get_state());
    var type_text = type_widget.render_initial();

    // Get source filter going.
    // var taxon_text =
    // 	widgets.form.multiselect('taxon', 'taxon', 4,
    // 				 species_data, 'Species');
    taxon_model = new org.bbop.amigo.ui.interactive.multi_model(taxon_set);
    taxon_widget =
	new org.bbop.amigo.ui.interactive.multi_widget('taxon', 'taxon',
						       4, 'Species');
    taxon_widget.update_with(taxon_model.get_state());
    var taxon_text = taxon_widget.render_initial();

    // Get source filter going.
    source_model = new org.bbop.amigo.ui.interactive.multi_model(source_data);
    source_widget =
	new org.bbop.amigo.ui.interactive.multi_widget('source', 'source',
						       4, 'Data source');
    source_widget.update_with(source_model.get_state());
    var source_text = source_widget.render_initial();

    // Get evidence filter going.
    evidence_model =
	new org.bbop.amigo.ui.interactive.multi_model(evcode_set);
    evidence_widget =
	new org.bbop.amigo.ui.interactive.multi_widget('evidence_type',
						       'evidence_type',
						       4, 'Evidence');
    evidence_widget.update_with(evidence_model.get_state());
    var evidence_type_text = evidence_widget.render_initial();

    // Get isa_partof_label_closure filter going.
    var ipl = 'isa_partof_label_closure';
    ip_lc_model =
	new org.bbop.amigo.ui.interactive.multi_model({});
    ip_lc_widget =
	new org.bbop.amigo.ui.interactive.multi_widget(ipl, ipl,
						       4, 'Term closure');
    ip_lc_widget.update_with(ip_lc_model.get_state());
    var isa_partof_label_closure_text = ip_lc_widget.render_initial();

    // Get document_category filter going.
    var dcid = 'document_category';
    document_category_model = new org.bbop.amigo.ui.interactive.multi_model({});
    document_category_widget =
	new org.bbop.amigo.ui.interactive.multi_widget(dcid, dcid,
						       3, 'Document type');
    document_category_widget.update_with(document_category_model.get_state());
    var document_category_text = document_category_widget.render_initial();

    // // Get annotation_extension_class_label filter going.
    // var aecl_id = 'annotation_extension_class_label';
    // aecl_model = new org.bbop.amigo.ui.interactive.multi_model({});
    // aecl_widget =
    // 	new org.bbop.amigo.ui.interactive.multi_widget(aecl_id, aecl_id, 4,
    // 						       'Annotation extension');
    // aecl_widget.update_with(aecl_model.get_state());
    // var aecl_text = aecl_widget.render_initial();

    // Get annotation_extension_class_label filter going.
    var aecl_closure_id = 'annotation_extension_class_label_closure';
    aecl_closure_model = new org.bbop.amigo.ui.interactive.multi_model({});
    aecl_closure_widget =
	new org.bbop.amigo.ui.interactive.multi_widget(aecl_closure_id,
						       aecl_closure_id, 4,
						       'Annotation extension closure');
    aecl_closure_widget.update_with(aecl_closure_model.get_state());
    var aecl_closure_text = aecl_closure_widget.render_initial();

    // Add in the order that we want things.
    //jQuery("#app-form").append(hidden_mode_search_text);
    jQuery("#app-form").append(hidden_count_text);
    jQuery("#app-form").append(hidden_facet_text);
    jQuery("#app-form").append(hidden_facet_field_document_category_text);
    jQuery("#app-form").append(hidden_facet_field_type_text);
    jQuery("#app-form").append(hidden_facet_field_ev_type_text);
    jQuery("#app-form").append(hidden_facet_field_source_text);
    jQuery("#app-form").append(hidden_facet_field_taxon_text);
    jQuery("#app-form").append(hidden_facet_field_term_label_closure_text);
    // jQuery("#app-form").append(hidden_facet_field_aecl_text);
    jQuery("#app-form").append(hidden_facet_field_aecl_closure_text);
    jQuery("#app-form-query").append(query_text);
    //jQuery("#app-form-filters").append(ontology_text);
    jQuery("#app-form-filters").append(document_category_text);
    jQuery("#app-form-filters").append(type_text);
    jQuery("#app-form-filters").append(taxon_text);
    jQuery("#app-form-filters").append(source_text);
    jQuery("#app-form-filters").append(evidence_type_text);
    jQuery("#app-form-filters").append(isa_partof_label_closure_text);
    // jQuery("#app-form-filters").append(aecl_text);
    jQuery("#app-form-filters").append(aecl_closure_text);

    //core.kvetch('GP type text: ' + type_text );

    function _generate_action_to_server(marshaller, do_results){
	return function(event){

	    // core.kvetch('EV: ' + event );
	    // core.kvetch('TP: ' + typeof(event) );
	    // core.kvetch('SP: ' + event.stopPropagation );
	    event.stopPropagation();

	    // core.kvetch('event1...' + event);
	    // core.kvetch('event3...' + event.keyCode);
	    // core.kvetch('event4...' + event.metaKey);
	    // core.kvetch('event5...' + event.ctrlKey);

	    var ignorable_event_p = false;

	    // Try and cut down on unnecessary hits by filtering out
	    // common (and often not very useful) characters during
	    // key events.
	    // BUG/TODO: check across browsers...
	    if( event ){
		var kc = event.keyCode;
		//core.kvetch('key event: ' + kc);
		if( kc ){
		    if( kc == 39 || // right
			kc == 37 || // left
			kc == 32 || // space
			kc == 20 || // ctl?
			kc == 17 || // ctl?
			kc == 16 || // shift
			//kc ==  8 || // delete // I want resets, and this goes
			kc ==  0 ){ // super
			    // // I want to allow zero-length resets, so
			    // // if the length is 0, let them through.
			    // if( all_inputs['q'] &&
			    // 	all_inputs['q'][0] &&
			    // 	all_inputs['q'][0].length == 0 ){
			    // 	    core.kvetch('non-ignorable 0 event');
			    // 	}else{				    
			    core.kvetch('ignorable key event: ' + kc);
			    ignorable_event_p = true;
			    // }
			}
		}
	    }
	    
	    //
	    if( ! ignorable_event_p ){

		// And...um... convert q to the correct query.
		var all_inputs = marshaller();
		//all_inputs['q'] = all_inputs[query_id];
		
		// // Cut down on overhead a little.
		// if( all_inputs &&
		//     all_inputs['q'] &&
		//     all_inputs['q'][0] &&
		//     all_inputs['q'][0].length >= 3 ){
			
		//core.kvetch('input q: ' + all_inputs['q'][0]);

		// Increment packet (async ordering).
		last_sent_packet++;
		all_inputs['packet'] = last_sent_packet;

		// BUG/TODO: a switch to dismax will eliminate
		// this, this is just here to bootstrap
		// debugging for now.
		// Also, for now, when no input is coming in, 
		if( all_inputs['q'] &&
		    all_inputs['q'][0] &&
		    all_inputs['q'][0].length >= 1 ){
			all_inputs['q'][0] =
			    'label:' + all_inputs['q'][0] +
			    ' OR annotation_class_label:' + all_inputs['q'][0];
		    }else{
			all_inputs['q'][0] = '*:*';
		    }

		var resrc = core.api.live_search.golr(all_inputs);
		var url = gm.golr_base() + '/' + resrc;

		core.kvetch('try: ' + url);		    
		widgets.start_wait('Updating...');
			
		// TODO/BUG: JSONP for solr looks like?
		var argvars = {
	    	    type: "GET",
	    	    url: url,
		    //data: myQueryParameters,
	    	    //dataType: 'json',
	    	    dataType: 'json',
		    jsonp: 'json.wrf',
	    	    success: do_results,
	    	    error: function (result, status, error) {
			
	    		core.kvetch('Failed server request ('+
				    status + '): ' + error);
			
			// Get the error out if possible.
			var jreq = result.responseText;
			var req = jQuery.parseJSON(jreq);
			if( req && req['errors'] &&
			    req['errors'].length > 0 ){
				var in_error = req['errors'][0];
				core.kvetch('ERROR:' + in_error);
				
				// Split on newline if possible to get
				// at the nice part before the perl
				// error.
				var reg = new RegExp("\n+", "g");
				var clean_error_split =
				    in_error.split(reg);
				var clean_error = clean_error_split[0];
				widgets.error(clean_error);
			    }
			
			// Close wait no matter what.
			widgets.finish_wait();
		    }
		};
		jQuery.ajax(argvars);
		// }else{
		// 	core.kvetch('Threshold not passed with: ' +
		// 		    all_inputs['q'][0]);
		// }
	    }
	};
    };
    
    // Create our callback function for this case.
    var marshal_form = 
    	widgets.form.create_jquery_marshal('#app-form',
					   ['input', 'option:selected']);
    
    var server_action =
	_generate_action_to_server(marshal_form, _process_results);
    
    // Attach listeners to the form.
    jQuery("#q").keyup(server_action);
    //jQuery("#ontology").change(assoc_saction);
    jQuery("#document_category").change(server_action);
    jQuery("#type").change(server_action);
    jQuery("#taxon").change(server_action);
    jQuery("#source").change(server_action);
    jQuery("#evidence_type").change(server_action);
    jQuery("#isa_partof_label_closure").change(server_action);
    jQuery("#annotation_extension_class_label").change(server_action);
    jQuery("#annotation_extension_class_label_closure").change(server_action);

    // NOTE: we can either use this or the one above.
    // // Slow down the input on our typing fields.
    // function delayed_keyup_action(selector, action, delay){
    // 	jQuery(selector).keyup(function(){
    // 	    if( typeof(window.inputTimeout) != 'undefined' ){
    // 		window.clearTimeout(window.inputTimeout);
    // 	    }
    // 	    window.inputTimeout = window.setTimeout(action, delay);
    // 	});
    // }
    // delayed_keyup_action("#q", server_action, delay_in_ms);

    // Make the forms unsubmitable.
    jQuery("#app-form").submit(function(){return false;});

    // TODO: first pass update on all facets.
    var init_url = gm.golr_base() + '/select?qt=standard&indent=on&wt=json&version=2.2&rows=10&start=0&fl=*%2Cscore&facet=true&facet.mincount=1&facet.field=document_category&facet.field=type&facet.field=evidence_type&facet.field=source&facet.field=taxon&facet.field=isa_partof_label_closure&facet.field=annotation_extension_class_label&facet.field=annotation_extension_class_label_closure&q=*:*&packet=1';
    last_sent_packet = 1; // TODO/BUG: Packeting getting awkward--class?
    core.kvetch('trying initialization: ' + init_url);
    // JSONP errors are hard to catch.
    // http://bugs.jquery.com/ticket/3442
    var init_argvars = {
	type: "GET",
	url: init_url,
	dataType: 'json',
	jsonp: 'json.wrf',
	success: _process_results,
	error: function (result, status, error) {
	    core.kvetch('ERROR: Failed initialization request (1)');
	}
    };
    jQuery.ajax(init_argvars);
    //var req = jQuery.ajax(init_argvars);
    //var req = jQuery.getJSON(init_argvars, _process_results);
    //req.error(function (result, status, error) {
    // 		  core.kvetch('ERROR: Failed initialization request: ' +
    // 			     status + ', ' + error);
    // 	      });
}


///
/// Helper functions that should be rolled into a new GUI generation
/// object at some point.
///


//
function _clear_app_forms(){
    jQuery("#app-form-query").empty();
    jQuery("#app-form-filters").empty();
}


///
/// Results processing.
///


// Convert the return JSON results into something usable...
// Include link arrows to page the results.
function _process_meta_results (json_data){

    // Grab meta information.
    var total = core.golr_response.total_documents(json_data);
    var first = core.golr_response.start_document(json_data);
    var last = core.golr_response.end_document(json_data);
    var meta_cache = new Array();
    meta_cache.push('Total: ' + total);

    // Only have paging headers is necessary.
    if( total > 0 ){
	meta_cache.push('&nbsp;&nbsp;&nbsp;First: ' + (first + 1));
	meta_cache.push('&nbsp;&nbsp;&nbsp;Last: ' + last);
    }

    // Add a special message is the result didn't contain any results.
    if( total == 0 ){

	//
	meta_cache.push('<p>');
	meta_cache.push("No results were returned. Perhaps you forgot to include a wildcard ('*') after your query?");
	meta_cache.push('</p>');
	meta_cache.push('<p>');
	meta_cache.push('Please read the <a title="Go to Live Search documentation" href="http://wiki.geneontology.org/index.php/AmiGO_Manual:_Live_Search">Live Search documentation</a> for more details on how to search and how to get the results that you want.');
	meta_cache.push('</p>');

    }else{

	meta_cache.push('<br />');

	// Our element ids.
	var backward_id = 'bak_paging_id_' + core.util.randomness(10);
	var forward_id = 'for_paging_id_' + core.util.randomness(10);

	// Determine which arguments we'll (or would) need to page
	// forwards or backwards.
	var b_args = null;
	//b_args = core.util.clone(args);
	b_args = core.util.clone(core.golr_response.parameters(json_data));
	//if( ! b_args['index'] ){ b_args['index'] = 2; }
	b_args['start'] = parseInt(b_args['start']) -
	    core.golr_response.row_step(json_data);
	var f_args = null;
	//f_args = core.util.clone(args);
	f_args = core.util.clone(core.golr_response.parameters(json_data));
	//if( ! f_args['index'] ){ f_args['index'] = 1; }
	f_args['start'] = parseInt(f_args['start']) +
	    core.golr_response.row_step(json_data);

	// Increment packet (async ordering).
	b_args['packet'] = last_sent_packet++;
	f_args['packet'] = last_sent_packet++;

	// Determine which results processor and urls we'll (or would)
	// use for the binding(s).
	var proc = null;
	var backward_url = null;
	var forward_url = null;
	proc = _process_results;
	backward_url = gm.golr_base() + '/' + core.api.live_search.golr(b_args);
	forward_url = gm.golr_base() + '/' + core.api.live_search.golr(f_args);
	
	// Generate the necessary paging html.
	if( first > 0 ){
	    meta_cache.push(' <a href="#results_block" id="' +
			    backward_id + '"><- back</a>');
	}
	if( last < total ){
	    meta_cache.push(' <a href="#results_block" id="' +
			    forward_id + '">forward -></a>');
	}
    }
    
    // Add all of the html.
    jQuery('#meta_results').html(meta_cache.join(''));

    // Where necessary, add forwards and backwards click bindings.
    if( first > 0 ){
	_paging_binding(backward_id, backward_url, proc);
    }
    if( last < total ){
	_paging_binding(forward_id, forward_url, proc);
    }    
}


// Update the GUI elements to reflect what came back from solr.
function _update_gui (json_data){

    // // Grab meta information.
    // var total = core.golr_response.total_documents(json_data);
    // var first = core.golr_response.start_document(json_data);
    // var last = core.golr_response.end_document(json_data);

    core.kvetch("GUI: Updating...");

    // Capture the current filters and facets. They come in as a hash
    // of arrays.
    var qfilters = core.golr_response.query_filters(json_data);
    var qfacets = core.golr_response.facet_counts(json_data);

    // // Define pre-defined filters.
    // var filterables = [
    // ];

    // // Operate on filters that have a pre-defined base.
    // for( var fi = 0; fi < filterables.length ; fi++){
    // 	var filterable = filterables[fi];
    // 	var curr_filter_id = filterable['filter_id'];
    // 	var curr_model = filterable['model'];
    // 	var curr_widget = filterable['widget'];

    // 	// core.kvetch("looking at facet: " + curr_filter_id);
    // 	// core.kvetch("\tmodel: " + curr_model);
    // 	// core.kvetch("\twidget: " + curr_widget);

    // 	// Update the model with query filters and facet counts. Since the
    // 	// return data is considered comprehensive, if one is not
    // 	var all_filters = curr_model.get_all_items();
    // 	//core.kvetch("all " + curr_filter_id + " filters: " + all_filters);
    // 	for( var ptfi = 0; ptfi < all_filters.length; ptfi++ ){
    //  	    var try_filter = all_filters[ptfi];
    //  	    // core.kvetch("try filter: " + try_filter);
    //  	    if( qfilters[curr_filter_id] &&
    // 		qfilters[curr_filter_id][try_filter] ){
    // 		curr_model.update_value(try_filter, 'selected', true);
    // 	    }else{
    // 		curr_model.update_value(try_filter, 'selected', false);
    // 	    }

    // 	    // Look at whether or not there is a count with it and add.
    //  	    if( qfacets[curr_filter_id] &&
    // 		typeof qfacets[curr_filter_id][try_filter] != 'undefined' ){
    // 		    var new_val = qfacets[curr_filter_id][try_filter];
    // 		    curr_model.update_value(try_filter, 'count', new_val);
    // 		}else{
    // 		    curr_model.update_value(try_filter, 'count', 0);
    // 		}
    // 	}

    // 	// Update it.
    // 	curr_widget.update_with(curr_model.get_state());
    // 	curr_widget.render_update();
    // }

    // Define dynamic filters.
    var dyn_filterables = [
	{
	    filter_id: 'type',
	    model: type_model,
	    widget: type_widget
	},
	{
	    filter_id: 'taxon',
	    model: taxon_model,
	    widget: taxon_widget
	},
	{
	    filter_id: 'source',
	    model: source_model,
	    widget: source_widget
	},
	{
	    filter_id: 'evidence_type',
	    model: evidence_model,
	    widget: evidence_widget
	},
	{
	    filter_id: 'document_category',
	    model: document_category_model,
	    widget: document_category_widget
	},
	{
	    filter_id: 'isa_partof_label_closure',
	    model: ip_lc_model,
	    widget: ip_lc_widget
	},
	// {
	//     filter_id: 'annotation_extension_class_label',
	//     model: aecl_model,
	//     widget: aecl_widget
	// },
	{
	    filter_id: 'annotation_extension_class_label_closure',
	    model: aecl_closure_model,
	    widget: aecl_closure_widget
	}
    ];

    // Operate on filters that have only return data to work with.
    for( var dfi = 0; dfi < dyn_filterables.length ; dfi++){
	var filterable = dyn_filterables[dfi];
	var curr_filter_id = filterable['filter_id'];
	var curr_model = filterable['model'];
	var curr_widget = filterable['widget'];

    	// core.kvetch("looking at facet: " + curr_filter_id);
    	// core.kvetch("\tmodel: " + curr_model);
    	// core.kvetch("\twidget: " + curr_widget);

	// Iterate over all facet values.
	var facet_keys = core.util.get_hash_keys(qfacets[curr_filter_id]);
	//core.kvetch("facet_keys: " + facet_keys);

	// Get all things currently in the model.
	var all_item_keys = curr_model.get_all_items();
	//core.kvetch("all_item_keys: " + all_item_keys);

	// Join them and update over the whole set.
	var all_keys = facet_keys.concat(all_item_keys);

	//core.kvetch("all " + curr_filter_id + " filters: " + all_keys);

	for( var aki = 0; aki < all_keys.length; aki++ ){
	    var curr_asp = all_keys[aki];
	    //core.kvetch("looking at: " + curr_asp);
	    
	    // Add things to the model if they aren't there.
	    if( ! curr_model.has_item(curr_asp) ){
		curr_model.add_item(curr_asp, {
					value: curr_asp,
					label: curr_asp,
					count: 0,
					selected: false,
					special: false
 				    });
		//core.kvetch("added: " + curr_asp);
	    }

	    // Look at whether or not there is a count with it. If
	    // there is no facet there, the count is reset to zero.
     	    if( typeof qfacets[curr_filter_id][curr_asp] == 'undefined' ){
		curr_model.update_value(curr_asp, 'count', 0);
	    }else{
		var new_val = qfacets[curr_filter_id][curr_asp];
		//core.kvetch("change " + curr_asp + ' to ' + new_val);
		curr_model.update_value(curr_asp, 'count', new_val);
	    }
	    
	    // ...
     	    if( qfilters[curr_filter_id] &&
		qfilters[curr_filter_id][curr_asp] ){
		    curr_model.update_value(curr_asp, 'selected', true);
		}else{
		    curr_model.update_value(curr_asp, 'selected', false);
		}
	    
	}

	// Update it.
	curr_widget.update_with(curr_model.get_state());
	curr_widget.render_update();
    }
}


// Convert the return JSON results into something usable...
function _process_results (json_data, status){

    core.kvetch('Checking results...');    

    // Clear the watchdog:
    watchdog_solr_is_responding = true;

    // Some trivial validation here.
    if( core.golr_response.success(json_data) ){

	core.kvetch('Results okay...');
	
	// Packet order checking.
	var in_params = core.golr_response.parameters(json_data);
	var our_packet = parseInt(in_params.packet);
	core.kvetch("packet: "+ our_packet +" (>? "+ last_received_packet +")");
	if( our_packet && our_packet > last_received_packet ){
	    
	    core.kvetch("Usable return packet: " + our_packet);
	    
	    // Set last received.
	    last_received_packet = our_packet;
	    
	    // Check to see if there is someting there first
	    var cache = new Array();
	    if( core.golr_response.total_documents(json_data) < 1 ){
		core.kvetch("No results (empty).");
	    }else{
		// Process main results table.
		var brdg = core.golr_response.documents(json_data);
		cache = _table_cache_from_results(brdg);
	    }
	    
	    // Set results div text. If there were no results, this
	    // will be cleared.
	    jQuery('#results_div').html('<p>' + cache.join('') + '</p>');
	    
	    // Set the text in the meta area.
	    _process_meta_results(json_data);

	    // Update filters to reflect solr's response contents.
	    _update_gui(json_data);
	    
	}else{
	    core.kvetch("Dropping packet.");
	}
    }else{
	core.kvetch("Invalid response.");
	core.kvetch("Data: " + json_data);
	core.kvetch("Status: " + status);
	if( json_data ){
	    core.kvetch("Data okay.");
	}
	if( json_data.response ){
	    core.kvetch("Response okay.");
	}
	if( json_data.responseHeader ){
	    core.kvetch("Header okay.");
	}
	if( json_data.facet_counts ){
	    core.kvetch("Facets okay.");
	}
    }
    //core.kvetch("finish wait");
    widgets.finish_wait();
    core.kvetch("Pass finish.");
}


//
function _table_cache_from_results (dlist){
    
    core.kvetch("Table: Refreshing...");
    //core.kvetch("Table: dlist.length: " + dlist.length);

    // Results rows first to see what's in there.
    var row_cache = [];
    var has_term_p = false;
    var has_annotation_p = false;
    var has_bioentity_p = false;
    for( var i = 0; i < dlist.length; i++ ){

	var r = dlist[i];
	// var encoded_acc = coder.encode(r.dbxref);

	// Create HTML.
	if( i % 2 == 0 ){
	    row_cache.push('<tr class="odd_row">');
	}else{
	    row_cache.push('<tr class="even_row">');
	}

	// TODO: document type identification.
	if( r.document_category ){
	    if( 'annotation' == r.document_category ){
		row_cache.push(_annotation_line(r));
		has_annotation_p = true;
	    }else if( 'ontology_class' == r.document_category ){
		row_cache.push(_term_line(r));
		has_term_p = true;
	    }else if( 'bioentity' == r.document_category ){
		row_cache.push(_bioentity_line(r));
		has_bioentity_p = true;
	    }else{
		row_cache.push('Unknown document category: '+
			       r.document_category );
	    }
	}else{
	    row_cache.push('WTFBBQ!');
	}

	row_cache.push('</tr>');
    }

    // Bulk change.
    var head_cache = new Array();
    // Term.
    if( has_term_p && has_annotation_p ){	
	// Term.
	head_cache.push('<tr>');
	head_cache.push('<th colspan="1" rowspan="2">score</th>');
	head_cache.push('<th colspan="1" rowspan="2">category</th>');
	head_cache.push('<th colspan="7">label/description</th>');
	head_cache.push('</tr>');
	// GP.
	head_cache.push('<tr>');
	head_cache.push('<th>score</th>');
	head_cache.push('<th>category</th>');
	head_cache.push('<th>symbol</th>');
	head_cache.push('<th>type</th>');
	head_cache.push('<th>description</th>');
	head_cache.push('<th>source</th>');
	head_cache.push('<th>species</th>');
	head_cache.push('</tr>');
	// Annotation.
	head_cache.push('<tr>');
	head_cache.push('<th>symbol</th>');
	head_cache.push('<th>ev</th>');
	head_cache.push('<th>term</th>');
	head_cache.push('<th>type</th>');
	head_cache.push('<th>extension</th>');
	head_cache.push('<th>source</th>');
	head_cache.push('<th>species</th>');
	head_cache.push('</tr>');
    }else if( has_annotation_p ){
	// Annotation.
	head_cache.push('<tr>');
	head_cache.push('<th>score</th>');
	head_cache.push('<th>category</th>');
	head_cache.push('<th>symbol</th>');
	head_cache.push('<th>ev</th>');
	head_cache.push('<th>term</th>');
	head_cache.push('<th>type</th>');
	head_cache.push('<th>extension</th>');
	head_cache.push('<th>source</th>');
	head_cache.push('<th>species</th>');
	// // cache.push('<th>synonym(s)</th>');
	head_cache.push('</tr>');
    }else if( has_bioentity_p ){
	// GP/Bioentity.
	head_cache.push('<tr>');
	head_cache.push('<th>score</th>');
	head_cache.push('<th>category</th>');
	head_cache.push('<th>symbol</th>');
	head_cache.push('<th>type</th>');
	head_cache.push('<th>description</th>');
	head_cache.push('<th>source</th>');
	head_cache.push('<th>species</th>');
	head_cache.push('</tr>');
    }else if( has_term_p ){	
	head_cache.push('<tr>');
	head_cache.push('<th colspan="1">score</th>');
	head_cache.push('<th colspan="1">category</th>');
	head_cache.push('<th colspan="7">label/description</th>');
	head_cache.push('</tr>');
    }else{
	head_cache.push('<tr>');
	head_cache.push('<th colspan="9">???</th>');
	head_cache.push('</tr>');	
    }
    // TODO: Now add the rows that we cached.
    
    var over_cache = [];
    over_cache.push('<table>');
    over_cache.push('<thead>');
    for( var qwe = 0; qwe < head_cache.length; qwe++ ){
	over_cache.push(head_cache[qwe]);
    }
    over_cache.push('</thead><tbody>');
    for( var rty = 0; rty < row_cache.length; rty++ ){
	over_cache.push(row_cache[rty]);
    }
    over_cache.push('</tbody></table>');
    
    return over_cache;
}


// Write an annotation line. Currently 9 columns.
function _annotation_line(r){
    
    //core.kvetch("Writing annotation line...");
    var cache = new Array();

    // 1 Score.
    cache.push('<td>');
    //cache.push((parseInt(r.score) * 100.00) + '%');
    cache.push(parseInt(r.score) + '%');
    cache.push('</td>');

    // 2
    cache.push('<td>');
    cache.push('annotation');
    cache.push('</td>');

    // 3 GP symbol.
    cache.push('<td>');
    cache.push(core.html.gene_product_link(r.bioentity_id,
					   r.bioentity_label));
    cache.push('</td>');

    // 4 Evidence.
    cache.push('<td>');
    cache.push(r.evidence_type);
    // //core.kvetch('homolset status: ' + r.homolset);
    // if( r.homolset == 'included' ){
    //   cache.push('<img src="' + gm.get_image_resource('star') + '"');
    //   cache.push(' title="This gene product is a member of a homolset." />');
    // }else{
    //     cache.push('&nbsp;');
    // }
    cache.push('</td>');

    // 5 Term info.
    //var tlink = core.link.term({acc: r.annotation_class});
    cache.push('<td>');
    cache.push(core.html.term_link(r.annotation_class,
				   r.annotation_class_label));
    cache.push(' (');
    cache.push(r.annotation_class);
    cache.push(')');
    cache.push('</td>');
    
    // 6 Type.
    cache.push('<td>');
    cache.push(r.type);
    cache.push('</td>');
    
    // 7 Extension.
    function _no_wrap(in_str){
	return '<span class="nowrap">' + in_str + '</span>';
    }
    cache.push('<td>');
    var aebuf = [];
    if( r.annotation_extension_class_label ){
	if( typeof r.annotation_extension_class_label == 'string' ){
	    aebuf.push(_no_wrap(r.annotation_extension_class_label));
	}else{
	    for( var aecli = 0;
		 aecli < r.annotation_extension_class_label.length;
		 aecli++ ){
		     var aeitem = r.annotation_extension_class_label[aecli];
		     aebuf.push(_no_wrap(aeitem));
	    	 }
	}
    }
    cache.push(aebuf.join('<br />'));
    cache.push('</td>');
    
    // 8 Source.
    cache.push('<td>');
    cache.push(r.source);
    cache.push('</td>');
    
    // 9 Species. Simple names aren't split, but complicated
    // ones are.
    var species_map = gm.species_map();
    var tax_splits = r.taxon.split(":");
    var simple_taxon_id = tax_splits[1];
    var s_name = species_map[simple_taxon_id];
    if( s_name && s_name.split(' ').length <= 2 ){
	cache.push('<td class="nowrap">');
    }else{
	cache.push('<td class="">');
    }
    if( ! s_name ){
	s_name = r.taxon;
    }    
    cache.push(s_name);
    //cache.push(r.taxon);
    cache.push('</td>');
	
    // // Synonyms.
    // cache.push('<td>');
    // //cache.push(r.hilite_gpsynonym.replace(newline_finder, ", "));
    // cache.push('nil');
    // cache.push('</td>');

    return cache.join('');
}


// Write an term line. Currently 3 (of 9) columns.
function _term_line(r){
    
    //core.kvetch("Writing annotation line...");
    var cache = new Array();

    // 1 Score.
    cache.push('<td colspan="1">');
    //cache.push((parseInt(r.score) * 100.00) + '%');
    cache.push(parseInt(r.score) + '%');
    cache.push('</td>');

    // 2 
    cache.push('<td colspan="1">');
    cache.push('term');
    cache.push('</td>');

    // 3 ...
    cache.push('<td colspan="7">');
    cache.push(r.label);
    cache.push(' (');
    cache.push(r.id);
    cache.push(')');
    if( r.description ){
	cache.push('<p>');
	cache.push(r.description);
	cache.push('</p>');
    }
    cache.push('</td>');

    return cache.join('');
}


// Write a bioentity line. Currently X (of Y) columns.
function _bioentity_line(r){
    
    //core.kvetch("Writing bioentity line...");
    var cache = new Array();

    // 1 Score.
    cache.push('<td colspan="1">');
    //cache.push((parseInt(r.score) * 100.00) + '%');
    cache.push(parseInt(r.score) + '%');
    cache.push('</td>');

    // 2 Doc category.
    cache.push('<td colspan="1">');
    cache.push('bioentity');
    cache.push('</td>');

    // 3 GP symbol.
    cache.push('<td>');
    cache.push(core.html.gene_product_link(r.id, r.label));
    cache.push('</td>');

    // 4 Type.
    cache.push('<td>');
    cache.push(r.type);
    cache.push('</td>');
    
    // 5 Descriptive name.
    cache.push('<td>');
    cache.push(r.descriptive_name);
    cache.push('</td>');
    
    // 6 Source.
    cache.push('<td>');
    cache.push(r.source);
    cache.push('</td>');

    // 7 Taxon.
    cache.push('<td>');
    cache.push(r.taxon);
    cache.push('</td>');
    
    return cache.join('');
}


///
/// Paging.
///


// Action binding for pager.
function _paging_binding(elt_id, url, processor){

    // TODO: bind actions to things in the action column.
    core.kvetch("paging bindings on " + elt_id + "...");
    var elt = jQuery("#" + elt_id);
    if( elt && elt.attr && elt.attr('id') == elt_id ){

	// Show menu on click.
	elt.click(function(){

	    core.kvetch("clicked_on_pager, try: " + url);
	    widgets.start_wait('Paging...');
	    jQuery.ajax({
	    	type: "GET",
	    	url: url,
	    	dataType: 'json',
		jsonp: 'json.wrf',
	    	success: processor,
	    	error: function (result, status, error) {
	    	    core.kvetch('Failed server request (paging): ' + status);
		    widgets.finish_wait();
	    	}
	    });
	});
    }
}
