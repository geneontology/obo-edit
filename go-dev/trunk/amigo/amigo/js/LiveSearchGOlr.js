////
//// A full take on a production live search for GOlr--try and make it
//// work directly off of the server for giggles/testing.
////


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

// Find newlines in text.
var newline_finder = new RegExp("\n", "g");


// Get the layout done and request GO meta-info.
function LiveSearchGOlrInit(){

    core.kvetch('');
    core.kvetch('LiveSearchGOlrInit start.');

    ///
    /// Try and get UI ready.
    ///

    //
    core.kvetch('Apply tabs...');
    jQuery("#search-tabs").tabs();
    jQuery("#search-tabs").tabs('select', 0);

    widgets = new org.bbop.amigo.ui.widgets();

    core.kvetch('LiveSearchGOlr init completed.');

    // Pull in GO meta info.
    //var ontology_data = gm.ontologies();
    var species_data = gm.species();
    var source_data = gm.sources();
    var type_data = gm.gp_types();

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
    
    // Clear the controls' area.
    _clear_app_forms();

    // Create the new form for a GOlr search.
    var hidden_document_category = // TODO: make dynamic later
    	widgets.form.hidden_input('document_category', 'annotation');
    // var hidden_mode_search_text =
    // 	widgets.form.hidden_input('mode', 'live_search_association_golr');
    var query_text =
    	widgets.form.text_input('q', 'q', 25, 
				'Search GO for ???<br />');
    // var ontology_text =
    // 	widgets.form.multiselect('ontology', 'ontology', 4,
    // 				 ontology_data, 'Ontology');

    // var type_text = widgets.form.multiselect('type', 'type', 4,
    // 					     type_data, 'GP type');
    type_model = new org.bbop.amigo.ui.interactive.multi_model(type_data);
    type_widget =
	new org.bbop.amigo.ui.interactive.multi_widget('type', 'type',
						       4, 'GP type');
    type_widget.update_with(type_model.get_state());

    var type_text = type_widget.render_initial();

    var taxon_text =
    	widgets.form.multiselect('taxon', 'taxon', 4,
    				 species_data, 'Species');
    var source_text =
    	widgets.form.multiselect('source', 'source', 4,
    				 source_data, 'Data source');
    var evidence_type_text =
    	widgets.form.multiselect('evidence_type', 'evidence_type', 4,
    				 evcode_set, 'Evidence');
    //jQuery("#app-form").append(hidden_mode_search_text);
    jQuery("#app-form").append(hidden_count_text);
    jQuery("#app-form").append(hidden_facet_text);
    jQuery("#app-form").append(hidden_facet_field_type_text);
    jQuery("#app-form").append(hidden_facet_field_ev_type_text);
    jQuery("#app-form-query").append(query_text);
    //jQuery("#app-form-filters").append(ontology_text);
    jQuery("#app-form-filters").append(type_text);
    jQuery("#app-form-filters").append(taxon_text);
    jQuery("#app-form-filters").append(source_text);
    jQuery("#app-form-filters").append(evidence_type_text);

    //core.kvetch('GP type text: ' + type_text );

    function _generate_action_to_server(marshaller, do_results){//, query_id){
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
			kc ==  8 || // delete
			kc ==  0 ){ // super
			    core.kvetch('ignorable key event: ' + kc);
			    ignorable_event_p = true;
			}
		}
	    }
	    
	    //
	    if( ! ignorable_event_p ){

		// And...um... convert q to the correct query.
		var all_inputs = marshaller();
		//all_inputs['q'] = all_inputs[query_id];
		
		// Cut down on overhead a little.
		if( all_inputs &&
		    all_inputs['q'] &&
		    all_inputs['q'][0] &&
		    all_inputs['q'][0].length >= 3 ){
			
			core.kvetch('input q: ' + all_inputs['q'][0]);

			// Increment packet (async ordering).
			last_sent_packet++;
			all_inputs['packet'] = last_sent_packet;

			// BUG/TODO: a switch to dismax will eliminate
			// this, this is just here to bootstrap
			// debugging for now.
			all_inputs['q'][0] =
			    'annotation_class_label:' + all_inputs['q'][0];

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
	    		    dataType: 'jsonp',
			    jsonp: 'json.wrf',
	    		    success: do_results,
	    		    error: function (result, status, error) {
				
	    			core.kvetch('Failed server request ('+
					    query_id + '): ' + status);
				
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
	    	    }else{
			core.kvetch('Threshold not passed with: ' +
				    all_inputs['q'][0]);
		    }
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
    jQuery("#type").change(server_action);
    jQuery("#taxon").change(server_action);
    jQuery("#source").change(server_action);
    jQuery("#evidence_type").change(server_action);

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
    if( total > 10 ){
	meta_cache.push('&nbsp;&nbsp;&nbsp;First: ' + first);
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
	if( ! b_args['index'] ){ b_args['index'] = 2; }
	b_args['index'] = parseInt(b_args['index']) - 1;
	var f_args = null;
	//f_args = core.util.clone(args);
	f_args = core.util.clone(core.golr_response.parameters(json_data));
	if( ! f_args['index'] ){ f_args['index'] = 1; }
	f_args['index'] = parseInt(f_args['index']) + 1;

	// Increment packet (async ordering).
	b_args['packet'] = last_sent_packet++;
	f_args['packet'] = last_sent_packet++;

	// Determine which results processor and urls we'll (or would)
	// use for the binding(s).
	var proc = null;
	var backward_url = null;
	var forward_url = null;
	proc = _process_results;
	backward_url = core.api.live_search.golr(b_args);
	forward_url = core.api.live_search.golr(f_args);
	
	// Generate the necessary paging html.
	if( first > 1 ){
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
    if( first > 1 ){
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

    core.kvetch("Updating GUI...");

    // Capture the current filters and facets. They come in as a hash
    // of arrays.
    var qfilters = core.golr_response.query_filters(json_data);
    var qfacets = core.golr_response.facet_counts(json_data);

    // Update the model with query filters and facet counts. Since the
    // return data is considered comprehensive, if one is not found in
    // the return data it is reset.
    var all_filters = type_model.get_all_filters();
    core.kvetch("all type filters: " + all_filters);
    for( var ptfi = 0; ptfi < all_filters.length; ptfi++ ){
     	var try_filter = all_filters[ptfi];
     	// core.kvetch("try filter: " + try_filter);

	// TODO: loop over each tryable filter...
	// TODO: only show filters that have counts...

	// Look at whether or not it was selected.
	// core.kvetch("1: " + qfilters );
	// core.kvetch("2: " + typeof qfilters );
	// core.kvetch("3: " + qfilters['type'] );
	// core.kvetch("4: " + typeof qfilters['type'] );
     	if( qfilters['type'] && qfilters['type'][try_filter] ){
	    type_model.update_value(try_filter, 'selected', true);
	}else{
	    type_model.update_value(try_filter, 'selected', false);
	}

	// Look at whether or not there is a count with it and add.
     	if( qfacets['type'] &&
	    typeof qfacets['type'][try_filter] != 'undefined' ){
		type_model.update_value(try_filter, 'count',
					qfacets['type'][try_filter]);
	    }else{
		type_model.update_value(try_filter, 'count', 0);
	    }
    }

    // TODO/BUG:
    type_widget.update_with(type_model.get_state());

    // Update it.
    type_widget.render_update();
}


// Convert the return JSON results into something usable...
function _process_results (json_data, status){

    core.kvetch('Checking results...');    

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
	    if( core.golr_response.total_documents(json_data) == 0 ){
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
    }
    //core.kvetch("finish wait");
    widgets.finish_wait();
}


// // Convert the return JSON results into something usable...
// function _process_results (json_data, status){

//     core.kvetch('checking results...');

//     // Verify the incoming data. Only go if we got something.
//     if( core.response.success(json_data) &&
// 	core.response.type(json_data) == 'live_search_golr' ){
// 	core.kvetch('assoc results okay...');

// 	// Packet order checking.
// 	var in_args = core.response.arguments(json_data);
// 	var our_packet = parseInt(in_args.packet);
// 	core.kvetch("packet: "+ our_packet +" (>? "+ last_received_packet +")");
// 	if( our_packet && our_packet > last_received_packet ){

// 	    core.kvetch("order usable packet: "+ our_packet);

// 	    // Set last received.
// 	    last_received_packet = our_packet;

// 	    // Okay, get the results data.
// 	    var ls = new org.bbop.amigo.live_search(json_data);
// 	    var struct = ls.results();

// 	    var species_map = gm.species_map();

// 	    // Bulk change.
// 	    var cache = new Array();
// 	    cache.push('<table>');
// 	    cache.push('<thead><tr>');

// 	    cache.push('<th>score</th>');

// 	    cache.push('<th>evidence</th>');

// 	    cache.push('<th>acc</th>');
// 	    cache.push('<th>name</th>');
// 	    //cache.push('<th>ontology</th>');
// 	    cache.push('<th>synonym(s)</th>');

// 	    cache.push('<th>dbxref</th>');
// 	    cache.push('<th>symbol</th>');
// 	    cache.push('<th>full_name</th>');
// 	    cache.push('<th>type</th>');
// 	    cache.push('<th>source</th>');
// 	    cache.push('<th>species</th>');
// 	    cache.push('<th>gpsynonym(s)</th>');

// 	    cache.push('</tr></thead><tbody>');
// 	    for( var i = 0; i < struct.length; i++ ){

// 		var r = struct[i];
// 		// var encoded_acc = coder.encode(r.acc);
// 		// var encoded_dbxref = coder.encode(r.dbxref);

// 		// Create HTML.
// 		if( i % 2 == 0 ){
// 		    cache.push('<tr class="odd_row">');
// 		}else{
// 		    cache.push('<tr class="even_row">');
// 		}

// 		// Score.
// 		cache.push('<td>');
// 		cache.push(r.score + '%');
// 		cache.push('</td>');

// 		// Evidence.
// 		cache.push('<td>');
// 		cache.push(r.evidence);
// 		cache.push('</td>');
		
// 		// Term section.
// 		cache.push('<td>');
// 		cache.push('<a title="link to information on ' + r.acc +
// 			   '" href=\"' + r.term_link +
// 			   '">' + r.hilite_acc +
// 			   '</a>');
// 		cache.push('</td>');
// 		cache.push('<td>');
// 		cache.push(r.hilite_name);
// 		cache.push('</td>');
// 		cache.push('<td>');
// 		cache.push(r.hilite_ontology);
// 		//cache.push('n/a');
// 		cache.push('</td>');
// 		cache.push('<td>');
// 		//cache.push(r.hilite_synonym);
// 		cache.push(r.hilite_synonym.replace(newline_finder, ", "));
// 		cache.push('</td>');

// 		// GP section.
// 		cache.push('<td>');
// 		cache.push('<a title="link to information on ' + r.dbxref +
// 			   '" href=\"' + r.gene_product_link +
// 			   '">' + r.hilite_dbxref +
// 			   '</a>');
// 		cache.push('</td>');
// 		cache.push('<td>');
// 		cache.push(r.hilite_symbol);
// 		cache.push('</td>');
// 		cache.push('<td>');
// 		cache.push(r.hilite_full_name);
// 		cache.push('</td>');
// 		cache.push('<td>');
// 		cache.push(r.hilite_gptype);
// 		cache.push('</td>');
// 		cache.push('<td>');
// 		cache.push(r.hilite_source);
// 		cache.push('</td>');
// 		// Simple names aren't split, complicated ones are.
// 		var s_name = species_map[r.species];
// 		if( s_name && s_name.split(' ').length <= 2 ){
// 		    cache.push('<td class="nowrap">');
// 		}else{
// 		    cache.push('<td class="">');
// 		}
// 		cache.push(species_map[r.species]);
// 		cache.push('</td>');
// 		cache.push('<td>');
// 		//cache.push(r.hilite_synonym);
// 		cache.push(r.hilite_gpsynonym.replace(newline_finder, ", "));
// 		cache.push('</td>');
// 		cache.push('</tr>');
// 	    }
// 	    cache.push('</tbody></table>');

// 	    // Set div text.
// 	    jQuery('#results_div').html('<p>' + cache.join('') + '</p>');

// 	    _process_meta_results(ls, 'association',
// 				  core.response.arguments(json_data));

// 	}else{
// 	    core.kvetch("dropping packet");
// 	}
//     }
//     widgets.finish_wait();
// }

//
function _table_cache_from_results (dlist){
    
    var species_map = gm.species_map();

    // Bulk change.
    var cache = new Array();
    cache.push('<table>');
    cache.push('<thead><tr>');
    cache.push('<th>score</th>');
    cache.push('<th>acc</th>');
    cache.push('<th>ev</th>');
    cache.push('<th>symbol</th>');
    cache.push('<th>name</th>');
    cache.push('<th>type</th>');
    cache.push('<th>source</th>');
    cache.push('<th>species</th>');
    cache.push('<th>synonym(s)</th>');
    cache.push('</tr></thead><tbody>');
    for( var i = 0; i < dlist.length; i++ ){

	var r = dlist[i];
	// var encoded_acc = coder.encode(r.dbxref);

	// Create HTML.
	if( i % 2 == 0 ){
	    cache.push('<tr class="odd_row">');
	}else{
	    cache.push('<tr class="even_row">');
	}

	// Score.
	cache.push('<td>');
	cache.push(r.score + '%');
	cache.push('</td>');

	// GO term acc.
	cache.push('<td>');
	cache.push(r.annotation_class);
	// cache.push('<a title="link to information on ' + r.dbxref +
	// 	   '" href=\"' + r.link +
	// 	   '">' + r.hilite_dbxref +
	// 	   '</a>');
	cache.push('</td>');

	// Evidence.
	cache.push('<td>');
	cache.push(r.evidence_type);
	// //core.kvetch('homolset status: ' + r.homolset);
	// if( r.homolset == 'included' ){
	//     cache.push('<img src="' + gm.get_image_resource('star') + '"');
	//     cache.push(' title="This gene product is a member of a homolset." />');
	// }else{
	//     cache.push('&nbsp;');
	// }
	cache.push('</td>');

	// GP symbol.
	cache.push('<td>');
	cache.push(r.bioentity_label);
	cache.push('</td>');

	// Full name.
	cache.push('<td>');
	cache.push(r.annotation_class_label);
	cache.push('</td>');

	// Type.
	cache.push('<td>');
	cache.push(r.type);
	cache.push('</td>');

	// Source.
	cache.push('<td>');
	cache.push(r.source);
	cache.push('</td>');

	// // Species. Simple names aren't split, but complicated
	// // ones are.
	// var s_name = species_map[r.species];
	// if( s_name && s_name.split(' ').length <= 2 ){
	//     cache.push('<td class="nowrap">');
	// }else{
	//     cache.push('<td class="">');
	// }
	// cache.push(species_map[r.species]);
	cache.push('<td>');
	cache.push(r.taxon);
	cache.push('</td>');
	
	// Synonyms.
	cache.push('<td>');
	//cache.push(r.hilite_gpsynonym.replace(newline_finder, ", "));
	cache.push('nil');
	cache.push('</td>');
	
	cache.push('</tr>');
    }
    cache.push('</tbody></table>');
    
    return cache;
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
	    	success: processor,
	    	error: function (result, status, error) {
	    	    core.kvetch('Failed server request (paging): ' + status);
		    widgets.finish_wait();
	    	}
	    });
	});
    }
}
