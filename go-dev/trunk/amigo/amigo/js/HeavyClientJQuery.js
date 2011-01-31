////
//// Experiments with heavy ext js client.
////
////


// Bring in the AmiGO core and keep a coder handy.
org.bbop.amigo.DEBUG = true;
var core = new org.bbop.amigo.core();
var gm = new org.bbop.amigo.go_meta();
//var cart = new org.bbop.amigo.ui.cart('cart-short-circut');
var cart = null;
var coder = new core.util.coder();
var last_sent_packet = 0;
var last_received_packet = 0;

// Might come in handy for GUI stuff as we go.
var layout = null;

// Our separate widget and notice object.
var widgets = null;

// Find newlines in text.
var newline_finder = new RegExp("\n", "g");

var id_item_hash = {};

//
// var global_workspace_list = [];

// Memory for last selected workspace.
//var global_last_workspace_selected = 'default';


// Get the layout done and request GO meta-info.
function HeavyClientInit(){

    core.kvetch('');
    core.kvetch('HeavyClientInit start.');

    // Needs to be done in here to prevent the append operation from
    // barfing during initialization.
    cart = new org.bbop.amigo.ui.cart('cart-short-circut');

    ///
    /// Try and get ready for menus by querying the workspace.
    ///

    // Go and get the workspace status information so we can finish the UI.
    function _populate_workspace_list(json_data, status){

	core.kvetch('_populate_workspace_list');

	// TODO: Build GUI using things we find in go meta.
	if( core.response.success(json_data) ){
	 
	    core.kvetch('We have "'+ core.response.type(json_data) +'" data.');

	    var ws = new org.bbop.amigo.workspace(json_data);
	    //global_workspace_list = ws.list_workspaces();
	    cart.update_workspaces(ws.list_workspaces());

	}else{
	    core.kvetch('We have a data FAILURE.');
	}
    }
    jQuery.ajax({
	type: "GET",
	url: core.api.workspace.status(), 
	dataType: 'json',
 	success: _populate_workspace_list,
 	error: function (result, status, error) {
 	    alert('Failed server request for workspaces: ' + status); 
 	} 
    });    


    ///
    /// Get the layout ready.
    ///

    // Well, this seems to work...
    //jQuery("#test").datepicker();

    //layout = jQuery('body').layout({ applyDefaultStyles: true });
    core.kvetch('Apply layout...');
    layout = jQuery('body').layout({

	//
	applyDefaultStyles: true,

	// North is treated as an island. 
	north__applyDefaultStyles: false,
	north__closable: false,
	north__resizable: false,
	north__slidable: false,

	// South is treated as an island. 
	south__applyDefaultStyles: false,
	south__closable: false,
	south__resizable: false,
	south__slidable: false,

	// We'll need this in certain cases (like map resizing because
	// it's actually donw through CSS).
	// Attach listeners to the center area. TODO: this will be
	// split out into the navi bits later on. For now, just a
	// test.	
	center__onresize: function(){
	    // Apparently, center is not completed yet, so we compute
	    // from all of the compass points.
	    var ymax = layout.state.container.innerHeight;
	    ymax = ymax - layout.state.north.size;
	    ymax = ymax - layout.state.south.size;

	    var xmax = layout.state.container.innerWidth;
	    //xmax = xmax - layout.state.west.size;
	    //xmax = xmax - layout.state.east.size;

	    core.kvetch('_resize_xy_:' + xmax + ':' + ymax);
	},

	//west__showOverflowOnHover: true,

	// East (workspaces) starts closed...
	east__initClosed: true
	// TODO: remove annoying padding.
	// 	east__applyDefaultStyles: false,
	// 	east__closable: true,
	// 	east__resizable: true,
	// 	east__slidable: true,
	// 	east__spacing_open: 0,
	// 	east__spacing_closed: 0
    });
    //core.kvetch('___' + layout);

    // Cart tabs. TODO: too big...
    core.kvetch('Apply tabs...');
    jQuery("#cart-tabs").tabs();
    jQuery("#search-tabs").tabs();
    jQuery("#results-tabs").tabs();
    jQuery("#search-tabs").tabs('select', 0);
    jQuery("#cart-tabs").tabs('select', 1);

    widgets = new org.bbop.amigo.ui.widgets();

    core.kvetch('HeavyClient init completed.');
    //core.kvetch('Contacting server for meta-information.');

    // Pull in GO meta info.
    var ontology_data = gm.ontologies();
    var species_data = gm.species();
    var source_data = gm.sources();
    var gptype_data = gm.gp_types();

    var evidence_data = gm.evidence_codes();
    var evcode_set = []; 
    for( var eci = 0; eci < evidence_data.length; eci++ ){
	var ekey = evidence_data[eci];
	evcode_set.push([ekey, ekey]); 
    }

    // Add a short-circut for the cart.
    var cart_short_circut_text =
	widgets.form.checkbox.front('cart-short-circut',
				    'cart-short-circut-name',
				    'use last cart',
				    true);
				    //false);
    jQuery("#cart-short-circut-div").append(cart_short_circut_text);

    ///
    /// Create forms and controls for term, gps, and associations.
    ///

    // The hidden count for all forms.
    var hidden_count_text = widgets.form.hidden_input('count', '10');
    
    // Clear the controls' area.
    _clear_app_forms();

    // Create the new form for terms.
    var hidden_mode_term_search_text =
	widgets.form.hidden_input('mode', 'live_search_term');
    var query_term_text =
	widgets.form.text_input('term-query','term-query',25,'Search GO');
    var ontology_text =
	widgets.form.multiselect('ontology','ontology',4,
				 ontology_data,'Ontology');
    jQuery("#app-form-term").append(hidden_mode_term_search_text);
    jQuery("#app-form-term").append(hidden_count_text);
    jQuery("#app-form-term-query").append(query_term_text);
    jQuery("#app-form-term-filters").append(ontology_text);

    // Create the new form for gps.
    var hidden_mode_gp_search_text =
	widgets.form.hidden_input('mode', 'live_search_gene_product');
    var query_gp_text =
	widgets.form.text_input('gp-query','gp-query',25,'Search GO');
    var gptype_text =
	widgets.form.multiselect('gptype','gptype',4,gptype_data,'GP type');
    var species_text =
	widgets.form.multiselect('species','species',4,species_data,'Species');
    var source_text =
	widgets.form.multiselect('source','source',4,source_data,'Data source');
    jQuery("#app-form-gp").append(hidden_mode_gp_search_text);
    jQuery("#app-form-gp").append(hidden_count_text);
    jQuery("#app-form-gp-query").append(query_gp_text);
    jQuery("#app-form-gp-filters").append(gptype_text);
    jQuery("#app-form-gp-filters").append(species_text);
    jQuery("#app-form-gp-filters").append(source_text);

    // Create the new form for associations.
    var hidden_mode_assoc_search_text =
	widgets.form.hidden_input('mode', 'live_search_association');
    var query_assoc_text =
	widgets.form.text_input('assoc-query', 'assoc-query', 25, 'Search GO');
    var assoc_ontology_text =
	widgets.form.multiselect('assoc-ontology', 'ontology', 4,
				 ontology_data, 'Ontology');
    var assoc_gptype_text =
	widgets.form.multiselect('assoc-gptype', 'gptype', 4,
				 gptype_data, 'GP type');
    var assoc_species_text =
	widgets.form.multiselect('assoc-species', 'species', 4,
				 species_data, 'Species');
    var assoc_source_text =
	widgets.form.multiselect('assoc-source', 'source', 4,
				 source_data, 'Data source');
    var assoc_evidence_text =
	widgets.form.multiselect('assoc-evidence', 'evidence', 4,
				 evcode_set, 'Evidence (alpha)');
    jQuery("#app-form-assoc").append(hidden_mode_assoc_search_text);
    jQuery("#app-form-assoc").append(hidden_count_text);
    jQuery("#app-form-assoc-query").append(query_assoc_text);
    jQuery("#app-form-assoc-filters").append(assoc_ontology_text);
    jQuery("#app-form-assoc-filters").append(assoc_gptype_text);
    jQuery("#app-form-assoc-filters").append(assoc_species_text);
    jQuery("#app-form-assoc-filters").append(assoc_source_text);
    jQuery("#app-form-assoc-filters").append(assoc_evidence_text);

    function _generate_action_to_server(marshaller, do_results, query_id, type){
	return function(event){

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

		// And...um... convert gp-query to the correct query.
		var all_inputs = marshaller();
		all_inputs['query'] = all_inputs[query_id];

		// Cut down on overhead a little.
		if( all_inputs &&
		    all_inputs['query'] &&
		    all_inputs['query'][0] &&
		    all_inputs['query'][0].length >= 3 ){

		    // Increment packet (async ordering).
		    all_inputs['packet'] = last_sent_packet++;

		    var url = null;
		    if( type == 'association' ){
			url = core.api.live_search.association(all_inputs);
		    }else if( type == 'gene_product' ){
			url = core.api.live_search.gene_product(all_inputs);
		    }else if( type == 'term' ){
			url = core.api.live_search.term(all_inputs);
		    }else{
			core.kvetch('ERROR: no good type, ready to die...');
		    }

		    core.kvetch('try: ' + url);		    
		    widgets.start_wait('Updating...');

		    jQuery.ajax({
	    		type: "GET",
	    		url: url,
	    		dataType: 'json',
	    		success: do_results,
	    		error: function (result, status, error) {
	    		core.kvetch('Failed server request ('+
				    query_id + '): ' + status);
			    widgets.finish_wait();
	    		}
		    });
		}
	    }
	};
    }

    // Create our callback function for this case.
    var marshal_assoc_form = 
	widgets.form.create_jquery_marshal('#app-form-assoc',
					   ['input', 'option:selected']);
    var marshal_gp_form = 
	widgets.form.create_jquery_marshal('#app-form-gp',
					   ['input', 'option:selected']);
    var marshal_term_form = 
	widgets.form.create_jquery_marshal('#app-form-term',
					   ['input', 'option:selected']);

    var assoc_saction = _generate_action_to_server(marshal_assoc_form,
						   _process_assoc_results,
						   'assoc-query','association');
    var gp_saction = _generate_action_to_server(marshal_gp_form,
						_process_gp_results,
						'gp-query', 'gene_product');
    var term_saction = _generate_action_to_server(marshal_term_form,
						  _process_term_results,
						  'term-query', 'term');
    
    // Attach listeners to the assoc form.
    jQuery("#assoc-query").keyup(assoc_saction);
    jQuery("#assoc-ontology").change(assoc_saction);
    jQuery("#assoc-gptype").change(assoc_saction);
    jQuery("#assoc-species").change(assoc_saction);
    jQuery("#assoc-source").change(assoc_saction);
    jQuery("#assoc-evidence").change(assoc_saction);

    // Attach listeners to the gp form.
    jQuery("#gp-query").keyup(gp_saction);
    jQuery("#gptype").change(gp_saction);
    jQuery("#species").change(gp_saction);
    jQuery("#source").change(gp_saction);

    // Attach listeners to the term form.
    jQuery("#term-query").keyup(term_saction);
    jQuery("#ontology").change(term_saction);

    // Make the forms unsubmitable.
    jQuery("#app-form-assoc").submit(function(){return false;});
    jQuery("#app-form-gp").submit(function(){return false;});
    jQuery("#app-form-term").submit(function(){return false;});
}


///
/// Helper functions that should be rolled into a new GUI generation
/// object at some point.
///


//
function _clear_app_forms(){

    jQuery("#app-form-assoc-query").empty();
    jQuery("#app-form-assoc-filters").empty();

    jQuery("#app-form-gp-query").empty();
    jQuery("#app-form-gp-filters").empty();

    jQuery("#app-form-term-query").empty();
    jQuery("#app-form-term-filters").empty();
}


///
/// Results processing.
///


// Convert the return JSON results into something usable...
// Include link arrows to page the results.
function _process_meta_results (data, type, args){

    // Grab meta information.
    var total = data.total();
    var first = data.first();
    var last = data.last();
    var meta_cache = new Array();
    meta_cache.push('[total: ' + total);
    meta_cache.push(', first: ' + first);
    meta_cache.push(', last: ' + last);
    meta_cache.push(']<br />');

    // Our element ids.
    var backward_id = 'bak_paging_id_' + core.util.randomness(10);
    var forward_id = 'for_paging_id_' + core.util.randomness(10);

    // Determine which arguments we'll (or would) need to page
    // forwards or backwards.
    var b_args = null;
    var f_args = null;
    b_args = core.util.clone(args);
    if( ! b_args['index'] ){ b_args['index'] = 2; }
    b_args['index'] = parseInt(b_args['index']) - 1;
    f_args = core.util.clone(args);
    if( ! f_args['index'] ){ f_args['index'] = 1; }
    f_args['index'] = parseInt(f_args['index']) + 1;

    // Increment packet (async ordering).
    b_args['packet'] = last_sent_packet++;
    f_args['packet'] = last_sent_packet++;

    // Determine which results processor and urls we'll (or would) use
    // for the binding(s).
    var proc = null
    var backward_url = null;
    var forward_url = null;
    if( type == 'association' ){
	proc = _process_assoc_results;
	backward_url = core.api.live_search.association(b_args);
	forward_url = core.api.live_search.association(f_args);
    }else if( type == 'gene_product' ){
	proc = _process_gp_results;
	backward_url = core.api.live_search.gene_product(b_args);
	forward_url = core.api.live_search.gene_product(f_args);
    }else if( type == 'term' ){
	proc = _process_term_results;
	backward_url = core.api.live_search.term(b_args);
	forward_url = core.api.live_search.term(f_args);
    }else{
	core.kvetch('ERROR: no good type in meta, ready to die...');
    }

    // Generate the necessary paging html.
    if( first > 1 ){
	meta_cache.push(' <a href="#" id="' + backward_id + '"><- back</a>');
    }
    if( last < total ){
	meta_cache.push(' <a href="#" id="' + forward_id + '">forward -></a>');
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


// Convert the return JSON results into something usable...
function _process_term_results (json_data, status){

    var find_class = 'term_results_id_' + core.util.randomness(10);
    
    core.kvetch('checking results...');
    
    // Verify the incoming data. Only go if we got something.
    if( core.response.success(json_data) &&
	core.response.type(json_data) == 'live_search_term' ){
	core.kvetch('term results okay...');
	
	// Packet order checking.
	var in_args = core.response.arguments(json_data);
	var our_packet = parseInt(in_args.packet);
	core.kvetch("packet: "+ our_packet +" (>? "+ last_received_packet +")");
	if( our_packet && our_packet > last_received_packet ){

	    core.kvetch("order usable packet: "+ our_packet);

	    // Set last received.
	    last_received_packet = our_packet;

	    // Okay, get the results data.
	    var ls = new org.bbop.amigo.live_search(json_data);	
	    var struct = ls.results();

	    // Bulk change.
	    var cache = new Array();
	    cache.push('<table>');
	    cache.push('<thead><tr>');
	    cache.push('<th>score</th>');
	    cache.push('<th>add</th>');
	    cache.push('<th>acc</th>');
	    cache.push('<th>name</th>');
	    cache.push('<th>ontology</th>');
	    cache.push('<th>synonym(s)</th>');
	    cache.push('</tr></thead><tbody>');
	    for( var i = 0; i < struct.length; i++ ){

		var r = struct[i];
		var encoded_acc = coder.encode(r.acc);

		// Create HTML.
		if( i % 2 == 0 ){
		    cache.push('<tr class="odd_row">');
		}else{
		    cache.push('<tr class="even_row">');
		}

		//
		cache.push('<td>');
		cache.push(r.score + '%');
		cache.push('</td>');

		//
		cache.push('<td>');
		cache.push('<a id="');
		//cache.push(coder.encode(r.acc));
		cache.push(encoded_acc);
		cache.push('" class="' + find_class + '" title="Save ');
		cache.push(r.acc);
		cache.push(' to cart" href="#">');
		cache.push('<img width="30px" height="20px" src="');
		cache.push(gm.image_base() + '/cart.png');
		cache.push('" alt="Add to your cart." title="AmiGO cart" />');
		cache.push('</a>');
		cache.push('</td>');

		cache.push('<td>');
		cache.push('<a title="link to information on ' + r.acc +
			   '" href=\"' + r.link +
			   '">' + r.hilite_acc +
			   '</a>');
		cache.push('</td>');

		cache.push('<td>');
		cache.push(r.hilite_name);
		cache.push('</td>');
		cache.push('<td>');
		cache.push(r.hilite_ontology);
		//cache.push('n/a');
		cache.push('</td>');
		cache.push('<td>');
		//cache.push(r.hilite_synonym);
		cache.push(r.hilite_synonym.replace(newline_finder, ", "));
		cache.push('</td>');
		cache.push('</tr>');

		// Add to cartspace.
		cart.clear_map(encoded_acc);
		cart.add_map(encoded_acc, new org.bbop.amigo.item(r.acc, ''));
	    }
	    cache.push('</tbody></table>');
	    // Set div text.
	    jQuery('#results').html('<p>' + cache.join('') + '</p>');

	    _process_meta_results(ls,'term',core.response.arguments(json_data));

	    //_workspace_bindings(find_class, 'term');
	    //cart.bind(find_class, 'term');
	    cart.bind(find_class);
	}else{
	    core.kvetch("dropping packet");
	}
    }
    widgets.finish_wait();
}


// Convert the return JSON results into something usable...
function _process_gp_results (json_data, status){

    var find_class = 'gp_results_id_' + core.util.randomness(10);

    core.kvetch('checking results...');

    // Verify the incoming data. Only go if we got something.
    if( core.response.success(json_data) &&
	core.response.type(json_data) == 'live_search_gene_product' ){
	core.kvetch('gp results okay...');

	// Packet order checking.
	var in_args = core.response.arguments(json_data);
	var our_packet = parseInt(in_args.packet);
	core.kvetch("packet: "+ our_packet +" (>? "+ last_received_packet +")");
	if( our_packet && our_packet > last_received_packet ){

	    core.kvetch("order usable packet: "+ our_packet);

	    // Set last received.
	    last_received_packet = our_packet;

	    // Okay, get the results data.
	    var ls = new org.bbop.amigo.live_search(json_data);
	    var struct = ls.results();

	    var species_map = gm.species_map();

	    // Bulk change.
	    var cache = new Array();
	    cache.push('<table>');
	    cache.push('<thead><tr>');
	    cache.push('<th>score</th>');
	    cache.push('<th>add</th>');
	    cache.push('<th>acc</th>');
	    cache.push('<th>symbol</th>');
	    cache.push('<th>name</th>');
	    cache.push('<th>gptype</th>');
	    cache.push('<th>source</th>');
	    cache.push('<th>species</th>');
	    cache.push('<th>synonym(s)</th>');
	    cache.push('</tr></thead><tbody>');
	    for( var i = 0; i < struct.length; i++ ){

		var r = struct[i];
		var encoded_acc = coder.encode(r.dbxref);

		// Create HTML.
		if( i % 2 == 0 ){
		    cache.push('<tr class="odd_row">');
		}else{
		    cache.push('<tr class="even_row">');
		}

		//
		cache.push('<td>');
		cache.push(r.score + '%');
		cache.push('</td>');

		//
		cache.push('<td>');
		cache.push('<a id="');
		cache.push(encoded_acc);
		cache.push('" class="' + find_class + '" title="Save ');
		cache.push(r.dbxref);
		cache.push(' to cart" href="#">');
		cache.push('<img width="30px" height="20px" src="');
		cache.push(gm.image_base() + '/cart.png');
		cache.push('" alt="Add to your cart." title="AmiGO cart" />');
		cache.push('</a>');
		cache.push('</td>');

		//
		cache.push('<td>');
		cache.push('<a title="link to information on ' + r.dbxref +
			   '" href=\"' + r.link +
			   '">' + r.hilite_dbxref +
			   '</a>');
		cache.push('</td>');
		cache.push('<td>');
		cache.push(r.hilite_symbol);
		cache.push('</td>');
		cache.push('<td>');
		cache.push(r.hilite_full_name);
		cache.push('</td>');
		cache.push('<td>');
		cache.push(r.hilite_gptype);
		cache.push('</td>');
		cache.push('<td>');
		cache.push(r.hilite_source);
		cache.push('</td>');
		// Simple names aren't split, complicated ones are.
		var s_name = species_map[r.species];
		if( s_name && s_name.split(' ').length <= 2 ){
		    cache.push('<td class="nowrap">');
		}else{
		    cache.push('<td class="">');
		}
		cache.push(species_map[r.species]);
		cache.push('</td>');
		cache.push('<td>');
		//cache.push(r.hilite_synonym);
		cache.push(r.hilite_gpsynonym.replace(newline_finder, ", "));
		cache.push('</td>');
		cache.push('</tr>');

		// Add to cartspace.
		cart.clear_map(encoded_acc);
		cart.add_map(encoded_acc, new org.bbop.amigo.item(r.dbxref,''));
	    }
	    cache.push('</tbody></table>');

	    // Set div text.
	    jQuery('#results').html('<p>' + cache.join('') + '</p>');

	    _process_meta_results(ls, 'gene_product',
				  core.response.arguments(json_data));

	    //_workspace_bindings(find_class, 'gene_product');
	    //cart.bind(find_class, 'gene_product');
	    cart.bind(find_class);
	}else{
	    core.kvetch("dropping packet");
	}
    }
    widgets.finish_wait();
}


// Convert the return JSON results into something usable...
function _process_assoc_results (json_data, status){

    var find_class = 'assoc_results_id_' + core.util.randomness(10);

    core.kvetch('checking results...');

    // Verify the incoming data. Only go if we got something.
    if( core.response.success(json_data) &&
	core.response.type(json_data) == 'live_search_association' ){
	core.kvetch('assoc results okay...');

	// Packet order checking.
	var in_args = core.response.arguments(json_data);
	var our_packet = parseInt(in_args.packet);
	core.kvetch("packet: "+ our_packet +" (>? "+ last_received_packet +")");
	if( our_packet && our_packet > last_received_packet ){

	    core.kvetch("order usable packet: "+ our_packet);

	    // Set last received.
	    last_received_packet = our_packet;

	    // Okay, get the results data.
	    var ls = new org.bbop.amigo.live_search(json_data);
	    var struct = ls.results();

	    var species_map = gm.species_map();

	    // Bulk change.
	    var cache = new Array();
	    cache.push('<table>');
	    cache.push('<thead><tr>');

	    //cache.push('<th>cart</th>');
	    cache.push('<th>score</th>');

	    cache.push('<th>evidence</th>');

	    cache.push('<th>add</th>');

	    cache.push('<th>acc</th>');
	    cache.push('<th>name</th>');
	    cache.push('<th>ontology</th>');
	    cache.push('<th>synonym(s)</th>');

	    cache.push('<th>add</th>');

	    cache.push('<th>dbxref</th>');
	    cache.push('<th>symbol</th>');
	    cache.push('<th>full_name</th>');
	    cache.push('<th>gptype</th>');
	    cache.push('<th>source</th>');
	    cache.push('<th>species</th>');
	    cache.push('<th>gpsynonym(s)</th>');

	    cache.push('</tr></thead><tbody>');
	    for( var i = 0; i < struct.length; i++ ){

		var r = struct[i];
		var encoded_acc = coder.encode(r.acc);
		var encoded_dbxref = coder.encode(r.dbxref);

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

		// Evidence.
		cache.push('<td>');
		cache.push(r.evidence);
		cache.push('</td>');
		
		// Term cart.
		cache.push('<td>');
		cache.push('<a id="');
		cache.push(encoded_acc);
		cache.push('" class="' + find_class + '" title="Save ');
		cache.push(r.acc);
		cache.push(' to cart" href="#">');
		cache.push('<img width="30px" height="20px" src="');
		cache.push(gm.image_base() + '/cart.png');
		cache.push('" alt="Add to your cart." title="AmiGO cart" />');
		cache.push('</a>');
		cache.push('</td>');

		// Term section.
		cache.push('<td>');
		cache.push('<a title="link to information on ' + r.acc +
			   '" href=\"' + r.term_link +
			   '">' + r.hilite_acc +
			   '</a>');
		cache.push('</td>');
		cache.push('<td>');
		cache.push(r.hilite_name);
		cache.push('</td>');
		cache.push('<td>');
		cache.push(r.hilite_ontology);
		//cache.push('n/a');
		cache.push('</td>');
		cache.push('<td>');
		//cache.push(r.hilite_synonym);
		cache.push(r.hilite_synonym.replace(newline_finder, ", "));
		cache.push('</td>');

		// GP cart.
		cache.push('<td>');
		cache.push('<a id="');
		cache.push(encoded_dbxref);
		cache.push('" class="' + find_class + '" title="Save ');
		cache.push(r.dbxref);
		cache.push(' to cart" href="#">');
		cache.push('<img width="30px" height="20px" src="');
		cache.push(gm.image_base() + '/cart.png');
		cache.push('" alt="Add to your cart." title="AmiGO cart" />');
		cache.push('</a>');
		cache.push('</td>');

		// GP section.
		cache.push('<td>');
		cache.push('<a title="link to information on ' + r.dbxref +
			   '" href=\"' + r.gene_product_link +
			   '">' + r.hilite_dbxref +
			   '</a>');
		cache.push('</td>');
		cache.push('<td>');
		cache.push(r.hilite_symbol);
		cache.push('</td>');
		cache.push('<td>');
		cache.push(r.hilite_full_name);
		cache.push('</td>');
		cache.push('<td>');
		cache.push(r.hilite_gptype);
		cache.push('</td>');
		cache.push('<td>');
		cache.push(r.hilite_source);
		cache.push('</td>');
		// Simple names aren't split, complicated ones are.
		var s_name = species_map[r.species];
		if( s_name && s_name.split(' ').length <= 2 ){
		    cache.push('<td class="nowrap">');
		}else{
		    cache.push('<td class="">');
		}
		cache.push(species_map[r.species]);
		cache.push('</td>');
		cache.push('<td>');
		//cache.push(r.hilite_synonym);
		cache.push(r.hilite_gpsynonym.replace(newline_finder, ", "));
		cache.push('</td>');
		cache.push('</tr>');

		// Add to cartspace.
		cart.clear_map(encoded_acc);
		cart.clear_map(encoded_dbxref);
		cart.add_map(encoded_acc,
			     new org.bbop.amigo.item(r.acc,''));
		cart.add_map(encoded_dbxref,
			     new org.bbop.amigo.item(r.dbxref,''));
	    }
	    cache.push('</tbody></table>');

	    // Set div text.
	    jQuery('#results').html('<p>' + cache.join('') + '</p>');

	    _process_meta_results(ls, 'association',
				  core.response.arguments(json_data));

	    //_workspace_bindings(find_class, 'gene_product');
	    //cart.bind(find_class, 'gene_product');
	    cart.bind(find_class);
	}else{
	    core.kvetch("dropping packet");
	}
    }
    widgets.finish_wait();
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
	    	    core.kvetch('Failed server request: ' + status);
		    widgets.finish_wait();
	    	}
	    });
	});
    }
}
