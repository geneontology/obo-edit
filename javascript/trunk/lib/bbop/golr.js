/* 
 * Package: golr.js
 * Namespace: bbop.golr
 * 
 * Generic BBOP manager for dealing with gross GOlr configuration
 * and management. Remember, this is actually a "subclass" of
 * bbop.registry.
 */

// Setup the internal requirements.
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'registry');
bbop.core.namespace('bbop', 'golr');

// Thinking about lessons learned from solr ajax.
// Updatable model that connects to the Solr server.
// Makes no attempt to join to a form--entirely held as an internal model.
// {url: 'http://theplace', facets: ['foo', 'bar']}

// This should act as a model--since we start with a completely open
// query (whether we display it or not), we will have all possible
// facets and can build the initial model off of that.

/*
 * Structure: bbop.golr.configuration
 * Constructor: manager
 * 
 * Contructor for the GOlr query manager
 * 
 * Arguments:
 *  golr_loc - string url to GOlr server;
 *  golr_conf_var - JSONized GOlr config
 * 
 * Returns: golr object
 * 
 * Also See: <bbop.registry>
 */
bbop.golr.manager = function (golr_loc, golr_conf_var){
    // We are a registry like this:
    bbop.registry.call(this, ['reset', 'search', 'error']);
    this._is_a = 'bbop.golr.manager';

    // Get a good self-reference point.
    var anchor = this;

    // Per-manager logger.
    var logger = new bbop.logger(this._is_a);
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    // AmiGO helper.
    //var amigo = new bbop.amigo();
    //var golr = amigo.golr_response;

    // TODO: Block requests from the past from haunting us.
    this.last_sent_packet = 0;
    this.last_received_packet = 0;

    // Lightly check incoming arguments.
    // There should be a string url argument.
    // There could be a hash of pinned filters argument.
    if( ! golr_loc || ! golr_conf_var ){
	ll('ERROR: no proper arguments');
    }
    if( typeof golr_loc != 'string' ){
	ll('ERROR: no proper golr url string argument');
    }
    if( typeof golr_conf_var != 'object' ){
	ll('ERROR: no proper golr conf var argument');
    }
    
    // Our default target url.
    this._solr_url = golr_loc;

    // Settle in the configurations.
    this._golr_conf = new bbop.golr.configuration(golr_conf_var);

    // // Get the aspects.
    // this._search_apsects_hash = {};
    // //bbop.core.each();
    
    ///
    /// BUG/TODO: Below still needs to be reworked.
    ///

    // Our default query args, with facet fields plugged in.
    this.query_invariants =
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
//	    'facet.field': in_args['facets'],
	    // TODO: 'json.nl': [flat|map|arrarr]
	    'json.nl': 'arrarr'

	    // Static facet filtering.

	    // For restricting ourselves to a certain part if the
	    // index as an initial condition.
//	    fq: in_args['filters']

	    // // Fixed UI location.
	    // NOTE: punted to UI object.
	    // interface_id: this.interface_id

	    // Query-type stuff is variant--see update and
	    // query_variants.
	    //q: '*:*', // start by going after everything
	};
    
    // The callback function called after a successful AJAX
    // intialization/reset call. First it runs some template code,
    // then it does all of the callbacks.
    this._run_reset_callbacks = function(json_data){
	ll('run reset callbacks...');
	anchor.apply_callbacks('reset', [json_data]);
    };

    // The main callback function called after a successful AJAX call in
    // the update function. First it runs some template code, then it does
    // all of the callbacks.
    this._run_search_callbacks = function(json_data){
	ll('run search callbacks...');
	anchor.apply_callbacks('search', [json_data]);
    };

    // This is the function that runs where there is an AJAX error
    // during an update. First it runs some template code, then it
    // does all of the callbacks.
    this._run_error_callbacks = function(result, status, error) {

	ll('Failed server request: '+ result +', '+ status +', '+ error);
		
	// Get the error out if possible.
	var jreq = result.responseText;
	var req = jQuery.parseJSON(jreq);
	if( req && req['errors'] && req['errors'].length > 0 ){
	    var in_error = req['errors'][0];
	    ll('ERROR:' + in_error);
	    // Split on newline if possible to get
	    // at the nice part before the perl
	    // error.
	    var reg = new RegExp("\n+", "g");
	    var clean_error_split =
		in_error.split(reg);
	    var clean_error = clean_error_split[0];
	}
	
	// Run all against registered functions.
	ll('run error callbacks...');
	anchor.apply_callbacks('error', [clean_error]);
    };
    var _run_error_callbacks = this._run_error_callbacks;

    // Try and decide between a reset callback and a search callback.
    function _callback_type_decider(json_data){
    	ll('in callback type decider...');

    	// 
    	if( ! golr.success(json_data) ){
    	    throw new Error("Unsuccessful response from golr server!");
    	}else{
    	    var cb_type = golr.callback_type(json_data);
    	    ll('okay response from server, will probe type...: ' + cb_type);
    	    if( cb_type == 'reset' ){
    		anchor._run_reset_callbacks(json_data);
    	    }else if( cb_type == 'search' ){
    		anchor._run_search_callbacks(json_data);
    	    }else{
    		throw new Error("Unknown callback type!");
    	    }
    	}
    };

    // The user code to select the type of update (and thus the type
    // of callbacks to be called on data return).
    this.update = function(update_type, logic_hash){

	// Our bookkeeping--increment packet.
	anchor.last_sent_packet = anchor.last_sent_packet + 1;
	
	// Necessary variants.
	var query_variants = {
	    packet: anchor.last_sent_packet,
	    callback_type: update_type
	};

	// Structure of the necessary invariant parts.	
	var qs_head = anchor._solr_url + 'select?';
	var invariant_qs = bbop.core.get_assemble(anchor.query_invariants);
	var qurl = qs_head + invariant_qs;

	// Conditional merging of the remaining variant parts.
	if( update_type == 'reset' ){

	    // Reset and do completely open query.
	    var variant_qs = bbop.core.get_assemble(query_variants);
	    ll('varient_qs: ' + variant_qs);
	    qurl = qurl + '&' + variant_qs + '&q=*:*';

	}else if( update_type == 'search' ){

	    // NOTE/TODO: a lot of previous wacky q handling was done
	    // in perl on the server, some of that will probably have
	    // to be ported over to JS around here.

	    // NOTE/TODO: Make this work well enough until we get
	    // dismax working properly.
	    var query_string = '*:*';
	    if( logic_hash && logic_hash['q'] ){
		var q_logic = logic_hash['q'];
		var str_rep = q_logic.to_string();

		if( str_rep.length > 0 ){
		    query_string = 'label:' + str_rep +
			' OR annotation_class_label:' + str_rep;
		}
	    }

	    // NOTE/TODO: Assemble filters from logic. Make clean for
	    // URLs.
	    var filter_qs = '';
	    if( logic_hash && logic_hash['fq'] ){
		var fq_logic = logic_hash['fq'];
		var str_rep = fq_logic.to_string();	    

		if( str_rep.length > 0 ){
		    filter_qs = '&fq=' + str_rep;
		}
	    }

	    // Finalize it.
	    var variant_qs = bbop.core.get_assemble(query_variants);
	    //ll('varient_qs: ' + variant_qs);
	    qurl = qurl + '&' + variant_qs + filter_qs + '&q=' + query_string;

	}else{
	    throw new Error("Unknown update_type: " + update_type);
	}

	ll('try: ' + qurl);
	//widgets.start_wait('Updating...');

	// TODO/BUG: JSONP for solr looks like?
	var argvars = {
	    type: "GET",
	    url: qurl,
	    dataType: 'json',
	    jsonp: 'json.wrf',
	    success: _callback_type_decider,
	    error: _run_error_callbacks
	};
	//jQuery.ajax(argvars);
	return argvars;
    };

    // Trigger the "reset" chain of events.
    this.reset = function(){
	anchor.update('reset', null);
    };

    // Trigger the "search" chain of events.
    // Takes a field-keyed hash of bbop.logics as an argument.
    this.search = function(logics){
	anchor.update('search', logics);
    };
};
bbop.golr.prototype = new bbop.registry;

/*
 * Structure: bbop.golr.configuration
 * Constructor: manager
 * 
 * Contructor for the GOlr query manager.
 * 
 * Arguments:
 *  golr_conf_var - JSONized GOlr config
 * 
 * Returns: golr object
 * 
 * Also See: <bbop.registry>
 */
bbop.golr.configuration = function (golr_conf_var){
    // // We are a registry like this:
    //bbop.registry.call(this, ['reset', 'search', 'error']);
    this._is_a = 'bbop.golr.configuration';

    // Get a good self-reference point.
    var anchor = this;

    // Per-manager logger.
    var logger = new bbop.logger(this._is_a);
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    // Lightly check incoming arguments.
    // There could be a hash of pinned filters argument.
    if( ! golr_conf_var || typeof golr_conf_var != 'object' ){
	ll('ERROR: no proper golr conf var argument');
    }
    
    // Settle in the configurations.
    this._golr_conf = golr_conf_var;

    // Get the search aspects.
    this._aspects = {};
    bbop.core.each(anchor._golr_conf,
		  function(key, val){
		      var new_asp = new bbop.golr.search_aspect(val);
		      anchor._aspects[new_asp.id()] = new_asp;
		  });

    /*
     * Function: get_aspect
     * 
     * Returns a search aspect by id string. Null otherwise.
     * 
     * Returns: bbop.search_aspect.
     */
    this.get_aspect = function(fid){
	retval = null;
	if( this._aspects &&
	    this._aspects[fid] ){
		retval = this._aspects[fid];
	    }
	return retval;
    };

    /*
     * Function: get_aspects
     * 
     * Returns an array of all search aspects.
     * 
     * Returns: Array of bbop.golr.search_aspect.
     */
    this.get_aspects = function(){
	ret = [];
	bbop.core.each(anchor._aspects,
		       function(key, val){
			   ret.push(val);
		       });
	return ret;
    };

    /*
     * Function: get_visible_aspect_ids
     * 
     * Returns an array of all visible search aspects by id string,
     * ordered by weight.
     * 
     * Returns: Array of strings.
     */
};

/*
 * Structure: bbop.golr.search_aspect
 * Constructor: search_aspect
 * 
 * Contructor for a GOlr search aspect.
 * 
 * Arguments:
 *  aspect_conf_struct - JSONized config
 * 
 * Returns: search_aspect object
 */
bbop.golr.search_aspect = function (aspect_conf_struct){
    // We are a registry like this:
    //bbop.registry.call(this, ['reset', 'search', 'error']);
    this._is_a = 'bbop.golr.search_aspect';

    // Get a good self-reference point.
    var anchor = this;

    // Per-manager logger.
    var logger = new bbop.logger(this._is_a);
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    // Capture aspect and the component fields into variables.
    this._aspect = aspect_conf_struct;
    this._fields = {};
    bbop.core.each(this._aspect['fields'],
		   function(item, index){
		       var sf = new bbop.golr.search_field(item);
		       anchor._fields[sf.id()] = sf;
		  });

    /*
     * Function: display_name
     * 
     * The user-facing display name. Suitable for label or title
     * somewhere.
     * 
     * Returns: Display name string.
     */
    this.display_name = function(){
	return this._aspect['display_name'];
    };

    /*
     * Function: description
     * 
     * A longer description. Suitable for tooltips.
     * 
     * Returns: Description string.
     */
    this.description = function(){
	return this._aspect['description'];
    };

    /*
     * Function: weight
     * 
     * The relative weight of this search aspect.
     * 
     * Returns: Integer.
     */
    this.weight = function(){
	return parseInt(this._aspect['weight']) || 0;
    };

    /*
     * Function: id
     * 
     * The unique ID of this profile.
     * 
     * Returns: String.
     */
    this.id = function(){
	return this._aspect['id'];
    };

    /*
     * Function: searchable_extension
     * 
     * ???
     * 
     * Returns: String.
     */
    this.searchable_extension = function(){
	return this._aspect['searchable_extension'] || '_searchable';
    };

    /*
     * Function: get_field
     * 
     * Returns a search field by id string. Null otherwise.
     * 
     * Returns: bbop.search_field.
     */
    this.get_field = function(fid){
	retval = null;
	if( this._fields &&
	    this._fields[fid] ){
		retval = this._fields[fid];
	    }
	return retval;
    };

    // /*
    //  * Function: get_visible_fields
    //  * 
    //  * Returns an array of all visible search field by id string. Null otherwise.
    //  * 
    //  * Returns: bbop.search_field.
    //  */
    // this.get_visible_fields = function(){
    // 	retval = null;
    // 	if( this._fields &&
    // 	    this._fields[fid] ){
    // 		retval = this._fields[fid];
    // 	    }
    // 	return retval;
    // };
};

/*
 * Structure: bbop.golr.search_field
 * Constructor: search_field
 * 
 * Contructor for a GOlr search field.
 * 
 * Arguments:
 *  field_conf_struct - JSONized config.
 * 
 * Returns: search_field object
 */
bbop.golr.search_field = function (field_conf_struct){
    // We are a registry like this:
    //bbop.registry.call(this, ['reset', 'search', 'error']);
    this._is_a = 'bbop.golr.search_field';

    // Get a good self-reference point.
    var anchor = this;

    // Per-manager logger.
    var logger = new bbop.logger(this._is_a);
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    // Capture search fields.
    this._field = field_conf_struct;

    /*
     * Function: display_name
     * 
     * The user-facing display name. Suitable for label or title
     * somewhere.
     * 
     * Returns: Display name string.
     */
    this.display_name = function(){
	return this._field['display_name'];
    };

    /*
     * Function: description
     * 
     * A longer description. Suitable for tooltips.
     * 
     * Returns: Description string.
     */
    this.description = function(){
	return this._field['description'];
    };

    /*
     * Function: weight
     * 
     * The relative weight of this search field.
     * 
     * Returns: Integer.
     */
    this.weight = function(){
	return parseInt(this._field['weight']) || 0;
    };

    /*
     * Function: id
     * 
     * The unique ID of this profile.
     * 
     * Returns: String.
     */
    this.id = function(){
	return this._field['id'];
    };

    /*
     * Function: searchable
     * 
     * Returns whether or not a string field has a shadow
     * "*_searchable" field defined that is suitable for dismax
     * searches. Defaults to false.
     * 
     * Returns: Boolean.
     */
    this.searchable = function(){
	var retval = false;
	if( this._field['searchable'] == 'true' ||
	    this._field['searchable'] == true ){
		retval = true;	
	    }
	return retval;
    };

    /*
     * Function: required
     * 
     * Returns whether or not this field is required. Defaults to
     * false.
     * 
     * Not of particular use.
     * 
     * Returns: Boolean.
     */
    this.required = function(){
	var retval = false;
	if( this._field['required'] == 'true' ||
	    this._field['required'] == true ){
		retval = true;	
	    }
	return retval;
    };

    /*
     * Function: is_multi
     * 
     * Using the "cardinality" entry, returns whether or not this
     * field is "single" (false) or "multi" (true). Defaults to false.
     * 
     * Returns: Boolean.
     */
    this.is_multi = function(){
	var retval = false;
	if( this._field['cardinality'] == 'multi' ){
	    retval = true;	
	}
	return retval;
    };

    /*
     * Function: is_fixed
     * 
     * Using the "property_type" entry, returns whether or not this
     * field is "dynamic" (false) or "fixed" (true). Defaults to false.
     * 
     * Not of particular use.
     * 
     * Returns: Boolean.
     */
    this.is_fixed = function(){
	var retval = false;
	if( this._field['property_type'] == 'fixed' ){
	    retval = true;	
	}
	return retval;
    };

    /*
     * Function: property
     * 
     * Returns the method of this field's generation in the loader.
     * 
     * Not of particular use.
     * 
     * Returns: String.
     */
    this.property = function(){
	var retval = '???';
	if( this._field['property'] ){
	    retval = this._field['property'];
	}
	return retval;
    };

    // TODO: ...
};
