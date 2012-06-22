/* 
 * Package: golr_manager.js
 * Namespace: bbop.golr.manager
 * 
 * Generic BBOP manager for dealing with gross GOlr configuration
 * and management. Remember, this is actually a "subclass" of
 * bbop.registry.
 */

// Setup the internal requirements.
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'registry');
bbop.core.require('bbop', 'golr', 'conf');
//bbop.core.namespace('bbop', 'golr');
bbop.core.namespace('bbop', 'golr', 'manager');
bbop.core.namespace('bbop', 'golr', 'faux_ajax');

/*
 * Structure: bbop.golr.faux_ajax
 * Constructor: faux_ajax
 * 
 * Contructor for a fake and harmless Ajax.
 * 
 * Returns: faux_ajax object
 */
bbop.golr.faux_ajax = function (){
    this._is_a = 'bbop.golr.faux_ajax';

    /*
     * Function: ajax
     *
     * Fake call to jQuery's ajax.
     *
     * Parameters: 
     *  args - whatever
     *
     * Returns: null
     */
    this.ajax = function(args){
	return null;
    };
    /*
     * Function: parseJSON
     *
     * Fake call to jQuery's parseJSON.
     *
     * Parameters: 
     *  args - whatever
     *
     * Returns: ""
     */
    this.parseJSON = function(args){
	return "";
    };
};

// Thinking about lessons learned from solr ajax.
// Updatable model that connects to the Solr server.
// Makes no attempt to join to a form--entirely held as an internal model.
// {url: 'http://theplace', facets: ['foo', 'bar']}

// This should act as a model--since we start with a completely open
// query (whether we display it or not), we will have all possible
// facets and can build the initial model off of that.


/*
 * Structure: bbop.golr.manager
 * Constructor: manager
 * 
 * Contructor for the GOlr query manager
 * 
 * Arguments:
 *  golr_loc - string url to GOlr server;
 *  golr_conf_obj - a bbop.golr.conf object
 * 
 * Returns: golr manager object
 * 
 * Also See: <bbop.registry>
 */
bbop.golr.manager = function (golr_loc, golr_conf_obj){
//function GOlrManager(in_args){
    // We are a registry like this:
    bbop.registry.call(this, ['reset', 'search', 'error']);
    this._is_a = 'bbop.golr.manager';

    // Get a good self-reference point.
    var anchor = this;

    // Per-manager logger.
    var logger = new bbop.logger(this._is_a);
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    // Before anything else, if we cannot find a viable jQuery library
    // for use, we're going to create a fake one so we can still test
    // and work in a non-browser/networked environment.
    var JQ = new bbop.golr.faux_ajax();
    try{ // some interpreters might not like this kind of probing
	if( typeof jQuery != 'undefined' ){ JQ = jQuery; }
    }catch (x){
    }finally{
	var got = bbop.core.what_is(JQ);
	if( got && got == 'bbop.golr.faux_ajax'){
	}else{
	    got = 'jQuery';
	}
	ll('Using ' + got + ' for Ajax calls.');
    }

    // // AmiGO helper.
    // var amigo = new bbop.amigo();
    // var golr = amigo.golr_response;

    // TODO: Block requests from the past from haunting us.
    this.last_sent_packet = 0;
    this.last_received_packet = 0;

    // Lightly check incoming arguments.
    // There should be a string url argument.
    // There could be a hash of pinned filters argument.
    if( ! golr_loc || ! golr_conf_obj ){
	ll('ERROR: no proper arguments');
    }
    if( typeof golr_loc != 'string' ){
	ll('ERROR: no proper golr url string argument');
    }
    if( ! golr_conf_obj._is_a || golr_conf_obj._is_a != 'bbop.golr.conf' ){
	ll('ERROR: no proper bbop.golr.conf object argument');
    }
    
    // Our default target url.
    this._solr_url = golr_loc;

    // Settle in the configurations.
    // this._golr_conf = new bbop.golr.conf(golr_conf_var);
    this._golr_conf = golr_conf_obj;

    // Our default query args, with facet fields plugged in.
    this.query_variants =
	{
	    // Things unlikely to be touched.
	    // There are unlikely to be messed with too much.
	    qt: 'standard',
	    indent: 'on',
	    wt: 'json',
	    //version: '2.2',
	    rows: 10,
	    start: 0, // Solr is offset indexing
	    fl: '*%2Cscore',
    
	    // For restricting ourselves to a certain part if the
	    // index as an initial condition.
	    //	    fq: in_args['filters'],
	    fq: {},
	    
	    // // Fixed UI location.
	    // NOTE: punted to UI object.
	    // interface_id: this.interface_id
	    
	    // Query-type stuff is variant--see update and
	    // update_variants.
	    //	    q: '*:*' // start by going after everything

	    // Control of facets.
	    facet: 'true',
	    'facet.mincount': 1,
	    'json.nl': 'arrarr', // only in facets right now
	    // TODO?: 'facet.limit': 20,
	    // TODO?: 'f.???.facet.limit': 50,
	    // TODO: 'json.nl': [flat|map|arrarr]
	    // There are unlikely to be messed with too much.
	    'facet.field': []
	};

    // A little extra thing that we might need sometimes.
    this.query_extra = null;

    // The callback function called after a successful AJAX
    // intialization/reset cal. First it runs some template code, then it
    // does all of the callbacks.
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
	var req = JQ.parseJSON(jreq);
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

    /*
     * Function: update
     *
     * The user code to select the type of update (and thus the type
     * of callbacks to be called on data return).
     *
     * Parameters: 
     *  update_type - update type string
     *  logic_hash - (optional?) logic hash argument
     *
     * Returns: n/a
     */
    this.update = function(update_type, logic_hash){

	// Structure of the necessary invariant parts.	
	var qurl = anchor.get_query_url();

	// Our bookkeeping--increment packet.
	anchor.last_sent_packet = anchor.last_sent_packet + 1;
	
	// Necessary variants.
	var update_variants = {
	    packet: anchor.last_sent_packet,
	    callback_type: update_type
	};

	// Conditional merging of the remaining variant parts.
	if( update_type == 'reset' ){

	    // Reset and do completely open query.
	    ll('reset variant assembly');
	    var update_qs = bbop.core.get_assemble(update_variants);
	    ll('varient_qs: ' + update_qs);
	    //qurl = qurl + '&' + update_qs + '&q=*:*';
	    qurl = qurl + '&' + update_qs;

	}else if( update_type == 'search' ){

	    // NOTE/TODO: a lot of previous wacky q handling was done
	    // in perl on the server, some of that will probably have
	    // to be ported over to JS around here.
	    var query_string = '*:*';
	    if( logic_hash && logic_hash['q'] ){
		var q_logic = logic_hash['q'];
		var str_rep = q_logic.to_string();
		if( str_rep.length > 0 ){
		    // query_string = 'label:' + str_rep +
		    // 	' OR annotation_class_label:' + str_rep;
		    query_string = str_rep;
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
	    ll('final variant assembly');
	    var update_qs = bbop.core.get_assemble(update_variants);
	    //ll('varient_qs: ' + update_qs);
	    qurl = qurl + '&' + update_qs + filter_qs + '&q=' + query_string;

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
	JQ.ajax(argvars);
    };

    /*
     * Function: reset
     *
     * Trigger the "reset" chain of events.
     *
     * Returns: n/a
     */
    // Trigger the "reset" chain of events.
    this.reset = function(){
	anchor.update('reset', null);
    };

    /*
     * Function: search
     *
     * Trigger the "search" chain of events.
     * Takes a field-keyed hash of bbop.logics as an argument.
     *
     * Parameters: 
     *  logic_hash - (optional?) logic hash argument
     *
     * Returns: n/a
     */
    this.search = function(logic_hash){
	anchor.update('search', logic_hash);
    };

    /*
     * Function: facets
     *
     * Getter/setter for facets.
     *
     * Parameters: 
     *  list_or_key - TODO (optional)
     *  value - TODO (optional)
     *
     * Returns: the current facets hash.
     */
    this.facets = function(list_or_key){
	if( list_or_key ){
	    if( bbop.core.what_is(list_or_key) == 'array' ){
		anchor.query_variants['facet.field'] = list_or_key;
	    }else{
		anchor.query_variants['facet.field'].push(list_or_key);
	    }
	}
	return anchor.get('facet.field');
    };

    /*
     * Function: filters
     *
     * Getter/setter for filters.
     *
     * Parameters: 
     *  hash_or_key - TODO (optional)
     *  value - TODO (optional)
     *
     * Returns: the current filters hash.
     */
    this.filters = function(hash_or_key, value){
	if( value && hash_or_key ){
	    anchor.query_variants['fq'][hash_or_key] = value;
	}else if( hash_or_key ){
	    anchor.query_variants['fq'] = hash_or_key;
	}
	return anchor.get('fq');
    };

    /*
     * Function: extra
     *
     * Getter/setter for the internal string variable to be appended
     * to the end. For special use cases only.
     *
     * Parameters: 
     *  new_extra - TODO (optional)
     *
     * Returns: n/a
     */
    this.extra = function(new_extra){
	anchor.query_extra = new_extra;
	return anchor.query_extra;
    };

    /*
     * Function: set
     *
     * Set an internal variable for the query.
     *
     * Parameters: 
     *  key - the name of the parameter to change
     *  new_val - what you want the new value to be
     *
     * Returns: n/a
     */
    this.set = function(key, new_val){
	anchor.query_variants[key] = new_val;
    };

    /*
     * Function: get
     *
     * Get an internal variable for the query.
     *
     * Parameters: 
     *  key - the name of the parameter to get
     *
     * Returns: n/a
     */
    this.get = function(key){
	return anchor.query_variants[key];
    };

    /*
     * Function: set_personality
     *
     * While we are always contacting the same Solr instance, we
     * sometimes want to have different weights, facets, etc. This
     * function allows us to use the pre-set ones defined in the
     * configuration argument.
     *
     * Parameters: 
     *  personality_id - string
     *
     * Returns: n/a - will error if personality doesn't exist
     */
    this.set_personality = function(personality_id){
	var retval = false;

	var cclass = anchor._golr_conf.get_class(personality_id);
	if( cclass ){
	    anchor.facets(cclass.field_order_by_weight('filter'));
	    retval = true;
	}

	// TODO: other consequences of "personality".

	return retval;
    };

    /*
     * Function: get_query_url
     *
     * Get the current invariant state of the manager returned as a
     * URL string.
     *
     * Returns: string
     */
    this.get_query_url = function(){

	// Structure of the necessary invariant parts.	
	var qurl = anchor._solr_url + 'select?';

	// Add all of our different specialized hashes.
	var things_to_add = [
	    //bbop.core.get_assemble(anchor.query_invariants),
	    //bbop.core.get_assemble(anchor.query_facets),
	    bbop.core.get_assemble(anchor.query_variants),
	    anchor.query_extra
	];
	bbop.core.each(things_to_add,
		       function(item, index){
			   if( item && item != '' ){
			       qurl = qurl + item;
			   }
		       });

    	return qurl;
    };
};
bbop.golr.manager.prototype = new bbop.registry;
