/* 
 * Package: golr_manager.js
 * Namespace: bbop.golr.manager
 * 
 * Generic BBOP manager for dealing with gross GOlr configuration
 * and management. Remember, this is actually a "subclass" of
 * bbop.registry.
 * 
 * Both json_data (or clean error data) and the manager itself (this
 * as anchor) should be passed to the callbacks.
 */

// Setup the internal requirements.
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'registry');
bbop.core.require('bbop', 'golr', 'conf');
bbop.core.require('bbop', 'golr', 'response');
//bbop.core.namespace('bbop', 'golr');
bbop.core.namespace('bbop', 'golr', 'manager');
bbop.core.namespace('bbop', 'golr', 'faux_ajax');

/*
 * Structure: bbop.golr.faux_ajax
 * Constructor: faux_ajax
 * 
 * Contructor for a fake and inactive Ajax. Used by bbop.golr.manager
 * in (testing) environments where jQuery is not available.
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
     * Returns:
     *  null
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
     * Returns:
     *  ""
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
 *  golr_conf_obj - a <bbop.golr.conf> object
 * 
 * Returns:
 *  golr manager object
 * 
 * See also:
 *  <bbop.registry>
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

    // Our (default) query and the real deal.
    this.default_query = '*:*';
    this.query = this.default_query;

    // We remember defaults in the case of rows and start since they
    // are the core to any paging mechanisms and may change often.
    this.default_rows = 10;
    this.default_start = 0;

    // Our default query args, with facet fields plugged in.
    this.query_variants =
	{
	    // Things unlikely to be touched.
	    // There are unlikely to be messed with too much.
	    qt: 'standard',
	    indent: 'on',
	    wt: 'json',
	    //version: '2.2',
	    rows: anchor.default_rows,
	    start: anchor.default_start, // Solr is offset indexing
	    fl: '*%2Cscore',
    
	    // Deprecated: see query_filters
	    //fq: {},
	    
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
	    // They are unlikely to be messed with too much.
	    'facet.field': []
	};

    // A richer way to handle the 'fq' query variant.
    // It should look like:
    // {<filter>: {<value>:{'sticky_p':(t|f), 'negative_p':(t|f)}, ...}}
    this.query_filters = {};

    // /*
    //  * Function: plist_to_property_hash
    //  *
    //  * Turn a plist to a hash containing the different properties that
    //  * can be defined for a query filter. Possible values are: '+'
    //  * (positive filter), '-' (negative filter), '*' (sticky filter),
    //  * '$' (transient). If mutually exclusive properties are defined
    //  * (e.g. both '+' and '-'), the last one will be used. Or, since
    //  * that is a call to silliness, let's say the behavior is
    //  * undefined.
    //  *
    //  * Parameters: 
    //  *  plist - *[optional]* a list of properties to apply to the filter
    //  *
    //  * Returns: 
    //  *  A hash version of the plist; otherwise, the defaul property hash
    //  */
    // this.set_query = function(plist){
    // };

    /*
     * Function: plist_to_property_hash
     *
     * Turn a plist to a hash containing the different properties that
     * can be defined for a query filter. Possible values are: '+'
     * (positive filter), '-' (negative filter), '*' (sticky filter),
     * '$' (transient). If mutually exclusive properties are defined
     * (e.g. both '+' and '-'), the last one will be used. Or, since
     * that is a call to silliness, let's say the behavior is
     * undefined.
     *
     * Parameters: 
     *  plist - *[optional]* a list of properties to apply to the filter
     *
     * Returns: 
     *  A hash version of the plist; otherwise, the defaul property hash
     */
    this.plist_to_property_hash = function(plist){

	// Let's start with the default values.
	var phash = {
	    //'positive_p': true,
	    'negative_p': false,
	    //'transient_p': true
	    'sticky_p': false
	};

	// If not defined, just return the default list.
	if( plist ){	    
	    bbop.core.each(plist,
			   function(item){
			       if( item == '+' ){
				   phash['negative_p'] = false;
				   //phash['positive_p'] = true;
			       }else if( item == '-' ){
				   phash['negative_p'] = true;
				   //phash['positive_p'] = false;
			       }else if( item == '*' ){
				   phash['sticky_p'] = true;
				   //phash['transient_p'] = false;
			       }else if( item == '$' ){
				   phash['sticky_p'] = false;
				   //phash['transient_p'] = true;
			       }
			   });
	}

	return phash;
    };

    /*
     * Function: add_query_filter
     *
     * Setter for query filters ('fq').
     *
     * Parameters: 
     *  filter - filter (type) string
     *  value - filter value string (or TODO: defined logic hash)
     *  plist - *[optional]* list of properties of the filter
     *
     * Returns: 
     *  (TODO) The current query filter hash.
     * 
     * See also:
     *  <plist_to_property_hash>
     */
    this.add_query_filter = function(filter, value, plist){
	
	// Make sure we've defined the group.
	if( ! bbop.core.is_defined(this.query_filters[filter]) ){
	    this.query_filters[filter] = {};
	}

	this.query_filters[filter][value] = this.plist_to_property_hash(plist);
	
	ll("Current state: " + bbop.core.dump(this.query_filters));

	return {}; // TODO
    };

    /*
     * Function: remove_query_filter
     *
     * Remover for query filters ('fq'), is a plist is specified, it
     * will only remove if all of the listed criteria are met.
     *
     * Parameters: 
     *  filter - filter (type) string
     *  value - filter value string (TODO: or defined logic hash)
     *  plist - *[optional]* list of properties of the filter
     *
     * Returns: 
     *  boolean (on success)
     */
    this.remove_query_filter = function(filter, value, plist){

	// Default return value.
	var retval = false;

	// Internal helper to delete a low level key, and then if the
	// top-level is empty, get that one too.
	function _full_delete(hash, key1, key2){
	    if( key1 && key2 && hash &&
		hash[key1] && hash[key1][key2] ){
		    delete hash[key1][key2];
		}
	    if( bbop.core.is_empty(hash[key1]) ){
		delete hash[key1];
	    }
	}

	// If we have a filter, a value, and it's there...
	if( filter && value &&
	    anchor.query_filters[filter] &&
	    anchor.query_filters[filter][value] ){

		// If no real plist hash been defined, just go ahead
		// and get rid of that. Otherwise, make sure that the
		// defined plist and the stored properties are the
		// same before deleting.
		if( ! plist || bbop.core.is_empty(plist) ){
		    _full_delete(anchor.query_filters, filter, value);
		    retval = true;
		}else{
		    
		    var filter_phash = anchor.query_filters[filter][value];
		    var in_phash = anchor.plist_to_property_hash(plist);
		    
		    if( bbop.core.is_same(filter_phash, in_phash) ){		
			_full_delete(anchor.query_filters, filter, value);
			retval = true;
		    }
		}
	    }

	return retval;
    };

    /*
     * Function: reset_query_filters
     *
     * Reset the query filters ('fq'); but leave sticky filters alone.
     *
     * Parameters: 
     *  n/a
     * 
     * Returns: 
     *  (TODO) The current query filter hash.
     */
    this.reset_query_filters = function(){

	// Drill down and delete all non-stickies.
	var loop = bbop.core.each;
	loop(anchor.query_filters,
	     function(filter, values){
		 //ll('filter: ' + filter);
		 loop(values,
		      function(value, props){
			  //ll('  value: ' + value);
			  var sticky_p = props['sticky_p'];
			  if( ! sticky_p ){
			      //ll('hit: ' + filter + ', ' + value);
			      anchor.remove_query_filter(filter, value);
			  }
		      });
	     });

	return {}; // TODO
    };

    /*
     * Function: get_query_filter_properties
     *
     * Get a hash representing a query filter ('fq').
     *
     * Parameters: 
     *  key - filter string (TODO: or defined logic hash)
     *
     * Returns: 
     *  The current query filter hash for key.
     */
    this.get_query_filter_properties = function(filter, value){

	// Default return value.
	var retobj = null;
	
	// If we have a key and it's there...
	var aqf = anchor.query_filters;
	if( filter && value && aqf[filter] && aqf[filter][value] ){
	    retobj =
		{
		    'filter' : filter,
		    'value' : value,
		    //'polarity': aqf[filter][value]['negative_p'],
		    'negative_p': aqf[filter][value]['negative_p'],
		    'sticky_p': aqf[filter][value]['sticky_p']
		};
	}

	return retobj;
    };

    /*
     * Function: get_query_filters
     *
     * Get a list of hashes representing the query filters ('fq'). The
     * return lists look like:
     *
     * : [{'filter': A, 'value': B, 'negative_p': C, 'sticky_p': D}, ...]
     *
     * Where A and B are strings and C and D are booleans.
     * 
     * Parameters: 
     *  n/a
     *
     * Returns: 
     *  A list of the current query filter hashs.
     */
    this.get_query_filters = function(){

	var retlist = [];	
	var loop = bbop.core.each;
	loop(anchor.query_filters,
	     function(f, values){
		 loop(values,
		      function(v, props){
			  retlist.push(anchor.get_query_filter_properties(f,v));
		      });
	     });

	return retlist;
    };

    /*
     * Function: get_sticky_query_filters
     *
     * Get a list of hashes representing the current stucky query
     * filters ('fq'). See <get_query_filters> for a specification of
     * what the return type looks like.
     * 
     * Parameters: 
     *  n/a
     *
     * Returns: 
     *  A list of the current sticky query filter hashs.
     * 
     * See also:
     *  <get_query_filters>
     */
    this.get_sticky_query_filters = function(){

	var retlist = [];	
	var loop = bbop.core.each;
	loop(anchor.query_filters,
	     function(f, values){
		 loop(values,
		      function(v, props){
			  var qfp = anchor.get_query_filter_properties(f,v);
			  if( qfp['sticky_p'] == true ){
			      retlist.push(qfp);			      
			  }
		      });
	     });

	return retlist;
    };

    // // TODO: deprecate this
    // // A set of filters that survive things like reset, etc. Must be
    // // explicitly set outside of the "normal" methods.
    // this.query_sticky_filters = {};

    // A little extra thing that we might need sometimes.
    this.query_extra = null;

    // The callback function called after a successful AJAX
    // intialization/reset cal. First it runs some template code, then it
    // does all of the callbacks.
    this._run_reset_callbacks = function(json_data){
	ll('run reset callbacks...');
	anchor.apply_callbacks('reset', [json_data, anchor]);
    };

    // The main callback function called after a successful AJAX call in
    // the update function. First it runs some template code, then it does
    // all of the callbacks.
    this._run_search_callbacks = function(json_data){
	ll('run search callbacks...');
	anchor.apply_callbacks('search', [json_data, anchor]);
    };

    // This is the function that runs where there is an AJAX error
    // during an update. First it runs some template code, then it
    // does all of the callbacks.
    this._run_error_callbacks = function(result, status, error) {

	ll('Failed server request: '+ result +', '+ status +', '+ error);
		
	// Get the error out (clean it) if possible.
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
	anchor.apply_callbacks('error', [clean_error, anchor]);
    };
    var _run_error_callbacks = this._run_error_callbacks;

    // Try and decide between a reset callback and a search callback.
    function _callback_type_decider(json_data){
    	ll('in callback type decider...');

    	// 
    	if( ! bbop.golr.response.success(json_data) ){
    	    throw new Error("Unsuccessful response from golr server!");
    	}else{
    	    var cb_type = bbop.golr.response.callback_type(json_data);
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
     * Function: clear
     *
     * Clear all non-sticky query parameters to get back to a more
     * "original" state.
     * 
     * Not to be confused with <reset>.
     * 
     * Parameters: 
     *  n/a
     *
     * Returns:
     *  n/a
     */
    this.clear = function(){

	// Reset 'q'.
	anchor.query = anchor.default_query;

	// Reset 'fq', all but sticky.
	anchor.reset_query_filters();
    };

    /*
     * Function: update
     *
     * The user code to select the type of update (and thus the type
     * of callbacks to be called on data return).
     * 
     * This mechanism adds a couple of variables over other methods
     * for bookkeeping: packet (incremented every time) and callback_type.
     * 
     * The currently recognized callback types are "reset" (for when
     * you are starting or starting over) and "search" (what you
     * typically want when you get new data).
     * 
     * The logic_hash argument is a string keyed hash of bbop.logic.
     * This is a curried wrapper for update objects. The only two keys
     * currently identified are 'q' and 'fq'.
     * 
     * If rows or start are not set, they will both be reset to their
     * initial values--this is to allow for paging on "current"
     * results and then getting back to the business of searching with
     * as little fuss as possible. Because of things like this, one
     * should avoid calling this directly whenever possible and prefer
     * simpler functionality of the wrapper methods: <search>,
     * <reset>, and <page>.
     *
     * Parameters: 
     *  callback_type - callback type string
     *  logic_hash - *[optional]* logic hash argument
     *  rows - *[serially optional]* integer; the number of rows to return
     *  start - *[serially optional]* integer; the offset of the returned rows
     *
     * Returns:
     *  n/a
     * 
     * Also see:
     *  <get_query_url>
     */
    this.update = function(callback_type, logic_hash, rows, start){

	// Handle paging in this main section by resetting to
	// the defaults if rows and offset are not explicitly
	// defined.
	if( ! bbop.core.is_defined(rows) || ! bbop.core.is_defined(start) ){
	    anchor.set('rows', anchor.default_rows);
	    anchor.set('start', anchor.default_start);
	}

	// Our bookkeeping--increment packet.
	anchor.last_sent_packet = anchor.last_sent_packet + 1;
	
	// Necessary updated query variants.
	var update_query_variants = {
	    packet: anchor.last_sent_packet,
	    callback_type: callback_type
	};
	var update_qv = bbop.core.get_assemble(update_query_variants);

	// Structure of the necessary invariant parts.	
	var qurl = anchor.get_query_url();

	// Conditional merging of the remaining variant parts.
	if( callback_type == 'reset' ){

	    // Take everything back to the initial state--this means
	    // resetting the query and removing all non-sticky
	    // filters.

	    // Reset and do completely open query.
	    ll('reset variant assembly');
	    ll('update_qv: ' + update_qv);
	    qurl = qurl + '&' + update_qv + '&q=' + anchor.query;
	    //qurl = qurl + '&' + update_qv;

	}else if( callback_type == 'search' ){

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
	    //ll('varient_qs: ' + update_qv);
	    qurl = qurl + '&' + update_qv + filter_qs + '&q=' + query_string;

	}else{
	    throw new Error("Unknown callback_type: " + callback_type);
	}

	ll('try: ' + qurl);
	//widgets.start_wait('Updating...');

	// TODO/BUG: JSONP for solr looks like?
	var argvars = {
	    type: "GET",
	    url: qurl,
	    dataType: 'json',
	    jsonp: 'json.wrf',
	    success: _callback_type_decider, // decide and run search or reset
	    error: _run_error_callbacks // run error callbacks
	};
	JQ.ajax(argvars);
    };

    /*
     * Function: reset
     *
     * Manually trigger the "reset" chain of events.
     *
     * This is a curried wrapper for <update> and should be preferred
     * over a direct call to update.
     *
     * Note to be confused with <clear>.
     *
     * Returns:
     *  n/a
     * 
     * See also:
     *  <update>
     */
    this.reset = function(){
	anchor.update('reset', null);
    };

    /*
     * Function: search
     *
     * Trigger the "search" chain of events.
     * Takes a field-keyed hash of bbop.logics as an argument.
     * 
     * This is a curried wrapper for <update> and should be preferred
     * over a direct call to update.
     * 
     * Parameters: 
     *  logic_hash - *[optional]* logic hash argument
     *
     * Returns: n/a
     * 
     * See also:
     *  <update>
     */
    this.search = function(logic_hash){
	anchor.update('search', logic_hash);
    };

    /*
     * Function: page
     *
     * Re-trigger the "search" chain of events, but with the variables
     * set for a different section of the results.
     * 
     * Note that this operates independently of any impossibilites in
     * the results--just how such paging would look and
     * triggering. Ths UI should handle impossibilities and the like.
     * 
     * This is a wrapper for <update> and should be preferred over a
     * direct call to update.
     * 
     * Parameters: 
     *  rows - the number of rows to return
     *  start - the offset of the rows to return
     *
     * Returns:
     *  n/a
     * 
     * See also:
     *  <update>
     */
    this.page = function(rows, start){
	anchor.set('rows', rows);
	anchor.set('start', start);
	anchor.update('search', null, rows, start);
    };

    /*
     * Function: page_first
     *
     * Currently a convenience alias for <search>. Think about it--it
     * makes sense.
     * 
     * This is a wrapper for <page> and should be preferred over a
     * direct call to page.
     * 
     * Parameters: 
     *  n/a
     *
     * Returns:
     *  n/a
     * 
     * See also:
     *  <page>
     */
    this.page_first = anchor.search;
    
    /*
     * Function: page_previous
     * 
     * This is a wrapper for <page> and should be preferred over a
     * direct call to page.
     * 
     * Parameters: 
     *  n/a
     *
     * Returns:
     *  n/a
     * 
     * See also:
     *  <page>
     */
    this.page_previous = function(){
	var do_rows = anchor.get_page_rows();
	var do_offset = anchor.get_page_start() - do_rows;
	anchor.page(do_rows, do_offset);
    };
    
    /*
     * Function: page_next
     * 
     * This is a wrapper for <page> and should be preferred over a
     * direct call to page.
     * 
     * Parameters: 
     *  n/a
     *
     * Returns:
     *  n/a
     * 
     * See also:
     *  <page>
     */
    this.page_next = function(){
	var do_rows = anchor.get_page_rows();
	var do_offset = anchor.get_page_start() + do_rows;
	anchor.page(do_rows, do_offset);
    };
    
    /*
     * Function: page_last
     * 
     * Trigger search on last page parameters.
     * 
     * Since the manager has no idea about what is actually being
     * returned, the real world number of total documents needs to be
     * added as an argument.
     * 
     * This is a wrapper for <page> and should be preferred over a
     * direct call to page.
     * 
     * Parameters: 
     *  total_document_count - integer for the total number of docs found
     *
     * Returns:
     *  n/a
     * 
     * See also:
     *  <page>
     */
    this.page_last = function(total_document_count){
	var do_rows = anchor.get_page_rows();
	var mod = total_document_count % do_rows;
	var do_offset = total_document_count - mod;
	// ll("page_last: " + total_document_count + " " +
	//    do_rows + " " + mod + " " + do_offset);
	if( mod == 0 ){
	    anchor.page(do_rows, do_offset - do_rows);
	}else{
	    anchor.page(do_rows, do_offset);
	}
    };

    /*
     * Function: get_page_rows
     *
     * Return the number of rows the manager is currently set
     * to. Useful as an argument to <page>.
     * 
     * Parameters: 
     *  n/a
     *
     * Returns:
     *  integer; the number of rows the manager is currently set to
     * 
     * See also:
     *  <page>
     */
    this.get_page_rows = function(){
	return anchor.get('rows');
    };

    /*
     * Function: get_page_start
     *
     * Return the rows offset the manager is currently set to. Useful
     * as an argument to <page>.
     * 
     * Parameters: 
     *  n/a
     *
     * Returns:
     *  integer; the offset the manager is currently set to
     * 
     * See also:
     *  <page>
     */
    this.get_page_start = function(){
	return anchor.get('start');
    };

    /*
     * Function: facets
     *
     * Getter/setter for facets.
     *
     * Parameters: 
     *  key - *[optional]* facet to add to the facet list
     *
     * Parameters: 
     *  list - *[optional]* list to replace the current list with
     *
     * Returns:
     *  the current facets hash.
     */
    this.facets = function(list_or_key){
	if( list_or_key ){
	    if( bbop.core.what_is(list_or_key) == 'array' ){ // replace as list
		anchor.query_variants['facet.field'] = list_or_key;
	    }else{ // add as key
		anchor.query_variants['facet.field'].push(list_or_key);
	    }
	}
	return anchor.get('facet.field');
    };

    /*
     * Function: extra
     *
     * Getter/setter for the internal string variable to be appended
     * to the end of a query. For special use cases only (e.g. extend
     * functionality of the API safely).
     *
     * Parameters: 
     *  new_extra - *[optional]* new value for the extras string
     *
     * Returns:
     *  The current setting of extra
     */
    this.extra = function(new_extra){
	anchor.query_extra = new_extra;
	return anchor.query_extra;
    };

    /*
     * Function: remove_extra
     *
     * Remove/reset the extra bit.
     *
     * Parameters:
     *  none
     *
     * Returns:
     *  ""
     */
    this.remove_extra = function(){
	anchor.query_extra = "";
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
     * Returns:
     *  The found value of the key.
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
     * constructor configuration argument.
     *
     * Parameters: 
     *  personality_id - string
     *
     * Returns:
     *  Will return false if personality doesn't exist
     */
    this.set_personality = function(personality_id){
	var retval = false;

	var cclass = anchor._golr_conf.get_class(personality_id);
	if( cclass ){
	    anchor.facets(cclass.field_order_by_weight('filter'));
	    retval = true;
	}

	// TODO: other consequences of "personality".
	// Like what!? Tell me, Past Me!

	return retval;
    };

    /*
     * Function: get_query_url
     *
     * Get the current invariant state of the manager returned as a
     * URL string.
     * 
     * This is generally appropriate for getting data, but maybe not
     * for things like high-speed autocomplete where races can
     * occur. For those, you might want to consider <update> or
     * <search>.
     *
     * Parameters:
     *  n/a
     * 
     * Returns:
     *  URL string
     * 
     * Also see:
     *  <update>, <search>
     */
    this.get_query_url = function(){

	// Structure of the necessary invariant parts.	
	var qurl = anchor._solr_url + 'select?';

	// TODO: Get all of our query filter variables and try and
	// make something of them that get_assemble can
	// understand. Sticky doesn't matter here, but negativity
	// does. However, we can be pretty naive since the hashing
	// should have already taken out mutually exclusive dupes.
	var fq = {};
	var loop = bbop.core.each;
	loop(anchor.get_query_filters(),
	     function(filter_property){

		 // Grab only the properties that affect the
		 // URL.
		 var filter = filter_property['filter'];
		 var value = filter_property['value'];
		 var negative_p = filter_property['negative_p'];

		 // We need to alter at the filter level.
		 if( negative_p ){
		     filter = '-' + filter;
		 }

		 // Make sure it is defined.
		 if( ! bbop.core.is_defined(fq[filter]) ){
		     fq[filter] = [];
		 }
		 fq[filter].push(value);
		 //fq[filter] = value;
	     });

	// Add all of our different specialized hashes.
	var things_to_add = [
	    //bbop.core.get_assemble(anchor.query_invariants),
	    //bbop.core.get_assemble(anchor.query_facets),
	    bbop.core.get_assemble(anchor.query_variants),
	    //bbop.core.get_assemble({'fq': anchor.query_sticky_filters}),
	    bbop.core.get_assemble({'fq': fq}),
	    anchor.query_extra
	];
	loop(things_to_add,
	     function(item, index){
		 if( item && item != '' ){
		     qurl = qurl + '&' + item;
		 }
	     });
	
    	return qurl;
    };
};
bbop.golr.manager.prototype = new bbop.registry;
