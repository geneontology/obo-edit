/* 
 * Package: golr_manager.js
 * 
 * Namespace: bbop.golr.manager
 * 
 * Generic BBOP manager for dealing with gross GOlr configuration
 * and management. Remember, this is actually a "subclass" of
 * <bbop.registry>.
 * 
 * Both json_data (or clean error data) and the manager itself (this
 * as anchor) should be passed to the callbacks.
 * 
 * TODO/BUG: <set_query> and <set_default_query> should both take
 * strings or <bbop.logic> as arguments. Those, as well as <get_query>
 * and <get_query> should only return <bbop.logic>.
 */

// Setup the internal requirements.
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'registry');
bbop.core.require('bbop', 'golr', 'conf');
bbop.core.require('bbop', 'golr', 'response');
//bbop.core.namespace('bbop', 'golr');
bbop.core.namespace('bbop', 'golr', 'manager');

/*
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
    //logger.DEBUG = true;
    logger.DEBUG = false;
    function ll(str){ logger.kvetch(str); }

    // To help keep requests from the past haunting us. Actually doing
    // something with this number is up to the UI.
    this.last_sent_packet = 0;
    //this.last_received_packet = 0;

    // Lightly check incoming arguments.
    // There should be a string url argument.
    // There could be a hash of pinned filters argument.
    if( ! golr_loc || ! golr_conf_obj ){
	ll('ERROR: no proper arguments');
    }
    if( typeof golr_loc != 'string' ){
	ll('ERROR: no proper golr url string argument');
    }
    if(	! golr_conf_obj._is_a || ! golr_conf_obj._is_a == 'bbop.golr.conf' ){
	    ll('ERROR: no proper bbop.golr.conf object argument');
	    throw new Error('boink! ' + bbop.core.what_is(golr_conf_obj) );
	}
    
    // Whether or not to prevent ajax events from going.
    // This may not be usable, or applicable, to all backends.
    this._safety = false;

    // Our default target url.
    this._solr_url = golr_loc;

    // Settle in the configurations.
    // this._golr_conf = new bbop.golr.conf(golr_conf_var);
    this._golr_conf = golr_conf_obj;

    // Our (default) query and the real deal.
    this.fundamental_query = '*:*'; // cannot be changed
    this.default_query = '*:*'; // changable
    this.query = this.default_query; //current

    // We remember defaults in the case of rows and start since they
    // are the core to any paging mechanisms and may change often.
    this.default_rows = 10;
    this.default_start = 0;

    // Our default query args, with facet fields plugged in.
    this.query_variants =
	{
	    // Our default standard search type. This means we don't
	    // have to explicitly add fields to the search (although
	    // the query fields ('qf') are still necessary to make
	    // anything real happen).
	    defType: 'edismax',

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
	    
	    // Deprecated: see query_fields
	    //qf: {},
	    
	    // Deprecated: see query
	    //q: '*:*'

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

    // This is the 'qf' parameter. Althoug we keep it, it only needs
    // to be exposed when the query ('q') field is set. These have a
    // different format, so assemble can't really be used.:
    // [qf=field01^value01]
    this.query_fields = [];

    // A richer way to handle the 'fq' query variant.
    // It should look like:
    // {<filter>: {<value>:{'sticky_p':(t|f), 'negative_p':(t|f)}, ...}}
    this.query_filters = {};

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
     *  A hash version of the plist; otherwise, the default property hash
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

    // A little extra thing that we might need sometimes.
    this.query_extra = null;

    // The callback function called after a successful AJAX
    // intialization/reset call. First it runs some template code,
    // then it does all of the callbacks.
    this._run_reset_callbacks = function(json_data){
	ll('run reset callbacks...');
	anchor.apply_callbacks('reset', [json_data, anchor]);
    };

    // The main callback function called after a successful AJAX call in
    // the update function.
    this._run_search_callbacks = function(json_data){
	ll('run search callbacks...');
	anchor.apply_callbacks('search', [json_data, anchor]);
    };

    // This set is called when we run into a problem.
    this._run_error_callbacks = function(json_data){
	ll('run error callbacks...');
	anchor.apply_callbacks('error', [json_data, anchor]);
    };

    /*
     * Function: sensible_query_p
     * 
     * Simply ask the manager if a free text query ('q') makes sense
     * at this point.
     * 
     * This currently means that the query text ('q') is three (3) or
     * longer and that query fields ('qf') are defined.
     * 
     * This is an overridable opinion of the manager.
     * 
     * Parameters:
     *  n/a
     *
     * Returns:
     *  boolean
     */
    this.sensible_query_p = function(qfs){
	var retval = false;
	var q = anchor.get_query();
	var qf = anchor.query_field_set();
	if( q && q.length >= 3 && qf && ! bbop.core.is_empty(qf) ){
	    retval = true;
	}
	return retval;
    };

    /*
     * Function: last_packet_sent
     *
     * It is up to the UI to do something interesting with this number.
     * 
     * Also remember that this number only rises through calls to
     * <update> or one of its wrappers. Calls to <get_query_url> and
     * the like will not affect this number.
     * 
     * Parameters:
     *  n/a 
     *
     * Returns:
     *  integer
     * 
     * See also:
     *  <update>
     */
    this.last_packet_sent = function(){
    	return anchor.last_sent_packet;
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
     *  the query url (with the jQuery callback specific parameters)
     * 
     * See also:
     *  <update>
     */
    this.reset = function(){
	return anchor.update('reset');
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
     *  n/a
     *
     * Returns:
     *  the query url (with the jQuery callback specific parameters)
     * 
     * See also:
     *  <update>
     */
    this.search = function(){
	return anchor.update('search');
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
     *  the query url (with the jQuery callback specific parameters)
     * 
     * See also:
     *  <update>
     */
    this.page = function(rows, start){
	anchor.set('rows', rows);
	anchor.set('start', start);
	return anchor.update('search', rows, start);
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
     *  the query url (with the jQuery callback specific parameters)
     * 
     * See also:
     *  <page>
     */
    this.page_previous = function(){
	var do_rows = anchor.get_page_rows();
	var do_offset = anchor.get_page_start() - do_rows;
	return anchor.page(do_rows, do_offset);
    };
    
    /*
     * Function: page_next
     * 
     * This is a wrapper for <page> and should be preferred over a
     * direct call to page.
     * 
     * Parameters: 
     *  the query url (with the jQuery callback specific parameters)
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
	return anchor.page(do_rows, do_offset);
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
     *  the query url (with the jQuery callback specific parameters)
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
	var ret = null;
	if( mod == 0 ){
	    ret = anchor.page(do_rows, do_offset - do_rows);
	}else{
	    ret = anchor.page(do_rows, do_offset);
	}
	return ret;
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
     * Function: query_field_set
     *
     * Getter/setter for the query fields--the fields that are search
     * (and by what weight) when using a query ('q').
     *
     * The qfs argument should be a hash like:
     * 
     *  {'field01': value01, ...}
     * 
     * Parameters: 
     *  qfs - *[optional]* query fields to set
     *
     * Returns:
     *  the current query_fields array (e.g. ["field01^value01", ...])
     */
    this.query_field_set = function(qfs){
	if( qfs ){
	    // Convert them to the proper internal format.
	    var actual_format = [];
	    bbop.core.each(qfs,
			   function(filter, value){
			       actual_format.push(filter + '^' + value);
			   });
	    anchor.query_fields = actual_format;
	}
	return anchor.query_fields;
    };

    /*
     * Function: facets
     *
     * Getter/setter for facets (technically 'facet.field').
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
     * Function: set_default_query
     *
     * Setter for the default query for the query variable ('q').
     * 
     * Call <reset_query> if you want to affect query immediately.
     * 
     * Parameters: 
     *  new_default_query - new default query string (or TODO: <bbop.logic>)
     *
     * Returns:
     *  the current setting of default query for ('q')
     */
    this.set_default_query = function(new_default_query){
	anchor.default_query = new_default_query;
	return anchor.default_query;
    };

    /*
     * Function: reset_default_query
     *
     * Reset the default query back to "*:*".
     * 
     * Call <reset_query> if you want to affect query immediately.
     * 
     * Parameters:
     *  n/a
     *
     * Returns:
     *  the current setting of default query ('q')
     */
    this.reset_default_query = function(){
	anchor.default_query = anchor.fundamental_query;
	return anchor.default_query;
    };

    /*
     * Function: set_query
     *
     * Setter for the query variable ('q').
     * 
     * Parameters: 
     *  new_query - new value for the query string (or TODO: <bbop.logic>)
     *
     * Returns:
     *  the current setting of query ('q')
     */
    this.set_query = function(new_query){
	anchor.query = new_query;
	return anchor.query;
    };

    /*
     * Function: set_id
     *
     * A limited setter, removing whatever else is on query. This is
     * for when you want to lock into one (unique) document by id
     * (essentially 'q=id:"foo"'). All other query operations behave
     * as they should around it.
     * 
     * Parameters: 
     *  new_id - string id
     *
     * Returns:
     *  the current setting of query ('q')
     */
    this.set_id = function(new_id){
	var nid = new_id;
	// Quote it if it doesn't already have them.
	if( new_id.charAt(0) != '"' && new_id.charAt(new_id.length -1) != '"' ){
	    nid = '"' + new_id + '"';	    
	}
	anchor.query = 'id:' + new_id;
	return anchor.query;
    };

    /*
     * Function: get_query
     *
     * Getter for the query variable ('q').
     * 
     * Parameters: 
     *  n/a
     *
     * Returns:
     *  the current setting of extra
     */
    this.get_query = function(){
	return anchor.query;
    };

    /*
     * Function: reset_query
     *
     * Remove/reset the query variable ('q'); this set it back to the
     * default query.
     *
     * Parameters:
     *  none
     *
     * Returns:
     *  ""
     * 
     * Also see:
     *  <set_default_query>
     *  <reset_default_query>
     */
    this.reset_query = function(){
	anchor.query = anchor.default_query;
	return anchor.query;
    };

    /*
     * Function: set_extra
     *
     * Setter for the internal string variable to be appended to the
     * end of a query. For special use cases only (e.g. extend
     * functionality of the API safely).
     * 
     * Parameters: 
     *  new_extra - *[optional]* new value for the extras string
     *
     * Returns:
     *  the current setting of extra
     */
    this.set_extra = function(new_extra){
	anchor.query_extra = new_extra;
	return anchor.query_extra;
    };

    /*
     * Function: get_extra
     *
     * Getter for the internal string variable to be appended
     * to the end of a query.
     *
     * Parameters: 
     *  n/a
     *
     * Returns:
     *  the current setting of extra
     */
    this.get_extra = anchor.set_extra;

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
     * Set an internal variable for the query. The internal variables
     * are typically things like 'qt', 'indent', etc.--things that you
     * might set and forget a while. It does /not/ include highly
     * dynamic variables (like callback and packet) or querying
     * variables like 'q' and 'fq'; for those you need to use the API.
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
     * See <set> for the kinds of parameters that can be read.
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
     * Currently, this only sets the 'facet.field' internal variable.
     *
     * Parameters: 
     *  personality_id - string
     *
     * Returns:
     *  Will return false if personality doesn't exist
     */
    this.set_personality = function(personality_id){
	var retval = false;

	// This sets the facet.field internal variable.
	var cclass = anchor._golr_conf.get_class(personality_id);
	if( cclass ){
	    anchor.facets(cclass.field_order_by_weight('filter'));

	    // Set the query field weights ('qf') necessary to make
	    // queries run properly.
	    anchor.query_field_set(cclass.get_weights('boost'));
	    
	    // Show that we did indeed set a personality.
	    retval = true;
	}

	return retval;
    };

    /*
     * Function: get_query_url
     *
     * Get the current invariant state of the manager returned as a
     * URL string.
     * 
     * This means the URL for the current query to the GOlr store, but
     * without extra information about packets, callbacks, and the
     * like.
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

	// Get all of our query filter variables and try and
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
	    //bbop.core.get_assemble({'qf': anchor.query_fields}),
	    bbop.core.get_assemble({'fq': fq}),
	    bbop.core.get_assemble({'q': anchor.query}),
	    anchor.query_extra
	];
	// Add query_fields ('qf') iff query ('q') is set and it is
	// not length 0.
	if( anchor.query &&
	    anchor.query.length &&
	    anchor.query.length != 0 &&
	    anchor.query != anchor.fundamental_query ){
		var in_qf = bbop.core.get_assemble({'qf': anchor.query_fields});
		things_to_add.push(in_qf);
	    }
	
	// Assemble the assemblies into a single URL, throw out
	// everything that seems like it isn't real to keep the URL as
	// clean a possible.
	var filtered_things = 
	    bbop.core.pare(things_to_add,
			   function(item, index){
			       var retval = true;
			       if( item && item != '' ){
				   retval = false;
			       }
			       return retval;
			   });
    	return qurl + filtered_things.join('&');
    };
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
 * The currently recognized callback types are "reset" (for when you
 * are starting or starting over) and "search" (what you typically
 * want when you get new data) and "error" for when something went
 * wrong. But only "search" and "reset" manipulate the system.
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
 *  callback_type - callback type string; 'search', 'reset' and 'error'
 *  rows - *[optional]* integer; the number of rows to return
 *  start - *[serially optional]* integer; the offset of the returned rows
 *
 * Returns:
 *  the query url (with the jQuery callback specific parameters)
 * 
 * Also see:
 *  <get_query_url>
 */
bbop.golr.manager.prototype.update = function(callback_type, rows, start){

    // Handle paging in this main section by resetting to
    // the defaults if rows and offset are not explicitly
    // defined.
    if( ! bbop.core.is_defined(rows) || ! bbop.core.is_defined(start) ){
    	this.set('rows', this.default_rows);
    	this.set('start', this.default_start);
    }
    
    // Our bookkeeping--increment packet.
    this.last_sent_packet = this.last_sent_packet + 1;
    
    // Necessary updated query variants.
    var update_query_variants = {
    	packet: this.last_sent_packet,
    	callback_type: callback_type
    };
    var update_qv = bbop.core.get_assemble(update_query_variants);
    
    // Structure of the necessary invariant parts.	
    //var qurl = this.get_query_url();
    var qurl = null;
    
    // Conditional merging of the remaining variant parts.
    if( callback_type == 'reset' ){
	
    	// Take everything back to the initial state--this means
    	// resetting the query and removing all non-sticky
    	// filters.
	
    	// Reset and do completely open query.
    	//ll('reset assembly');
	
    	// Save the q vals, do a fundamental get, then reset to
    	// what we had.
    	//var tmp_save = this.get_query();
    	//this.reset_default_query();
    	this.reset_query();
    	this.reset_query_filters();
    	qurl = this.get_query_url();
    	qurl = qurl + '&' + update_qv;
    	//this.set_query(tmp_save);
	
    }else if( callback_type == 'search' ){
	
    	//ll('search assembly');
    	qurl = this.get_query_url();
    	qurl = qurl + '&' + update_qv;
	
    }else{
    	throw new Error("Unknown callback_type: " + callback_type);
    }
    
    return qurl;
};
