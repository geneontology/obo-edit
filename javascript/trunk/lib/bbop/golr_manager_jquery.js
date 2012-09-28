/* 
 * Package: golr_manager_jquery.js
 * 
 * Namespace: bbop.golr.manager.jquery
 * 
 * jQuery BBOP manager for dealing with actual ajax calls. Remember,
 * this is actually a "subclass" of <bbop.golr.manager>.
 * 
 * This should still be able to limp along (no ajax and no error
 * parsing) even outside of a jQuery environment.
 */

// Setup the internal requirements.
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'registry');
bbop.core.require('bbop', 'golr', 'conf');
bbop.core.require('bbop', 'golr', 'response');
bbop.core.require('bbop', 'golr', 'manager');
bbop.core.namespace('bbop', 'golr', 'manager', 'jquery');

/*
 * Constructor: jquery
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
 *  <bbop.golr.manager>
 */
bbop.golr.manager.jquery = function (golr_loc, golr_conf_obj){
//function GOlrManager(in_args){
    // We are a registry like this:
    bbop.golr.manager.call(this, golr_loc, golr_conf_obj);
    this._is_a = 'bbop.golr.manager.jquery';

    // Get a good self-reference point.
    var anchor = this;

    // Per-manager logger.
    var logger = new bbop.logger(this._is_a);
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    // Before anything else, if we cannot find a viable jQuery library
    // for use, we're going to create a fake one so we can still test
    // and work in a non-browser/networked environment.
    anchor.JQ = new bbop.golr.faux_ajax();
    try{ // some interpreters might not like this kind of probing
    	if( typeof(jQuery) !== 'undefined' ){
    	    //JQ = jQuery;
    	    anchor.JQ = jQuery.noConflict();
    	}
    }catch (x){
    }finally{
    	var got = bbop.core.what_is(anchor.JQ);
    	if( got && got == 'bbop.golr.faux_ajax'){
    	}else{
    	    got = 'jQuery';
    	}
    	ll('Using ' + got + ' for Ajax calls.');
    }

    // The base jQuery Ajax args we need with the setup we have.
    anchor.jq_vars = {
	//url: qurl,
	type: "GET",
	dataType: 'jsonp',
	jsonp: 'json.wrf'
    };

    /*
     * Function: safety
     *
     * Getter/setter for the trigger safety.
     * 
     * If the safety is on, ajax events controlled by the manager will
     * not occur. The default if off (false).
     * 
     * Parameters: 
     *  safety_on_p - boolean
     *
     * Returns:
     *  boolean
     */
    this.safety = function(safety_on_p){
	if( bbop.core.is_defined(safety_on_p) ){
	    anchor._safety = safety_on_p;
	}
	return anchor._safety;
    };

//     /*
//      * Function: async
//      *
//      * Getter/setter for the jQuery sync/async action.
//      * 
//      * You probably really really /really/ don't want to touch this.
//      * 
//      * Parameters: 
//      *  async_p - boolean; default true
//      *
//      * Returns:
//      *  boolean (current state)
//      */
//     this.async = function(async_p){
// 	// We lied to them up there. What we really want is to not
// 	// have this variable specificied most of the time since it
// 	// defaults in jQuery to true.
// 	var retval = true;
// 	if( ! bbop.core.is_defined(async_p) ){ // just want to know what it is
// 	    if( bbop.core.is_defined(anchor.jq_vars['async']) ){
// 		retval = false;
// 	    }
// 	}else{ // define and return
// 	    if( ! bbop.core.is_defined(anchor.jq_vars['async']) ){
// 		if( async_p == true ){
// 		    delete anchor.jq_vars['async'];
// 		}else{
// 		    anchor.jq_vars['async'] = false;
// 		    retval = false;
// 		}
// 	    }
// 	}
// 	return retval;
//     };
};

/*
 * Function: update
 *
 *  See the documentation in <golr_manager.js> on update to get more
 *  of the story. This override function adds functionality for
 *  jQuery.
 * 
 * You can prevent the triggering of ajax with the <safety>
 * method.
 *
 * Parameters: 
 *  callback_type - callback type string
 *  rows - *[serially optional]* integer; the number of rows to return
 *  start - *[serially optional]* integer; the offset of the returned rows
 *
 * Returns:
 *  the query url (with the jQuery callback specific parameters)
 * 
 * Also see:
 *  <get_query_url>
 */
bbop.golr.manager.jquery.prototype.update = function(callback_type,
						     rows, start){
    
    // Get "parents" url first.
    var parent_update = bbop.golr.manager.prototype.update;
    var qurl = parent_update.call(this, callback_type, rows, start);
    
    // Only actually trigger if the safety of off.
    if( ! this.safety() ){
	
	//ll('try: ' + qurl);
	//widgets.start_wait('Updating...');
	
	// Setup JSONP for Solr and jQuery ajax-specific parameters.
	this.jq_vars['success'] = this._callback_type_decider; // decide & run
	this.jq_vars['error'] = this._run_error_callbacks; // run error cbs
	//done: _callback_type_decider, // decide & run search or reset
	//fail: _run_error_callbacks, // run error callbacks
	//always: function(){} // do I need this?
	this.JQ.ajax(qurl, this.jq_vars);
    }
    
    return qurl;
};

// /*
//  * Function: action
//  *
//  * A odd cousin to <update>. It's only of use in very specific
//  * circumstances; generally, do not use.
//  * 
//  * Parameters: 
//  *  callback - callback
//  *
//  * Returns:
//  *  the query url (with the jQuery callback specific parameters)
//  * 
//  * Also see:
//  *  <update>
//  */
// bbop.golr.manager.jquery.prototype.action = function(callback_type){
    
//     // Get "parents" url first.
//     var qurl = this.get_query_url();
    
//     //ll('try: ' + qurl);
//     //widgets.start_wait('Updating...');
	
//     // Setup JSONP for Solr and jQuery ajax-specific parameters.
//     this.jq_vars['success'] = this.callback;
//     this.jq_vars['error'] = function(){}; // all errors dropped
//     this.JQ.ajax(qurl, this.jq_vars);
    
//     return qurl;
// };

/*
 * Namespace: bbop.golr.faux_ajax
 *
 * Constructor: faux_ajax
 * 
 * Contructor for a fake and inactive Ajax. Used by bbop.golr.manager.jquery
 * in (testing) environments where jQuery is not available.
 * 
 * Returns:
 *  faux_ajax object
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
     *  args - whatever--they are ignored
     *
     * Returns:
     *  ""
     */
    this.parseJSON = function(args){
	return "";
    };
};
