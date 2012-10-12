/* 
 * Package: rhino.js
 * 
 * Namespace: bbop.golr.manager.rhino
 * 
 * Rhino BBOP manager for dealing with remote calls. Remember,
 * this is actually a "subclass" of <bbop.golr.manager>.
 * 
 * This may be madness.
 */

// Setup the internal requirements.
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'registry');
bbop.core.require('bbop', 'golr', 'conf');
bbop.core.require('bbop', 'golr', 'response');
bbop.core.require('bbop', 'golr', 'manager');
bbop.core.namespace('bbop', 'golr', 'manager', 'rhino');

/*
 * Constructor: rhino
 * 
 * Contructor for the GOlr query manager; Rhino-style.
 * 
 * Beware that this version is a synchronous call.
 * 
 * Arguments:
 *  golr_loc - string url to GOlr server
 *  golr_conf_obj - a <bbop.golr.conf> object
 * 
 * Returns:
 *  golr manager object
 * 
 * See also:
 *  <bbop.golr.manager>
 */
bbop.golr.manager.rhino = function (golr_loc, golr_conf_obj){
    bbop.golr.manager.call(this, golr_loc, golr_conf_obj);
    this._is_a = 'bbop.golr.manager.rhino';
};
bbop.core.extend(bbop.golr.manager.rhino, bbop.golr.manager);

/*
 * Function: update
 *
 *  See the documentation in <golr_manager.js> on update to get more
 *  of the story. This override function adds functionality to Rhino.
 *
 * Parameters: 
 *  callback_type - callback type string
 *  rows - *[serially optional]* integer; the number of rows to return
 *  start - *[serially optional]* integer; the offset of the returned rows
 *
 * Returns:
 *  the query url (with any Rhino specific paramteters)
 * 
 * Also see:
 *  <get_query_url>
 */
bbop.golr.manager.rhino.prototype.update = function(callback_type,
						    rows, start){
    // Get "parents" url first.
    var parent_update = bbop.golr.manager.prototype.update;
    var qurl = parent_update.call(this, callback_type, rows, start);

    // 
    var logger = new bbop.logger(this._is_a);
    //this._logger = new bbop.logger(this._is_a);
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    // Grab the data from the server and pick the right callback group
    // accordingly.
    var raw = readUrl(qurl); // in Rhino
    var json_data = null;
    if( raw && raw != '' ){
	json_data = JSON.parse(raw);
	if( json_data ){
	    this.apply_callbacks(callback_type, [json_data, this]);
	}else{
	    this.apply_callbacks('error', ['unparsable data', this]);
	}
    }else{
	this.apply_callbacks('error', ['no data', this]);
    }

    return qurl;
};

/*
 * Function: fetch
 *
 * This is the synchronous data getter for Rhino--probably your best
 * bet right now for scripting.
 * 
 * Parameters:
 *  n/a 
 *
 * Returns:
 *  the JSON response as an object or null
 * 
 * Also see:
 *  <update>
 */
bbop.golr.manager.rhino.prototype.fetch = function(){
    
    var qurl = this.get_query_url();

    // Grab the data from the server and pick the right callback group
    // accordingly.
    var raw = readUrl(qurl); // in Rhino
    var json_data = null;
    var retval = null;
    if( raw && raw != '' ){
	json_data = JSON.parse(raw);
	if( json_data ){
	    retval = json_data;
	}
    }

    return retval;
};

