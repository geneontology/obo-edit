/* 
 * Package: preload.js
 * 
 * Namespace: bbop.golr.manager.preload
 * 
 * Preload BBOP manager for dealing with remote calls. Remember,
 * this is actually a "subclass" of <bbop.golr.manager>.
 * 
 * This is synchronous.
 * 
 * This is mostly for testing purposes.
 */

// Setup the internal requirements.
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'registry');
bbop.core.require('bbop', 'golr', 'conf');
bbop.core.require('bbop', 'golr', 'response');
bbop.core.require('bbop', 'golr', 'manager');
bbop.core.namespace('bbop', 'golr', 'manager', 'preload');

/*
 * Constructor: preload
 * 
 * Contructor for the GOlr query manager.
 * 
 * Allows preloading of the returned document.
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
bbop.golr.manager.preload = function (golr_loc, golr_conf_obj){
    bbop.golr.manager.call(this, golr_loc, golr_conf_obj);
    this._is_a = 'bbop.golr.manager.preload';

    // The only property to add.
    this._bgm_load = null;
};
bbop.core.extend(bbop.golr.manager.preload, bbop.golr.manager);

/*
 * Function: load
 *
 * Parameters: 
 *  thing - what to send to the callbacks
 *
 * Returns:
 *  n/a
 */
bbop.golr.manager.preload.prototype.load = function(thing){
    this._bgm_load = thing;    
};

/*
 * Function: update
 *
 *  See the documentation in <golr_manager.js> on update to get more
 *  of the story. This override function adds a trigger that can be
 *  preloaded with results. Really only for testing.
 *
 * Parameters: 
 *  callback_type - callback type string
 *  rows - *[serially optional]* integer; the number of rows to return
 *  start - *[serially optional]* integer; the offset of the returned rows
 *
 * Returns:
 *  the query url
 * 
 * Also see:
 *  <get_query_url>
 */
bbop.golr.manager.preload.prototype.update = function(callback_type,
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
    var data = this._bgm_load;
    if( bbop.core.is_defined(data) ){
	this.apply_callbacks(callback_type, [data, this]);
    }else{
	this.apply_callbacks('error', ['unparsable data', this]);
    }

    return qurl;
};
