/* 
 * Package: nodejs.js
 * 
 * Namespace: bbop.golr.manager.nodejs
 * 
 * NodeJS BBOP manager for dealing with remote calls. Remember,
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
bbop.core.namespace('bbop', 'golr', 'manager', 'nodejs');

/*
 * Constructor: nodejs
 * 
 * Contructor for the GOlr query manager; NodeJS flavor. YMMV.
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
bbop.golr.manager.nodejs = function (golr_loc, golr_conf_obj){
//function GOlrManager(in_args){
    // We are a registry like this:
    bbop.golr.manager.call(this, golr_loc, golr_conf_obj);
    this._is_a = 'bbop.golr.manager.nodejs';

    // Get a good self-reference point.
    //var anchor = this;

    // Per-manager logger.
    //this._ll = ll;

    // //
    // ll('Alive.');

};
bbop.core.extend(bbop.golr.manager.nodejs, bbop.golr.manager);

/*
 * Function: update
 *
 *  See the documentation in <golr_manager.js> on update to get more
 *  of the story. This override function adds functionality to NodeJS.
 *
 * Parameters: 
 *  callback_type - callback type string
 *  rows - *[serially optional]* integer; the number of rows to return
 *  start - *[serially optional]* integer; the offset of the returned rows
 *
 * Returns:
 *  the query url (with any NodeJS specific paramteters)
 * 
 * Also see:
 *  <get_query_url>
 */
bbop.golr.manager.nodejs.prototype.update = function(callback_type,
						     rows, start){
    // Get "parents" url first.
    var parent_update = bbop.golr.manager.prototype.update;
    var qurl = parent_update.call(this, callback_type, rows, start);

    // 
    var logger = new bbop.logger(this._is_a);
    //this._logger = new bbop.logger(this._is_a);
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    var anchor = this;
    this.last = null;
    
    //
    function on_error(e) {
	console.log('problem with request: ' + e.message);
    }
    function on_connect(res){
	//console.log('STATUS: ' + res.statusCode);
	//console.log('HEADERS: ' + JSON.stringify(res.headers));
	res.setEncoding('utf8');
	var raw_data = '';
	res.on('data', function (chunk) {
		   //console.log('BODY: ' + chunk);
		   raw_data = raw_data + chunk;
	       });
	// Parse JS and call callback_type callbacks.
	res.on('end', function () {
		   var json_data = JSON.parse(raw_data);
		   anchor.last = json_data;
		   anchor.apply_callbacks(callback_type, [json_data, anchor]);
	       });
    }
    var http = require('http');
    var req = http.request(qurl, on_connect);
    req.on('error', on_error);
    
    // write data to request body
    //req.write('data\n');
    //req.write('data\n');
    req.end();
    
    return qurl;
};
