#!/usr/bin/env my_rhino
/* 
 * Package: get_document.js
 * 
 * This is a Rhino script.
 * 
 * Get the document with the specified id.
 * Optionally, if a profile is added, it will do a minimal (.lite())
 * return set using that.
 * 
 * Usage like:
 *  : get_document.js GO:0022008
 *  : get_document.js GO:0022008 bbop_ont
 */

// Loading the necessary files.
// TODO/BUG: These should be pointing at the remote files, not the
// local ones.
load('../staging/bbop.js');
load('../_data/golr.js');
load('../_data/server.js');

// Setup environment.
var acc = arguments[0];
var opt = arguments[1];
var gconf = new bbop.golr.conf(amigo.data.golr);
var gserv = new amigo.data.server();
var go = new bbop.golr.manager.rhino(gserv.golr_base(), gconf);
go.set_id(acc);
//go.debug(true);
if( bbop.core.is_defined(opt) ){
    //print('opt: ' + opt);
    go.set_personality(opt);
    go.lite(true);
}

// Get data.
var json_data = go.fetch();
var resp = new bbop.golr.response(json_data);
var doc = resp.get_doc(0);

// Dump data.
//print(bbop.core.dump(doc));
var loop = bbop.core.each;
loop(doc,
     function(key, val){
	 if( bbop.core.is_array(val) ){
	     loop(val,
		  function(item, index){
		      print(key + "\t" + item);
		  });
	 }else{
	     print(key + "\t" + val);
	 }
     });
