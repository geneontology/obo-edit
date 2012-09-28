#!/usr/bin/env my_rhino
/* 
 * Package: get_parents.js
 * 
 * Namespace: NONE
 * 
 * This is a Rhino script.
 * 
 * Get the ids and labels of the parents of the specified term.
 * 
 * Usage like: "get_parents.js GO:0022008"
 * 
 * This is also a bit of a unit test for the Rhino update function.
 */

// Loading the necessary files.
// TODO/BUG: These should be pointing at the remote files, not the
// local ones.
load('./../lib/bbop/core.js');
load('./../lib/bbop/logger.js');
load('./../lib/bbop/registry.js');
load('./../lib/bbop/golr_conf.js');
load('./../lib/bbop/golr_response.js');
load('./../lib/bbop/golr_manager.js');
load('./../lib/bbop/golr_manager_rhino.js');
load('./../lib/bbop/model.js');
load('./../../../AmiGO/trunk/javascript/bbop/amigo.js');
load('./../../../AmiGO/trunk/javascript/bbop/amigo/golr_meta.js');
load('./../../../AmiGO/trunk/javascript/bbop/amigo/amigo_meta.js');

// First, get the last arg
var term_acc = arguments[arguments.length -1];

// Define what we do when our (async) information comes back.
function report(json_data){

    // Gather out info graph info from the first doc.
    var doc = bbop.golr.response.documents(json_data)[0];
    var graph_json = doc['topology_graph'];
    var graph = new bbop.model.graph();
    graph.load_json(JSON.parse(graph_json));
    var kids = graph.get_parent_nodes(term_acc);

    // Dump to STDOUT.
    var loop = bbop.core.each;
    loop(kids,
	 function(kid){
	     print(kid.id() + "\t" + kid.label());
	 });
}

// Define the server, define the query, bind the callback, and
// trigger.
gconf = new bbop.golr.conf(bbop.amigo.golr_meta);
go = new bbop.golr.manager.rhino('http://golr.berkeleybop.org/', gconf);
go.set_id(term_acc);
go.register('search', 'do', report);
go.update('search');
