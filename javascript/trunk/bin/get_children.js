#!/home/sjcarbon/local/src/tarballs/node-v0.8.10-linux-x64/bin/node
/* 
 * Package: get_children.js
 * 
 * Namespace: NONE
 * 
 * This is a NodeJS script.
 * 
 * Get the ids and labels of the children of the specified node.
 * 
 * Usage like: "get_children.js GO:0022008"
 * 
 * TODO: Maybe NodeJS isn't such a hot idea for scripting; how about
 * Rhino?
 */

// Loading the necessary files.
// TODO/BUG: These should be pointing at the remote files, not the
// local ones.
require('./../lib/bbop/core');
require('./../lib/bbop/logger');
require('./../lib/bbop/registry');
require('./../lib/bbop/golr_conf');
require('./../lib/bbop/golr_response');
require('./../lib/bbop/golr_manager');
require('./../lib/bbop/golr_manager_nodejs');
require('./../lib/bbop/model');
require('./../../../AmiGO/trunk/javascript/bbop/amigo');
require('./../../../AmiGO/trunk/javascript/bbop/amigo/golr_meta');
require('./../../../AmiGO/trunk/javascript/bbop/amigo/amigo_meta');

// First, get the last arg
//console.log(process.argv.length);
//console.log(process.argv[process.argv.length -1]);
var term_acc = process.argv[process.argv.length -1];

// Define what we do when our (async) information comes back.
function report(json_data){

    // Gather out info graph info from the first doc.
    var doc = bbop.golr.response.documents(json_data)[0];
    var graph_json = doc['topology_graph'];
    var graph = new bbop.model.graph();
    graph.load_json(JSON.parse(graph_json));
    var kids = graph.get_child_nodes(term_acc);

    // Dump to STDOUT.
    //console.log(kids);
    var loop = bbop.core.each;
    loop(kids,
	 function(kid){
	     process.stdout.write(kid.id() + "\t" + kid.label() + "\n");
	 });
}

// Define the server, define the query, bind the callback, and
// trigger.
gconf = new bbop.golr.conf(bbop.amigo.golr_meta);
go = new bbop.golr.manager.nodejs('http://golr.berkeleybop.org/', gconf);
go.set_id(term_acc);
go.register('search', 'do', report);
go.update('search');
