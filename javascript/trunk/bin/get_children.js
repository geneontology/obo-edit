#!/home/sjcarbon/local/src/tarballs/node-v0.8.10-linux-x64/bin/node
/* 
 * Package: get_children.js
 * 
 * This is a NodeJS script.
 * 
 * Get the ids and labels of the children of the specified term.
 * 
 * Usage like:
 *  : get_children.js GO:0022008
 * 
 * This is also a bit of a unit test for the NodeJS update function.
 * 
 * TODO: Maybe NodeJS isn't such a hot idea for scripting; how about
 * Rhino?
 */

// Loading the necessary files.
// TODO/BUG: These should be pointing at the remote files, not the
// local ones.
require('../staging/bbop');
require('../_data/golr');
//require('./../../../AmiGO/trunk/staging/bbop-amigo');

// First, get the last arg
//console.log(process.argv.length);
//console.log(process.argv[process.argv.length -1]);
var term_acc = process.argv[process.argv.length -1];

// Define what we do when our (async) information comes back.
function report(json_data){

    // Gather out info graph info from the first doc.
    var resp = new bbop.golr.response(json_data);
    var doc = resp.documents()[0];
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
gconf = new bbop.golr.conf(amigo.data.golr);
go = new bbop.golr.manager.nodejs('http://golr.berkeleybop.org/', gconf);
go.set_id(term_acc);
go.register('search', 'do', report);
go.update('search');
