#!/usr/bin/env my_rhino
/* 
 * Package: shared_annotation_count.js
 * 
 * This is a Rhino script.
 * 
 * Return the number of shared annotations between all pairs of listed
 * terms.
 * 
 * Usage like:
 * : shared_annotation_count.js GO:0043473 GO:0009987 GO:0022008 ...
 */

// Loading the necessary files.
// TODO/BUG: These should be pointing at the remote files, not the
// local ones.
load('../staging/bbop.js');
load('../_data/golr.js');

// First, collect all of our input.
var term_accs = [];
for(var arg_i = 0; arg_i < arguments.length; arg_i++){
    var arg_acc = arguments[arg_i];
    term_accs.push(arg_acc);
}

// Next, setup the manager environment.
var gconf = new bbop.golr.conf(amigo.data.golr);
var go = new bbop.golr.manager.rhino('http://golr.berkeleybop.org/', gconf);
go.add_query_filter('document_category', 'annotation', ['*']);
go.set_personality('bbop_ann');
//go.debug(false); // I think the default is still on?

// Now, cycle though all of the posible pairs of terms while setting
// and unsetting the query filter on the manager. Print the output as
// we progress.
for(var v_i = 0; v_i < term_accs.length; v_i++){
    for(var h_i = 0; h_i < v_i; h_i++){
	var v = term_accs[v_i];
	var h = term_accs[h_i];

	// Set the next query.
	go.reset_query_filters(); // reset from the last iteration
	go.add_query_filter('isa_partof_closure', v);
	go.add_query_filter('isa_partof_closure', h);

	// Fetch the data and grab the number we want.
	var resp = new bbop.golr.response(go.fetch());
	var count = resp.total_documents();
	print(h + ', ' + v + ': ' + count);
   }
}
