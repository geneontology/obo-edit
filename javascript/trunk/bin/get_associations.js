#!/usr/bin/env my_rhino
/* 
 * Package: get_associations.js
 * 
 * This is a Rhino script.
 * 
 * Get the ids and labels of the transitive associations of the specified term.
 * 
 * Usage like:
 * : get_associations.js GO:0022008 25
 */

// Loading the necessary files.
// TODO/BUG: These should be pointing at the remote files, not the
load('../staging/bbop.js');
load('../_data/golr.js');

// First, get the last arg
//print(arguments.length);
var term_acc = arguments[arguments.length -2];
var count = arguments[arguments.length -1];

// Get the environment.
var gconf = new bbop.golr.conf(amigo.data.golr);
var go = new bbop.golr.manager.rhino('http://golr.berkeleybop.org/', gconf);
go.set('rows', count);
go.debug(false); // I think the default is still on?

// Filter setup.
go.set_personality('bbop_ann');
go.add_query_filter('document_category', 'annotation');
go.add_query_filter('isa_partof_closure', term_acc);

// Loop over the fetched docs.
var loop = bbop.core.each;
var resp = new bbop.golr.response(go.fetch());
loop(resp.documents(go.fetch()),
     function(doc){
	 print(doc['bioentity'] + '\t' +
	       doc['bioentity_label'] + '\t' +
	       doc['evidence_type'] + '\t' +
	       doc['annotation_class'] + '\t' +
	       doc['annotation_class_label']);
     });
