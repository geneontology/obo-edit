#!/usr/bin/env my_rhino
/* 
 * Package: get_associations.js
 * 
 * Namespace: NONE
 * 
 * This is a Rhino script.
 * 
 * Get the ids and labels of the transitive associations of the specified term.
 * 
 * Usage like: "get_associations.js 1000 GO:0022008"
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
//print(arguments.length);
var count = arguments[arguments.length -2];
var term_acc = arguments[arguments.length -1];

// Get the environment.
var gconf = new bbop.golr.conf(bbop.amigo.golr_meta);
var go = new bbop.golr.manager.rhino('http://golr.berkeleybop.org/', gconf);
go.set('rows', count);
go.DEBUG = false;

// Filter setup.
go.set_personality('bbop_ann');
go.add_query_filter('document_category', 'annotation');
go.add_query_filter('isa_partof_closure', term_acc);

// Loop over the fetched docs.
var loop = bbop.core.each;
loop(bbop.golr.response.documents(go.fetch()),
     function(doc){
	 print(doc['bioentity'] + ' ' +
	       doc['bioentity_label'] + ' ' +
	       doc['evidence_type'] + ' ' +
	       doc['annotation_class'] + ' ' +
	       doc['annotation_class_label']);
     });
