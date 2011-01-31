////
//// Some unit testing for workspace.js
////
//// Usage:
////    Command line: "smjs -f workspace.tests.js -f"
////    Interactive: "smjs -f workspace.tests.js -f -"
////

// Load testing.
load('../test.js');
var mr_t = new org.bbop.test();

// Correct environment.
load('../amigo.js');
load('../amigo/go_meta.js');
load('workspace.js');

///
/// Start unit testing.
///

// Test data. TODO: Split to separate file.
var r0 =
    {"success": 1,
     "errors": [],
     "results": {"default": []}};
var r1 =
    {"success": 1,
     "errors": [],
     "results": {"default": [],
		    "new": [],
		    "foo": [
			{"date": "2009-05-21 01:39:07",
			 "name": "",
			 "type": "term",
			 "key": "GO:123"},
			{"date":" 2009-05-21 01:39:24",
			 "name": "",
			 "type": "term",
			 "key": "GO:456"}
		    ]
		   }};

var core = new org.bbop.amigo.core();
var d0 = core.response.results(r0);
var d1 = core.response.results(r1);

// Create workspaces.
var ws0 = new org.bbop.amigo.workspace(d0);
mr_t.is_defined(ws0);
mr_t.atom_compare(1, ws0.list_workspaces().length, '');
mr_t.atom_compare('default', ws0.list_workspaces()[0], '');
mr_t.atom_compare(0, ws0.list_items('default').length, '');
mr_t.atom_compare(0, ws0.list_items('foo').length, '');

var ws1 = new org.bbop.amigo.workspace(d1);
mr_t.atom_compare(3, ws1.list_workspaces().length, '');
mr_t.atom_compare(2, ws1.list_items('foo').length, 'ws should have two items');
mr_t.atom_compare('GO:123', ws1.list_items('foo')[0].key, 'key of first item');

///
/// End unit testing.
///

// Final report.
mr_t.report();
