////
//// Can we cram the whole GO in?
////
//// BUG: works interactive, but not from CLI--huh!?
//// Otherwise, seems to be alright so far...
////
//// Usage:
////    Command line: "js -f model.tests.go.js"
////    Interactive: "js -f model.tests.go.js -f -"
////


// Load testing.
load('test.js');
var mr_t = new bbop.test();

// Correct environment.
load('model.js');
load('data/go.js');

///
/// Start unit testing.
///

// 
g = new bbop.model.graph();

// Add all nodes.
//print('all nodes len: ' + bbop.model.go.nodes.length);
for( var n = 0; n < bbop.model.go.nodes.length; n++ ){
    var jnode = bbop.model.go.nodes[n];
    print('index: ' + n + ' jnode: ' + jnode['id']);
    g.add_node(new bbop.model.node(jnode['id']));
}

// Add all edges.
// print('all edges len: ' + bbop.model.go.edges.length);
for( var e = 0; e < bbop.model.go.edges.length; e++ ){
    var jedge = bbop.model.go.edges[e];
    print('index: ' + e);
    g.add_edge(new bbop.model.edge(jedge['subject'],jedge['object']));
}

//
mr_t.is_same_atom(3, g.get_roots().length, 'right number of roots');

///
/// End unit testing.
///

// Final report.
mr_t.report();
