////
//// The GO makes a nice test case.
////
//// BUG: works interactive, but not from CLI--huh!?
//// Otherwise, seems to be alright so far...
//// See: http://coachwei.sys-con.com/node/676073/mobile
////
//// Usage (Java BUG: Force interpretation):
////    Command line: "js -opt -1 -f model.tests.go.js"
////    Interactive: "js -opt -1 -f model.tests.go.js -f -"
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

// Global testing graph.
g = new bbop.model.graph();

// Add all nodes.
//print('all nodes len: ' + bbop.model.go.nodes.length);
for( var n = 0; n < bbop.model.go.nodes.length; n++ ){
    var jnode = bbop.model.go.nodes[n];
    //print('index: ' + n + ' jnode: ' + jnode['id']);
    g.add_node(new bbop.model.node(jnode['id']));
}

// Add all edges.
// print('all edges len: ' + bbop.model.go.edges.length);
for( var e = 0; e < bbop.model.go.edges.length; e++ ){
    var jedge = bbop.model.go.edges[e];
    //print('index: ' + e);
    g.add_edge(new bbop.model.edge(jedge['subject'],jedge['object']));
}

//
mr_t.is_same_atom(3, g.get_root_nodes().length, 'right number of GO roots');
mr_t.is_same_atom(false, g.is_leaf('GO:0022008'), 'neurogenesis not a leaf');
mr_t.is_same_atom(true, g.is_leaf('GO:0048174'), 'but this should be');

// Let's get serious about parents.
var p_hash = {};
var parents = g.get_parent_nodes('GO:0022008');
for( var i = 0; i < parents.length; i++ ){
    p_hash[ parents[i].id() ] = true;
}
mr_t.is_same_atom(2, parents.length, '2 parents');
mr_t.is_same_atom(true, p_hash['GO:0007399'], 'has 1 of 2');
mr_t.is_same_atom(true, p_hash['GO:0030154'], 'has 2 of 2');

// Let's get serious about children.
var c_hash = {};
var children = g.get_child_nodes('GO:0022008');
for( var j = 0; j < children.length; j++ ){
    c_hash[ children[j].id() ] = true;
}
mr_t.is_same_atom(5, g.get_child_nodes('GO:0022008').length, '5 kids');
mr_t.is_same_atom(true, c_hash['GO:0048699'], 'has 1 of 5');
mr_t.is_same_atom(true, c_hash['GO:0042063'], 'has 2 of 5');
mr_t.is_same_atom(true, c_hash['GO:0050768'], 'has 3 of 5');
mr_t.is_same_atom(true, c_hash['GO:0050769'], 'has 4 of 5');
mr_t.is_same_atom(true, c_hash['GO:0050767'], 'has 5 of 5');

///
/// End unit testing.
///

// Final report.
mr_t.report();
