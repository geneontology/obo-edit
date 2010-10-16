////
//// Some unit testing for model.js
////
//// Usage:
////    Command line: "js -f model.tests.js"
////    Interactive: "js -f model.tests.js -f -"
////


// Load testing.
load('test.js');
var mr_t = new bbop.test();

// Correct environment.
load('model.js');

///
/// Start unit testing.
///

// Nodes.
(function(){

     // Setup.
     var n1 = new bbop.model.node('a');

     mr_t.is_defined(n1, 'at least this would be nice (node)');
  
     mr_t.is_same_atom('a', n1.id(), 'is a');
     mr_t.is_same_atom('node', n1.type(), 'is a node');

     n1.id('b');

     mr_t.is_same_atom('b', n1.id(), 'is b');

 })();

// Edges.
(function(){

     // Setup.
     var n1 = new bbop.model.node('a');
     var n2 = new bbop.model.node('b');
     var n3 = new bbop.model.node('c');

     var e1 = new bbop.model.edge(n1, n2);
     var e2 = new bbop.model.edge(n2, n3, 'foo');
     var e3 = new bbop.model.edge('d', 'e', 'bar');

     mr_t.is_defined(e1, 'at least this would be nice (edge 1)');
     mr_t.is_defined(e2, 'at least this would be nice (edge 2)');
     mr_t.is_defined(e2, 'at least this would be nice (edge 3)');
  
     mr_t.is_same_atom('a', e1.subject_id(), 'is a');
     mr_t.is_same_atom('b', e1.object_id(), 'is b');
     mr_t.is_same_atom('e', e3.object_id(), 'is e');

     mr_t.is_same_atom(bbop.model.default_predicate, e1.predicate_id(), 'is p');
     mr_t.is_same_atom('foo', e2.predicate_id(), 'is p 2');

 })();

// Check simple modelling.
(function(){

     // Setup.
     var g = new bbop.model.graph();

     // Singleton. Root?
     g.add_node(new bbop.model.node('z'));

     // Standard graph.
     g.add_node(new bbop.model.node('a'));
     g.add_node(new bbop.model.node('b'));
     g.add_node(new bbop.model.node('c'));
     g.add_node(new bbop.model.node('d'));
     g.add_node(new bbop.model.node('e'));
     g.add_node(new bbop.model.node('n'));
     g.add_edge(new bbop.model.edge('b', 'a', 'is_a'));
     g.add_edge(new bbop.model.edge('c', 'a', 'is_a'));
     g.add_edge(new bbop.model.edge('d', 'c', 'is_a'));
     g.add_edge(new bbop.model.edge('d', 'n', 'is_a'));
     g.add_edge(new bbop.model.edge('e', 'c', 'is_a'));
     
     // Dangling subject.
     // g.add_edge(new bbop.model.edge('y', 'z', 'is_a'));

     // Test graph.
     mr_t.is_defined(g, 'at least this would be nice (graph)');

     // Test leaf correctness.  
     mr_t.is_same_atom(4, g.get_leaves('is_a').length, 'four leaves');
     mr_t.is_same_atom(true, g.is_leaf('b', 'is_a'), 'leaf b');
     mr_t.is_same_atom(true, g.is_leaf('d', 'is_a'), 'leaf d');
     mr_t.is_same_atom(true, g.is_leaf('e', 'is_a'), 'leaf e');
     mr_t.is_same_atom(true, g.is_leaf('z', 'is_a'), 'leaf z');
     mr_t.is_same_atom(false, g.is_leaf('n', 'is_a'), 'not leaf n');
     mr_t.is_same_atom(false, g.is_leaf('a', 'is_a'), 'not leaf a');
     mr_t.is_same_atom(false, g.is_leaf('c', 'is_a'), 'not leaf c');

     // Test node correctness.  
     mr_t.is_same_atom(7, g.get_nodes().length, 'seven edges');
     mr_t.is_same_atom(false, g.is_leaf('c', 'is_a'), 'not leaf c');

     // Test edge correctness.  
     mr_t.is_same_atom(5, g.get_edges().length, 'five edges');

 })();

///
/// End unit testing.
///

// Final report.
mr_t.report();
