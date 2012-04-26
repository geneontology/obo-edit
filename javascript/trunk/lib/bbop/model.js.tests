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
load('core.js');
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

     // Create graph described below.
     //
     //      a   n   x   z  
     //     / \  |   |
     //    b   c |   y?  <-- y is not extant, just referenced
     //   ||  / \|
     //   || e   d
     //    \\___//  <-- non-default relationship (d is_a b)
     //     \---/
     //
     var g = new bbop.model.graph();
     g.add_node(new bbop.model.node('a'));
     g.add_node(new bbop.model.node('b'));
     g.add_node(new bbop.model.node('c'));
     g.add_node(new bbop.model.node('d'));
     g.add_node(new bbop.model.node('e'));
     g.add_node(new bbop.model.node('n'));
     g.add_node(new bbop.model.node('x'));
     g.add_node(new bbop.model.node('z'));
     g.add_edge(new bbop.model.edge('b', 'a'));
     g.add_edge(new bbop.model.edge('c', 'a'));
     g.add_edge(new bbop.model.edge('d', 'c'));
     g.add_edge(new bbop.model.edge('e', 'c'));
     g.add_edge(new bbop.model.edge('d', 'n'));
     g.add_edge(new bbop.model.edge('d', 'b', 'is_a'));
     g.add_edge(new bbop.model.edge('y', 'x'));

     var dpred = bbop.model.default_predicate;

     // Test graph construction.
     mr_t.is_defined(g, 'at least this would be nice (graph)');
     mr_t.is_same_atom(8, g.all_nodes().length, '7 nodes');
     mr_t.is_same_atom(7, g.all_edges().length, '7 edges');
     mr_t.is_same_atom(7, g.all_edges().length, '7 edges');
     mr_t.is_same_atom(1, g.get_singleton_nodes().length, 'just one single');
     mr_t.is_same_atom('z', g.get_singleton_nodes()[0].id(), 'z alone');
     mr_t.is_same_atom(1, g.all_dangling().length, 'just one dangle');
     mr_t.is_same_atom(false, g.is_complete(), 'nope'); 

     // Test leaf correctness.
     mr_t.is_same_atom(false, g.is_leaf_node('a'), '! leaf a');
     mr_t.is_same_atom(false, g.is_leaf_node('b'), '! leaf b');
     mr_t.is_same_atom(false, g.is_leaf_node('c'), '! leaf c');
     mr_t.is_same_atom(true, g.is_leaf_node('d'), 'leaf d');
     mr_t.is_same_atom(true, g.is_leaf_node('e'), 'leaf e');
     mr_t.is_same_atom(false, g.is_leaf_node('n'), '! leaf n');
     mr_t.is_same_atom(false, g.is_leaf_node('x'), '! leaf z');
     mr_t.is_same_atom(false, g.is_leaf_node('y'), '! leaf y');
     mr_t.is_same_atom(true, g.is_leaf_node('z'), 'leaf z');
     mr_t.is_same_atom(3, g.get_leaf_nodes().length, '3 leaves');

     // Test roots.
     mr_t.is_same_atom(true, g.is_root_node('a'), 'root a');
     mr_t.is_same_atom(false, g.is_root_node('b'), '! root b');
     mr_t.is_same_atom(false, g.is_root_node('c'), '! root c');
     mr_t.is_same_atom(false, g.is_root_node('d'), '! root d');
     mr_t.is_same_atom(false, g.is_root_node('e'), '! root e');
     mr_t.is_same_atom(true, g.is_root_node('n'), 'root n');
     mr_t.is_same_atom(true, g.is_root_node('x'), 'root z');
     mr_t.is_same_atom(false, g.is_root_node('y'), '! root y');
     mr_t.is_same_atom(true, g.is_root_node('z'), 'root z');
     mr_t.is_same_atom(4, g.get_root_nodes().length, '4 roots');

     // Test graph structure up.
     mr_t.is_same_atom(0, g.get_parent_nodes('a').length, 'a is root');
     mr_t.is_same_atom(1, g.get_parent_nodes('b').length, 'b under a (1)');
     mr_t.is_same_atom('a', g.get_parent_nodes('b')[0].id(), 'b under a (2)');
     mr_t.is_same_atom(3, g.get_parent_nodes('d').length, 'd: b c n');
     mr_t.is_same_atom(2, g.get_parent_nodes('d', dpred).length, 'd: c n');
     mr_t.is_same_atom(1, g.get_parent_nodes('d', 'is_a').length, 'd: b');

     // Test graph structure down.
     mr_t.is_same_atom(2, g.get_child_nodes('a').length, 'a has 2');
     mr_t.is_same_atom(1, g.get_child_nodes('b').length, 'b has 1');
     mr_t.is_same_atom('d', g.get_child_nodes('b')[0].id(), 'b: d 1');
     mr_t.is_same_atom(0, g.get_child_nodes('b', dpred).length, 'b: d 2');
     mr_t.is_same_atom('d', g.get_child_nodes('b', 'is_a')[0].id(), 'b: d 3');
     mr_t.is_same_atom(0, g.get_child_nodes('d').length, 'd: -');
     mr_t.is_same_atom(0, g.get_child_nodes('z').length, 'z: -');
     mr_t.is_same_atom(0, g.get_child_nodes('x').length, 'x: -');

     ///
     /// Test subgraphs.
     ///

     var sub1 = g.get_ancestor_subgraph('d');
     // Roots.
     mr_t.is_same_atom(true, sub1.is_root_node('a'), 'root a');
     mr_t.is_same_atom(true, sub1.is_root_node('n'), 'root n');
     mr_t.is_same_atom(false, sub1.is_root_node('x'), '! root x');
     mr_t.is_same_atom(2, sub1.get_root_nodes().length, '2 roots');
     // Leaves.
     mr_t.is_same_atom(1, sub1.get_leaf_nodes().length, '1 leaf');
     mr_t.is_same_atom('d', sub1.get_leaf_nodes()[0].id(), 'd leaf');
     // Graph structure up.
     mr_t.is_same_atom(0, sub1.get_parent_nodes('a').length, 'a is root');
     mr_t.is_same_atom(1, sub1.get_parent_nodes('b').length, 'b under a (1)');
     mr_t.is_same_atom('a', sub1.get_parent_nodes('b')[0].id(), 'b under a (2)');
     mr_t.is_same_atom(3, sub1.get_parent_nodes('d').length, 'd: b c n');
     mr_t.is_same_atom(2, sub1.get_parent_nodes('d', dpred).length, 'd: c n');
     mr_t.is_same_atom(1, sub1.get_parent_nodes('d', 'is_a').length, 'd: b');
     // Graph structure down.
     mr_t.is_same_atom(2, sub1.get_child_nodes('a').length, 'a has 2');
     mr_t.is_same_atom(1, sub1.get_child_nodes('b').length, 'b has 1');
     mr_t.is_same_atom('d', sub1.get_child_nodes('b')[0].id(), 'b: d 1');
     mr_t.is_same_atom(0, sub1.get_child_nodes('b', dpred).length, 'b: d 2');
     mr_t.is_same_atom('d', sub1.get_child_nodes('b', 'is_a')[0].id(), 'b: d 3');
     mr_t.is_same_atom(0, sub1.get_child_nodes('d').length, 'd: -');

     var sub2 = g.get_ancestor_subgraph('d', 'is_a');
     // Roots.
     mr_t.is_same_atom(false, sub2.is_root_node('a'), '! root a');
     mr_t.is_same_atom(false, sub2.is_root_node('d'), '! root d');
     mr_t.is_same_atom(true, sub2.is_root_node('b'), 'root b');
     // Leaves.
     mr_t.is_same_atom(1, sub2.get_leaf_nodes().length, '1 leaf');
     mr_t.is_same_atom('d', sub2.get_leaf_nodes()[0].id(), 'd leaf');
     // Graph structure up.
     mr_t.is_same_atom(0, sub2.get_parent_nodes('b').length, 'b root');
     mr_t.is_same_atom(1, sub2.get_parent_nodes('d').length, 'd: b');
     mr_t.is_same_atom(0, sub2.get_parent_nodes('d', dpred).length, 'd: -');
     mr_t.is_same_atom(1, sub2.get_parent_nodes('d', 'is_a').length, 'd: b');
     // Graph structure down.
     mr_t.is_same_atom('d', sub2.get_child_nodes('b')[0].id(), 'b: d 1');
     mr_t.is_same_atom(0, sub2.get_child_nodes('b', dpred).length, 'b: d 2');
     mr_t.is_same_atom('d', sub2.get_child_nodes('b', 'is_a')[0].id(), 'b:d 3');
     mr_t.is_same_atom(0, sub2.get_child_nodes('d').length, 'd: -');

 })();

///
/// End unit testing.
///

// Final report.
mr_t.report();
