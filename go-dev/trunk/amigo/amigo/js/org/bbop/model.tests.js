////
//// Some unit testing for model.js
////
//// Usage:
////    Command line: "smjs -f model.tests.js"
////    Interactive: "smjs -f model.tests.js -f -"
////


// Load testing.
load('test.js');
var mr_t = new org.bbop.test();

// Correct environment.
load('model.js');

///
/// Start unit testing.
///

// Check simple modelling.
    var g = new org.bbop.model.graph();
(function(){

    // Setup.

    // Singleton. Root?
    g.add_node(new org.bbop.model.node('z'));

    // Dangling subject.
    // g.add_link(new org.bbop.model.link('y', 'is_a', 'z'));

    // Standard graph.
    g.add_node(new org.bbop.model.node('a'));
    g.add_node(new org.bbop.model.node('b'));
    g.add_node(new org.bbop.model.node('c'));
    g.add_node(new org.bbop.model.node('d'));
    g.add_node(new org.bbop.model.node('e'));
    g.add_node(new org.bbop.model.node('n'));
    g.add_link(new org.bbop.model.link('b', 'is_a', 'a'));
    g.add_link(new org.bbop.model.link('c', 'is_a', 'a'));
    g.add_link(new org.bbop.model.link('d', 'is_a', 'c'));
    g.add_link(new org.bbop.model.link('d', 'is_a', 'n'));
    g.add_link(new org.bbop.model.link('e', 'is_a', 'c'));
    
    // Test graph.
    mr_t.is_defined(g, 'at least this would be nice');    

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
    mr_t.is_same_atom(7, g.get_nodes().length, 'seven links');
    mr_t.is_same_atom(false, g.is_leaf('c', 'is_a'), 'not leaf c');

    // Test link correctness.  
    mr_t.is_same_atom(5, g.get_links().length, 'five links');

})();

///
/// End unit testing.
///

// Final report.
mr_t.report();
