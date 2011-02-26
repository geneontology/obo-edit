////
//// Some unit testing for amigo.js
////
//// Usage:
////    Command line: "smjs -f newick.tests.js"
////    Interactive: "smjs -f newick.tests.js -f -"
////

// TODO/BUG: Remove absolute path once this is all folded into the
// main AmiGO JS area.
// Load testing.
load('/home/sjcarbon/local/src/svn/geneontology/javascript/graph/test.js');
var mr_t = new bbop.test();

// Libs to test.
load('json2.js');
load('NewickTree.js');
load('NewickTreeUtils.js');

// Test set 1.
(function(){

    // Distances and all names (from Wikipedia on Newick).
    var tree_str = '(A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F;'
    var tree = NewickTreeUtils.parseNewick(tree_str);

    // Tree checks.
    mr_t.is_same_atom('F', tree.getRoot().id,
		      "root id is F");
    mr_t.is_same_atom(264, NewickTreeUtils.toJSON(tree).length,
		      "consistent JSON");

    var n = new NewickNode('F');

    // Node checks.
    mr_t.is_same_atom('F', n.getId(),
		      "node id is F");
    
})();

///
/// End unit testing.
///

// Final report.
mr_t.report();
