////
//// Usage:
////    Command line: "js -f newick_parser.tests.js"
////    Interactive: "js -f newick_parser.tests.js -f -"
////


// Load testing.
load('test.js');
var mr_t = new bbop.test();

// Correct environment.
load('core.js');
load('model.js');
load('newick_parser.js');

///
/// Start unit testing.
///


// These are from: http://en.wikipedia.org/wiki/Newick_format
// TODO: parse them as described.
// (,,(,));                               no nodes are named
// (A,B,(C,D));                           leaf nodes are named
// (A,B,(C,D)E)F;                         all nodes are named
// (:0.1,:0.2,(:0.3,:0.4):0.5);           all but root node have a distance to parent
// (:0.1,:0.2,(:0.3,:0.4):0.5):0.0;       all have a distance to parent
// (A:0.1,B:0.2,(C:0.3,D:0.4):0.5);       distances and leaf names (popular)
// (A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F;     distances and all names
// ((B:0.2,(C:0.3,D:0.4)E:0.5)F:0.1)A;    a tree rooted on a leaf node (rare)



//
g = new bbop.model.graph();

test_trees = [
    'AN0(AN1(AN2(XP_800359:0.687,XP_790652:0.774,XP_800360:0.695):0.473,AN6(Q7RKB3:1.366,Q7RBF2:1.208):0.223):1.0,Q747I8:1.0);',
    '(,,(,));',
    '(A,B,(C,D));',
    '(A,B,(C,D)E)F;',
    '(:0.1,:0.2,(:0.3,:0.4):0.5);',
    '(:0.1,:0.2,(:0.3,:0.4):0.5):0.0;',
    '(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);',
    '(A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F;',
    '((B:0.2,(C:0.3,D:0.4)E:0.5)F:0.1)A;'
];
    
p = new bbop.parser.newick();
p.tokenize(n_str);



