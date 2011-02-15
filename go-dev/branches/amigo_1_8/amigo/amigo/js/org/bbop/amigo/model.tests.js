////
//// Some unit testing for model.js
////
//// Usage:
////    Command line: "smjs -f model.tests.js"
////    Interactive: "smjs -f model.tests.js -f -"
////

// Load testing.
load('../test.js');
var mr_t = new org.bbop.test();

// Correct environment.
load('../amigo.js');
load('model.js');

///
/// Start unit testing by building graph.
///

var g = new org.bbop.amigo.model.graph();

// Singleton
g.addNode(new org.bbop.amigo.model.node('z'));

// Dangling subject and object.
g.addLink(new org.bbop.amigo.model.link('y', 'is_a', 'z'));

//
g.addNode(new org.bbop.amigo.model.node('a'));
g.addNode(new org.bbop.amigo.model.node('b'));
g.addNode(new org.bbop.amigo.model.node('c'));
g.addNode(new org.bbop.amigo.model.node('d'));
g.addNode(new org.bbop.amigo.model.node('e'));
g.addNode(new org.bbop.amigo.model.node('x'));

//
g.addLink(new org.bbop.amigo.model.link('b', 'is_a', 'a'));
g.addLink(new org.bbop.amigo.model.link('c', 'is_a', 'a'));
g.addLink(new org.bbop.amigo.model.link('d', 'is_a', 'c'));
g.addLink(new org.bbop.amigo.model.link('d', 'is_a', 'x'));
g.addLink(new org.bbop.amigo.model.link('e', 'is_a', 'c'));
g.addLink(new org.bbop.amigo.model.link('d', 'part_of', 'b'));
g.addLink(new org.bbop.amigo.model.link('e', 'part_of', 'd'));

//
var link = new org.bbop.amigo.model.link('c', 'part_of', 'x');
link.id('l');
g.addLink(link);

///
/// Examine graph.
///

//print('_5_: ' + g.getRoots().length);
//print('_6_: ' + g.getRoots('is_a').length);
//print('_7_: ' + g.getRoots('part_of').length);

//var nodez = g.getLeaves();
//for( var z = 0; z < nodez.length; z++ ){
//    print("  ___: " + nodez[z] + " : " + nodez[z].id());
//}
//print('___' + g.getNode('a'));

// Still looks bad...
//print('_8_: ' + g.getSingletons().length);

mr_t.atom_compare(7, g.getNodes().length, 'total nodes');
mr_t.atom_compare(9, g.getLinks().length, 'total links');
mr_t.atom_compare(2, g.getPredicates().length, 'total predicates');
mr_t.atom_compare(8, g.getExtantBodies().length, 'total bodies');
mr_t.atom_compare('a', g.getNode('a').id(), "get id 'a'");
mr_t.atom_compare(2, g.getChildren(g.getNode('a').id(), 'is_a').length, 'kids');
mr_t.atom_compare(3, g.getRoots().length, 'all roots');

// This one is strange, but true (you might think that "z" or "y"
// should appear as well instead of just "e")--it has to do with the
// way that we handle "extant" vs. "named" bodies back from when this
// was the OBD model. If you don't want stuff like this, make sure
// your data reflects a closed universe.
mr_t.atom_compare(1, g.getLeaves().length, 'all leaves');

//
mr_t.report();
