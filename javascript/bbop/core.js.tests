////
//// Some unit testing for core.js
////
//// Usage:
////    Command line: "js -f core.tests.js"
////    Interactive: "js -f core.tests.js -f -"
////


// Load testing.
load('test.js');
var mr_t = new bbop.test();

// Correct environment.
load('core.js');

///
/// Start unit testing.
///

// Cloning.
var o1 = {'foo': 1, 'bar': 2};
var o2 = bbop.core.clone(o1);
mr_t.is_same_hash(o1, o2);

// Namespace generation.
bbop.core.namespace("happy", "bar");
mr_t.is_defined(happy.bar, "made namespace");
happy.bar.prop = true;
mr_t.is_same_atom(true, happy.bar.prop, "added prop to new NS");



///
/// End unit testing.
///

// Final report.
mr_t.report();
