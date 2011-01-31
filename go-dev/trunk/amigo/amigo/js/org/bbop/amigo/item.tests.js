////
//// Some unit testing for item.js
////
//// Usage:
////    Command line: "smjs -f item.tests.js -f"
////    Interactive: "smjs -f item.tests.js -f -"
////

// Load testing.
load('../test.js');
var mr_t = new org.bbop.test();

// Correct environment.
load('../amigo.js');
load('../amigo/item.js');

///
/// Start unit testing.
///

//
var i1 = new org.bbop.amigo.item('1');
mr_t.is_defined(i1);
mr_t.atom_compare('1', i1.get_key(), '');
mr_t.atom_compare('', i1.get_name(), '');

var i2 = new org.bbop.amigo.item(2, 'foo');
mr_t.is_defined(i2);
mr_t.atom_compare(2, i2.get_key(), '');
mr_t.atom_compare('foo', i2.get_name(), '');

///
/// End unit testing.
///

// Final report.
mr_t.report();
