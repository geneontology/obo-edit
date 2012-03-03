////
//// Some unit testing for test.js
////
//// Usage:
////    Command line: "js -f test.tests.js"
////    Interactive: "js -f test.tests.js -f -"
////
//// Notes: cannot depend on core.js
////


// Load testing.
load('test.js');
var mr_t = new bbop.test();

///
/// Start unit testing.
///

//
mr_t.is_defined({});

//
var a = "foo";
var b = 2;
mr_t.is_same_atom(true, true);
mr_t.is_same_atom(a, 'foo');
mr_t.is_same_atom(b, 2);

//
mr_t.is_true(! false);

//
mr_t.is_same_url("completion?format=amigo&type=general&query=",
		 "completion?query=&format=amigo&type=general",
		 "link compare testing 1");
mr_t.is_different_url("completion?type=general&query=",
		      "completion?type=general&format=&query=",
		      "link compare testing 2");

//
var a_hash = {foo: 1, bar: 2};
mr_t.is_same_hash({},{});
mr_t.is_same_hash({foo: 1}, {foo: 1});
mr_t.is_same_hash(a_hash, {foo: 1, bar: 2});
mr_t.is_different_hash({foo: 1},{});
mr_t.is_different_hash({foo: 1}, {foo: 12});
mr_t.is_different_hash(a_hash, {foo: 1, bar: 2, blah: 3});

///
/// End unit testing.
///

// Final report.
mr_t.report();
