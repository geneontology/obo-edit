////
//// Some unit testing for utilities.js
////
//// Usage:
////    Command line: "smjs -f utilities.tests.js"
////    Interactive: "smjs -f utilities.tests.js -f -"
////


// Load testing and env.
load('utilities.js');
load('test.js');
var mr_t = new org.bbop.test();

///
/// Start unit testing.
///

var util = new org.bbop.utilities();
//util.addGS();

var shell = function(){
    
};

var foo = new shell();
var bar = new shell();

print('foo: ' + foo);
print('foo: ' + foo.prototype);

util.addGS(foo, 'id1');
util.addGS(foo, 'id2');
util.addGS(bar, 'id3');
util.addGS(bar, 'id4');
