////
//// Benchmark the speeds for various ways of compiling strings.
////
//// Usage:
////    smjs -f string-concat-bench.js
////    rhino -f string-concat-bench.js
////
//// Results for random input (20110830):
////    smjs: plus_equals_rand
////    rhino: pre_array_rand -> dynamic_array_rand
////
//// NOTE: As an issue in smjs (1.7.0), when I use the static array
//// method of caching, I can actually cause a complete meltdown.
////
//// NOTE: rhino doesn't get past 1000000 very well, sticks on first
//// plus_equals.
////
//// See "COMPLETION NOTE".
////

///
/// Utility functions.
///

function Stopwatch (){
    var start_date = new Date();
    var end_date = new Date();

    this.start = function(){
	start_date = new Date();	
    };

    this.stop = function(){
	end_date = new Date();	
    };

    this.elapsed = function(){
	return end_date.getTime() - start_date.getTime();
    };
}

// Return a random number string of length len.
var random_base =
    ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
     'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];
function _randstr_pre_array(len){
    var length = 10;
    if( len ){
	length = len;
    }
    var cache = new Array(length);
    for( var ii = 0; ii < length; ii++ ){
	var rbase_index = Math.floor(Math.random() * random_base.length);
	cache[ii] = random_base[rbase_index];
    }
    return cache.join('');
};
function _randstr_plus(len){
    var length = 10;
    if( len ){
	length = len;
    }
    var cache = '';
    for( var ii = 0; ii < length; ii++ ){
	var rbase_index = Math.floor(Math.random() * random_base.length);
	cache += random_base[rbase_index];
    }
    return cache;
};
// COMPLETION NOTE: Change this in order to actually finish the
// benchmark in certain circumstances.
//var randstr = _randstr_plus;
var randstr = _randstr_pre_array;


// Best guess logging for various platforms.
function Logger(){

    // We'll start with print because we're doing stuff from the
    // command line in smjs, but we'll work our way out and see if we
    // have a browser environment.
    var sayer = function(){};
    var ender = '';

    // Check for: Opera, FF, Safari, etc.
    if( typeof(opera) != 'undefined' &&
	typeof(opera.postError) != 'undefined' ){
	sayer = opera.postError;
	ender = "\n";
    }else if( typeof(window) != 'undefined' &&
	      typeof(window.dump) != 'undefined' ){
	// From developer.mozilla.org: To see the dump output you have
	// to enable it by setting the preference
	// browser.dom.window.dump.enabled to true. You can set the
	// preference in about:config or in a user.js file. Note: this
	// preference is not listed in about:config by default, you
	// may need to create it (right-click the content area -> New
	// -> Boolean).
	sayer = dump;
	ender = "\n";
    }else if( typeof(window) != 'undefined' &&
	      typeof(window.console) != 'undefined' &&
	      typeof(window.console.log) != 'undefined' ){
	// From developer.apple.com: Safari's "Debug" menu allows you to
	// turn on the logging of JavaScript errors. To display the
	// debug menu in Mac OS X, open a Terminal window and type:
	// "defaults write com.apple.Safari IncludeDebugMenu 1"
	// Need the wrapper function because safari has personality
	// problems.
	sayer = function(msg){ window.console.log(msg); };
	ender = "\n";
    }else if( typeof(console) != 'undefined' &&
	      typeof(console.log) != 'undefined' ){
	// This may be okay for Chrome...
	sayer = console.log;
	ender = "\n";
    }else if( typeof(build) == 'function' &&
	      typeof(getpda) == 'function' &&
	      typeof(pc2line) == 'function' &&
	      typeof(print) == 'function' ){
	// This may detect SpiderMonkey on the comand line.
	sayer = print;
	ender = "";
    }else if( typeof(org) != 'undefined' &&
	      typeof(org.rhino) != 'undefined' &&
	      typeof(print) == 'function' ){
	// This may detect Rhino on the comand line.
	sayer = print;
	ender = "";
    }

    this.log = function(string){
	if( typeof(string) == 'undefined' ){
	    string = '';
	}
	sayer(string + ender);
    };
};

///
/// The things to be benchmarked:
///

// We'll loop all of our functions through this.
var style = {};

// Concat with the default operator and a static string.
style.plus_equals_static = function (times){

    var nstr = '';
    var addition = '0123456789';
    for( var i = 0; i < times; i++){
	nstr += addition;
    }

    return nstr;
};

// Concat with the default operator and a random string.
style.plus_equals_rand = function (times){

    var nstr = '';
    for( var i = 0; i < times; i++){
	nstr += randstr(10);
    }

    return nstr;
};

// Push dynamic array.
style.dynamic_array_static = function (times){

    var addition = '0123456789';
    var mbuf = [];
    for( var i = 0; i < times; i++){
	mbuf.push(addition);
    }
    var nstr = mbuf.join('');

    return nstr;
};

// Push dynamic array with random string.
style.dynamic_array_rand = function (times){

    var mbuf = [];
    for( var i = 0; i < times; i++){
	mbuf.push(randstr());
	// if( i % 10000 == 0 ){
	//     print("(in " + i + ")");
	// }
    }
    var nstr = mbuf.join('');

    return nstr;
};

// Push known array size with a static string.
style.pre_array_static = function (times){
    var watch = new Stopwatch();
    watch.start();

    var addition = '0123456789';
    var mbuf = new Array(times);
    for( var i = 0; i < times; i++){
	mbuf[i] = addition;
    }
    var nstr = mbuf.join('');

    return nstr;
};

// Push known array size with a dynamic string.
style.pre_array_rand = function (times){
    var watch = new Stopwatch();
    watch.start();

    var mbuf = new Array(times);
    for( var i = 0; i < times; i++){
	mbuf[i] = randstr();
    }
    var nstr = mbuf.join('');

    return nstr;
};

///
/// Actual benchmarking.
///

(function benchmark(){

     var l = new Logger();
     var steps = [1, 10, 100, 1000, 10000, 100000, 1000000];


     for(var i = 0; i < steps.length; i++){
	 var curr_step = steps[i];

	 l.log(curr_step + ":");
	 for (s in style){
	     
	     var watch = new Stopwatch();
	     watch.start();
	     
	     style[s](curr_step);
	     
	     watch.stop();
	     l.log((watch.elapsed() / 1000.0) + 's with ' + s);
	 }
	 l.log();
     }
 })();
