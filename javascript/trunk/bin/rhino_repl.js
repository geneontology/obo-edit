#!/usr/bin/env my_rhino
/* 
 * Package: rhino_golr.js
 * 
 * This is a Rhino script.
 * 
 * Attempt at an interactive Rhino session.
 * Start a REPL in a sane GOlr environment.
 * 
 * Usage like:
 * : my_rhino -f rhino_repl.js -f -
 * 
 * REPL usage like: rhino_repl.js 
 * : golr> go.get_query_url()
 * : golr> go.set_personality('bbop_ann')
 * : golr> go.fetch()
 */

///
/// Create a sane GOlr environment.
///

// Loading the necessary file/url.
//eval(readUrl('https://cdn.berkeleybop.org/jsapi/bbop.js'));
load('../staging/bbop.js');
load('../_data/golr.js');

// Setup the environment a little bit.
var gconf = new bbop.golr.conf(amigo.data.golr);
var go = new bbop.golr.manager.rhino('http://golr.berkeleybop.org/', gconf);
//go.set_personality('bbop_ont');

var prompts = ['golr> ', '.....'];


// // Drop into a REPL, 'last' and 'quit' are special.
// importPackage(java.io);
// importPackage(java.lang);
// var last = null;
// //(function(){

//      var keep_going_p = true;
//      while( keep_going_p ){
	
// 	 // Prompt.
// 	 java.lang.System.out.print("golr> ");

// 	 // Read line (into red).
// 	 var isr = new InputStreamReader(System['in']);
// 	 var br = new BufferedReader(isr);
// 	 var red = br.readLine();

// 	 print('read: ' + red);

// 	 if( red == 'quit' ){
// 	     print('DONE');
// 	     keep_going_p = false;	    
// 	 }else{
	    
// 	     print('will eval: ' + red);
// 	     print('a: ' + typeof(red));

// 	     // Evaluate red.
// 	     last = eval(red);
	     
// 	     print('evalled to: ' + last);

// 	     // Print 
// 	     print(last);
// 	 }
//      }

// //})();
