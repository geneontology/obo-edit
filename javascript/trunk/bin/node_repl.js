#!/home/sjcarbon/local/src/tarballs/node-v0.8.10-linux-x64/bin/node
/* 
 * Package: repl.js
 * 
 * Namespace: NONE
 * 
 * This is a NodeJS script.
 * 
 * Start a REPL in a sane GOlr environment.
 * 
 * Usage like: node_repl.js 
 *  golr> go.get_query_url()
 *  golr> go.set_personality('bbop_ann')
 *  golr> go.get_query_url()
 */

// Loading the necessary files.
// TODO/BUG: These should be pointing at the remote files, not the
// local ones.
// var http = require('http');
// http.get('http://localhost/amigo2/javascript/bbop/core.js',
// 	 function(res){
// 	     res.on('data', function(d) {
// 			process.stdout.write(d);
// TODO: how to process remote again?
// 		    });
// 	 });
require('./../lib/bbop/core');
require('./../lib/bbop/logger');
require('./../lib/bbop/registry');
require('./../lib/bbop/golr_conf');
require('./../lib/bbop/golr_response');
require('./../lib/bbop/golr_manager');
require('./../lib/bbop/golr_manager_nodejs');
require('./../lib/bbop/model');
require('./../../../AmiGO/trunk/javascript/bbop/amigo');
require('./../../../AmiGO/trunk/javascript/bbop/amigo/golr_meta');
require('./../../../AmiGO/trunk/javascript/bbop/amigo/amigo_meta');

// Setup the environment a little bit.
gconf = new bbop.golr.conf(bbop.amigo.golr_meta);
go = new bbop.golr.manager.nodejs('http://golr.berkeleybop.org/', gconf);
//go.set_personality('bbop_ont');

// Start the REPL and drop out.
var repl = require("repl");

// var net = require("net");
// // Allow remote connections.
// _connections = 0;
// net.createServer(function (socket) {
//   _connections += 1;
//   repl.start({
//     useGlobal: true,
//     prompt: "golr (remote)> ",
//     input: socket,
//     output: socket
//   }).on('exit', function() {
//     socket.end();
//   });
// }).listen(5001);

// Start the local STDIN REPL.
repl.start({
	       useGlobal: true,
	       prompt: "golr> ",
	       input: process.stdin,
	       output: process.stdout
	   });

