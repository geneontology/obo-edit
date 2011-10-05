////////////
////
//// bbop.logger
////
//// BBOP JS logger object.
////
//// Taken name spaces:
////    bbop.logger
////
//////////

bbop.core.require('bbop', 'core');
bbop.core.namespace('bbop', 'logger');

bbop.logger = function(){

    // Different debugging available per object.
    this.DEBUG = false;

    // Generalizer console (or whatever) printing.
    this._console_sayer = function(){};

    // Check for: Opera, FF, Safari, etc.
    if( typeof(opera) != 'undefined' &&
	typeof(opera.postError) != 'undefined' ){
	this._console_sayer = function(msg){ opera.postError(msg + "\n"); };
    }else if( typeof(window) != 'undefined' &&
	      typeof(window.dump) != 'undefined' ){
	// From developer.mozilla.org: To see the dump output you have
	// to enable it by setting the preference
	// browser.dom.window.dump.enabled to true. You can set the
	// preference in about:config or in a user.js file. Note: this
	// preference is not listed in about:config by default, you
	// may need to create it (right-click the content area -> New
	// -> Boolean).
	this._console_sayer = function(msg){ dump( msg + "\n"); };
    }else if( typeof(window) != 'undefined' &&
	      typeof(window.console) != 'undefined' &&
	      typeof(window.console.log) != 'undefined' ){
	// From developer.apple.com: Safari's "Debug" menu allows you
	// to turn on the logging of JavaScript errors. To display the
	// debug menu in Mac OS X, open a Terminal window and type:
	// "defaults write com.apple.Safari IncludeDebugMenu 1" Need
	// the wrapper function because safari has personality
	// problems.
	this._console_sayer = function(msg){ window.console.log(msg + "\n"); };
    }else if( typeof(console) != 'undefined' &&
	      typeof(console.log) != 'undefined' ){
	// This may be okay for Chrome...
	this._console_sayer = function(msg){ console.log(msg + "\n"); };
    }else if( typeof(build) == 'function' &&
	      typeof(getpda) == 'function' &&
	      typeof(pc2line) == 'function' &&
	      typeof(print) == 'function' ){
	// This may detect SpiderMonkey on the comand line.
	this._console_sayer = function(msg){ print(msg); };
    }else if( typeof(org) != 'undefined' &&
	      typeof(org.rhino) != 'undefined' &&
	      typeof(print) == 'function' ){
	// This may detect Rhino on the comand line.
	this._console_sayer = function(msg){ print(msg); };
    }
    
    this.kvetch = function(string){
	if( this.DEBUG == true ){
	    if( typeof(string) == 'undefined' ){ string = ''; }
	    this._console_sayer(string);
	}
    };
};
