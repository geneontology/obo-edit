/*
 * Package: logger.js
 * 
 * Namespace: bbop.logger
 * 
 * BBOP JS logger object.
 */

// Setup the internal requirements.
bbop.core.require('bbop', 'core');
bbop.core.namespace('bbop', 'logger');

/*
 * Constructor: logger
 * 
 * Arguments: (optional) initial context.
 */
bbop.logger = function(initial_context){

    /*
     * Variable: DEBUG 
     * 
     * Different debugging available per object. Externally toggle
     * between true and false to switch on and off the logging.
     */
    this.DEBUG = false;

    // Define an optional context to tag onto the front of messages.
    this._context = [];
    if( initial_context ){
	this._context = [initial_context];
    }

    /*
     * Function: reset_context
     * 
     * Define the ability to reset the contex.
     * 
     * Arguments:
     *  new_initial_context - (optional) New context to start with.
     */
    this.reset_context = function(new_initial_context){
	if( new_initial_context ){
	    this._context = [new_initial_context];
	}else{
	    this._context = [];	    
	}
    };

    /*
     * Function: push_context
     * 
     * Add an additional logging context to the stack.
     * 
     * Arguments:
     *  new_context - New context to add to the context stack.
     */
    this.push_context = function(new_context){
	this._context.push(new_context);
    };

    /*
     * Function: pop_context
     * 
     * Remove the last context if it's there.
     */
    this.pop_context = function(){
	var popped_context = null;
	if( this._context.length > 0 ){
	    popped_context = this._context.pop();
	}
	return popped_context;
    };

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
    
    /*
     * Function: kvetch
     * 
     * Log a string to somewhere. Also return a string to (mostly for
     * the unit tests).
     * 
     * Arguments:
     *  string - The string to print out to wherever we found.
     */
    this.kvetch = function(string){
	var ret_str = null;
	if( this.DEBUG == true ){

	    // Make sure there is something there no matter what.
	    if( typeof(string) == 'undefined' ){ string = ''; }

	    // Redefined the string a little if we have contexts.
	    if( this._context.length > 0 ){
		var cstr = this._context.join(':');
		string = cstr + ': '+ string;
	    }

	    // Actually log to the console.
	    this._console_sayer(string);

	    // Bind for output.
	    ret_str = string;
	}
	return ret_str;
    };
};
