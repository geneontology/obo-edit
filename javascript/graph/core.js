////////////
////
//// bbop.core
////
//// Purpose: Helpful basic utilities and operations.
//// 
//// TODO: Think again on whether or not these should be prototypes.
//// 
//// Taken name spaces:
////    bbop.core.*
////
//////////


// Module and namespace checking.
if ( typeof bbop == "undefined" ){ bbop = {}; }
if ( typeof bbop.core == "undefined" ){ bbop.core = {}; }


// Capture the global object for later reference.
bbop.core.global_object = this;


// Clone an object (or return the valued atom). Only for shallow
// (basic object) copying.
// Two returns--missing them is very bad. 
bbop.core.clone = function(obj){

    if(obj == null || typeof(obj) != 'object'){
        return obj;	
    }else{
	var temp = obj.constructor();
	for( var key in obj ){
            temp[key] = bbop.core.clone(obj[key]);
	}
	return temp;
    }
    throw new Error("Impossible clone error with: " + obj);
};


// This function extends the global object for easy namespace
// creation.
bbop.core.namespace = function(){

    // Go through the arguments and add them to the namespace,
    // starting at global.
    var current_object = bbop.core.global_object;
    for ( var i = 0; i < arguments.length; i++ ) {
	var ns = arguments[i];
	if( ! current_object[ns] ){
	    current_object[ns] = {};
	}
	current_object = current_object[ns];
    }
    return current_object;
};


// Checks to make sure that the requested namespace is extant
// (checking to see if our libraries are loaded).
bbop.core.require = function(){

    // Walk through from global namespace, checking.
    var current_object = bbop.core.global_object;
    for ( var i = 0; i < arguments.length; i++ ) {
	var ns = arguments[i];
	if( ! current_object[ns] ){
	    throw new Error("Could not find required NS: " + ns);
	}
	current_object = current_object[ns];
    }
    return current_object;
};


// Extend an object properly.
bbop.core.extend = function(kid, sup){  
    for (var property in sup.prototype) {  
	if (typeof kid.prototype[property] == "undefined")  
            kid.prototype[property] = sup.prototype[property];  
    }  
    return kid;
};


// Generalizer console (or whatever) printing.
bbop.core.DEBUG = false;
bbop.core.logger = function(){};

// We'll start with print because we're doing stuff from the
// command line in smjs, but we'll work our way out and see if we
// have a browser environment.
// Check for: Opera, FF, Safari, etc.
if( typeof(opera) != 'undefined' && typeof(opera.postError) != 'undefined' ){
    bbop.core.logger = function(msg){ opera.postError(msg + "\n"); };
}else if( typeof(window) != 'undefined' && typeof(window.dump) != 'undefined' ){
    // From developer.mozilla.org: To see the dump output you have
    // to enable it by setting the preference
    // browser.dom.window.dump.enabled to true. You can set the
    // preference in about:config or in a user.js file. Note: this
    // preference is not listed in about:config by default, you
    // may need to create it (right-click the content area -> New
    // -> Boolean).
    bbop.core.logger = function(msg){ dump( msg + "\n"); };
}else if( typeof(window) != 'undefined' &&
	  typeof(window.console) != 'undefined' &&
	  typeof(window.console.log) != 'undefined' ){
    // From developer.apple.com: Safari's "Debug" menu allows you to
    // turn on the logging of JavaScript errors. To display the
    // debug menu in Mac OS X, open a Terminal window and type:
    // "defaults write com.apple.Safari IncludeDebugMenu 1"
    // Need the wrapper function because safari has personality
    // problems.
    bbop.core.logger = function(msg){ window.console.log(msg + "\n"); };
}else if( typeof(console) != 'undefined' &&
	  typeof(console.log) != 'undefined' ){
    // This may be okay for Chrome...
    bbop.core.logger = function(msg){ console.log(msg + "\n"); };
}else if( typeof(build) == 'function' &&
	  typeof(getpda) == 'function' &&
	  typeof(pc2line) == 'function' &&
	  typeof(print) == 'function' ){
    // This may detect SpiderMonkey on
    // the comand line.
    bbop.core.logger = function(msg){ print(msg); };
}else if( typeof(Packages) !=  'undefined' &&
	  typeof(Packages.java) !=  'undefined' ){
    // This could be a Rhino environment.
    bbop.core.logger = function(msg){ print(msg); };
}
bbop.core.kvetch = function(string){
    if( bbop.core.DEBUG == true ){
	bbop.core.logger(string);
    }
};

// bbop.core.addGS = function(object, name){
//     print(object);
//     print(object.prototype);
//     var mangled_name = '_' + name;
//     object.prototype[mangled_name] = function(value){
//         //
//         if(value){ this[mangled_name] = value; }
//         var retval = null;
//         if( this[mangled_name] ){
//             retval = this[mangled_name];
//         }
//         return retval;
//     };
// };
