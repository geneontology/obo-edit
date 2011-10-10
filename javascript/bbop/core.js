////////////
////
//// bbop.core
////
//// BBOP language extensions to JavaScript.
////
//// Purpose: Helpful basic utilities and operations to fix common
//// needs in JS.
//// 
//// TODO: Think again on whether or not these should be folded in to
//// language defined object prototypes.
//// 
//// Taken name spaces:
////    bbop.core.*
////
//////////


// Module and namespace checking.
if ( typeof bbop == "undefined" ){ bbop = {}; }
if ( typeof bbop.core == "undefined" ){ bbop.core = {}; }
//if ( typeof bbop.core.json == "undefined" ){ bbop.core.json = {}; }

// @???
// Capture the global object for later reference.
bbop.core._global = this;

///
/// Utility functions can hang as prototypes.
///

// @Object?
// This function extends the global object for easy namespace
// creation.
bbop.core.namespace = function(){

    // Go through the arguments and add them to the namespace,
    // starting at global.
    var current_object = bbop.core._global;
    for ( var i = 0; i < arguments.length; i++ ) {
	var ns = arguments[i];
	if( ! current_object[ns] ){
	    current_object[ns] = {};
	}
	current_object = current_object[ns];
    }
    return current_object;
};

// @Object?
// Checks to make sure that the requested namespace is extant
// (checking to see if our libraries are loaded).
bbop.core.require = function(){

    // Walk through from global namespace, checking.
    var current_object = bbop.core._global;
    for ( var i = 0; i < arguments.length; i++ ) {
	var ns = arguments[i];
	if( ! current_object[ns] ){
	    throw new Error("Could not find required NS: " + ns);
	}
	current_object = current_object[ns];
    }
    return current_object;
};

// @Object?
// Extend an object properly.
bbop.core.extend = function(kid, sup){  
    for (var property in sup.prototype) {  
	if (typeof kid.prototype[property] == "undefined")  
            kid.prototype[property] = sup.prototype[property];  
    }  
    return kid;
};

// @String
// Crop a string to a certain limit and add ellipses.
bbop.core.crop = function(str, lim, suff){
    var ret = str;

    var limit = 10;
    if( lim ){ limit = lim; }

    var suffix = '';
    if( suff ){ suffix = suff; }
    
    if( str.length > limit ){
	ret = str.substring(0, (limit - suffix.length)) + suffix;
    }
    return ret;
};

// @Object?
// Merge a pair of hashes, using the first as default and template.
bbop.core.merge = function(default_hash, arg_hash){

    if( ! default_hash ){ default_hash = {}; }
    if( ! arg_hash ){ arg_hash = {}; }

    var ret_hash = {};
    for( var key in default_hash ){
	if( arg_hash[key] ){
	    ret_hash[key] = arg_hash[key];
	}else{
	    ret_hash[key] = default_hash[key];
	}
    }
    return ret_hash;
};

// @Object?
// Get the hash keys from a hash.
bbop.core.get_hash_keys = function get_hash_keys (arg_hash){

    if( ! arg_hash ){ arg_hash = {}; }
    var out_keys = [];
    for (var out_key in arg_hash) {
	if (arg_hash.hasOwnProperty(out_key)) {
	    out_keys.push(out_key);
	}
    }
    
    return out_keys;
};

// Implement a simple iterator so I don't go mad.
// (@Array|@Object)/@Function
// @Array: function(item, index)
// @Object: function(key, value)
bbop.core.each = function(in_thing, in_function){

    // Probably an not array then.
    if( typeof(in_thing) != 'object' ){
	throw new Error('unsupported type in each');
    }else{
	// Probably a hash, otherwise likely an array.
	if( typeof(in_thing).length == 'undefined' ){
	    var hkeys = bbop.core.get_hash_keys(in_thing);
	    for( var ihk = 0; ihk < hkeys.length; ihk++ ){
		var ikey = hkeys[ihk];
		var ival = in_thing[ikey];
		in_function(ikey, ival);
	    }
	}else{
	    for( var iai = 0; iai < in_thing.length; iai++ ){
		in_function(in_thing[iai], iai);
	    }
	}
    }
};

// @Object?
// Clone an object down to its atoms.
bbop.core.clone = function(thing){

    var clone = null;

    if( typeof(thing) == 'undefined' ){
	// Nothin' doin'.
	//print("looks undefined");
    }else if( typeof(thing) == 'function' ){
	// Dunno about this case...
	//print("looks like a function");
	clone = thing;
    }else if( typeof(thing) == 'boolean' ||
	      typeof(thing) == 'number' ||
	      typeof(thing) == 'string' ){
	// Atomic types can be returned as-is (i.e. assignment in
	// JS is the same as copy for atomic types).
	//print("cloning atom: " + thing);
	clone = thing;
    }else if( typeof(thing) == 'object' ){
	// Is it a hash or an array?
	if( typeof(thing.length) == 'undefined' ){
	    // Looks like a hash!
	    //print("looks like a hash");
	    clone = {};
	    for(var h in thing){
		clone[h] = bbop.core.clone(thing[h]);
	    }
	}else{
	    // Looks like an array!
	    //print("looks like an array");
	    clone = [];
	    for(var i = 0; i < thing.length; i++){
		clone[i] = bbop.core.clone(thing[i]);
	    }
	}
    }else{
	// Then I don't know what it is--might be platform dep.
	//print("no idea what it is");
    }
    return clone;
};

// @Object
// Assemble an object into a GET-like query.
bbop.core.get_assemble = function(qargs){

    var mbuff = [];	
    for( var qname in qargs ){
	var qval = qargs[qname];

	if( typeof qval == 'string' || typeof qval == 'number' ){
	    // Is standard name/value pair.
	    var nano_buff = [];
	    nano_buff.push(qname);
	    nano_buff.push('=');
	    nano_buff.push(qval);
	    mbuff.push(nano_buff.join(''));
	}else if( typeof qval == 'object' ){
	    if( typeof qval.length != 'undefined' ){
		// Is array (probably).
		// Iterate through and double on.
		for(var qval_i = 0; qval_i < qval.length ; qval_i++){
		    var nano_buff = [];
		    nano_buff.push(qname);
		    nano_buff.push('=');
		    nano_buff.push(qval[qval_i]);
		    mbuff.push(nano_buff.join(''));
		}
	    }else{
		// // TODO: The "and" case is pretty much like
		// // the array, the "or" case needs to be
		// // handled carfeully. In both cases, care will
		// // be needed to show which filters are marked.
		// Is object (probably).
		for( var sub_name in qval ){
		    var sub_val = qval[sub_name];

		    var nano_buff = [];
		    nano_buff.push(qname);
		    nano_buff.push('=');
		    nano_buff.push(sub_name);
		    nano_buff.push(':');
		    nano_buff.push('"' + sub_val + '"');
		    mbuff.push(nano_buff.join(''));
		}
	    }
	}else{
	    throw new Error("bbop.coreget_assemble: unknown type");
	}
    }
    
    return mbuff.join('&');
};

// @Math
// Random number generator of fixed length.
// Return a randome number string of length len.
bbop.core.randomness = function(len){

    var random_base =
	['1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
	 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
	 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];
    var length = 10;
    if( len ){
	length = len;
    }
    var cache = new Array();
    for( var ii = 0; ii < length; ii++ ){
	var rbase_index = Math.floor(Math.random() * random_base.length);
	cache.push(random_base[rbase_index]);
    }
    return cache.join('');
};

// @???
// Functions to encode and decode data that we'll be hiding in the
// element ids. This is a
bbop.core.coder = function(args){

    var mangle_base_string = "bbop_core_coder_mangle_";
    var mangle_base_space_size = 10;

    var defs = {string: mangle_base_string, size: mangle_base_space_size};
    var final_args = _merge(defs, args);
    var mangle_str = final_args['string'];
    var space_size = final_args['size'];

    // TODO/BUG: apparently, html ids can only be of a limited
    // character set.
    //var en_re = new RegExp("/:/", "gi");
    //var de_re = new RegExp("/-_-/", "gi");
    this.encode = function(str){
	// Mangle and encode.
	var new_str = mangle_str + _randomness(space_size) +'_'+ str;
	// TODO:
	// str.replace(en_re, "-_-");
	return new_str;
    };
    this.decode = function(str){	    
	// Decode and demangle.
	var new_str = str.substring(mangle_str.length + space_size + 1);
	// TODO:
	// str.replace(de_re, ":");
	return new_str;
    };
};
