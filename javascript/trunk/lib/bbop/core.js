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

/*
 * Variable: global
 * 
 * Capture the global object for later reference.
 */
bbop.core.global = this;

///
/// Utility functions can hang as prototypes.
///

/*
 * Function: namespace
 * 
 * Create a namespace (chained object) in the global environment.
 * 
 * Parameters: An arbitrary number of strings.
 * 
 * Returns: Nothing. Side-effects: this function extends the global
 * object for easy namespace creation.
 * 
 * Also See: <require>
 */
bbop.core.namespace = function(){

    // Go through the arguments and add them to the namespace,
    // starting at global.
    var current_object = bbop.core.global;
    for ( var i = 0; i < arguments.length; i++ ) {
	var ns = arguments[i];
	if( ! current_object[ns] ){
	    current_object[ns] = {};
	}
	current_object = current_object[ns];
    }
    return current_object;
};

/*
 * Function: require
 * 
 * Throw an error unless a specified namespace is defined.
 * 
 * Parameters: An arbitrary number of strings.
 * 
 * Returns: Nothing. Side-effects: throws an error if the namespace
 * defined by the strings is not currently found.
 * 
 * Also See: <namespace>
 */
bbop.core.require = function(){

    // Walk through from global namespace, checking.
    var current_object = bbop.core.global;
    for ( var i = 0; i < arguments.length; i++ ) {
	var ns = arguments[i];
	if( ! current_object[ns] ){
	    throw new Error("Could not find required NS: " + ns);
	}
	current_object = current_object[ns];
    }
    return current_object;
};

// /*
//  * Function: extend
//  * 
//  * Parameters: An arbitrary number of strings.
//  * 
//  * Returns: Nothing. Side-effects: throws an error if the namespace
//  * defined by the strings is not currently found.
//  */
// // @Object?
// // Extend an object properly.
// bbop.core.extend = function(kid, sup){  
//     for( var property in sup.prototype ){
// 	if (typeof kid.prototype[property] == "undefined")  
//             kid.prototype[property] = sup.prototype[property];  
//     }  
//     return kid;
// };

/*
 * Function: crop
 *
 * Crop a string nicely.
 * 
 * Parameters:
 *  str - the string to crop
 *  lim - the final length to crop to (optional, defaults to 10)
 *  suff - the string to add to the end (optional, defaults to '')
 * 
 * Returns: Nothing. Side-effects: throws an error if the namespace
 * defined by the strings is not currently found.
 */
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

/*
 * Function: fold
 *
 * Fold a pair of hashes together, using the first one as a template.
 * 
 * Parameters:
 *  default_hash - Template hash.
 *  arg_hash - Argument hash to match.
 * 
 * Returns: A new hash.
 */
bbop.core.fold = function(default_hash, arg_hash){

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

/*
 * Function: merge
 *
 * Merge a pair of hashes together, using the second values getting
 * precedence.
 * 
 * Parameters:
 *  older_hash - first pass
 *  newer_hash - second pass
 * 
 * Returns: A new hash.
 */
bbop.core.merge = function(older_hash, newer_hash){

    if( ! older_hash ){ older_hash = {}; }
    if( ! newer_hash ){ newer_hash = {}; }

    var ret_hash = {};
    function _add (key, val){
	ret_hash[key] = val;
    }
    bbop.core.each(older_hash, _add);
    bbop.core.each(newer_hash, _add);
    return ret_hash;
};

/*
 * Function: get_keys
 *
 * Get the hash keys from a hash/object, return as an array.
 *
 * Parameters:
 *  arg_hash - the hash in question
 *
 * Returns: an array of keys
 */
bbop.core.get_keys = function get_keys (arg_hash){

    if( ! arg_hash ){ arg_hash = {}; }
    var out_keys = [];
    for (var out_key in arg_hash) {
	if (arg_hash.hasOwnProperty(out_key)) {
	    out_keys.push(out_key);
	}
    }
    
    return out_keys;
};

/*
 * Function: what_is
 *
 * Return the string best guess for what the input is, null if it
 * can't be identified. In addition to the _is_a property convention,
 * current core output strings are: 'null', 'array', 'boolean',
 * 'number', 'string', 'function', and 'object'.
 * 
 * Parameters: 
 *  in_thing - the thing in question
 *
 * Returns: a string
 */
bbop.core.what_is = function(in_thing){
    var retval = null;
    if( typeof(in_thing) != 'undefined' ){

	// If it's an object, try and guess the 'type', otherwise, let
	// typeof.
	if( in_thing == null ){
	    retval = 'null';
	}else if( typeof(in_thing) == 'object' ){
	    
	    // Look for the 'is_a' property that I should be using.
	    if( typeof(in_thing._is_a) != 'undefined' ){
		retval = in_thing._is_a;
	    }else{
		if( bbop.core.is_array(in_thing) ){
		    retval = 'array';
		}else{
		    retval = 'object';
		}		
	    }
	}else{
	    retval = typeof(in_thing);
	}
    }
    return retval;
};

/*
 * Function: is_array
 *
 * Return the best guess (true/false) for whether ot not a given
 * object is being used as an array.
 *
 * Parameters: 
 *  in_thing - the thing in question
 *
 * Returns: boolean
 */
bbop.core.is_array = function(in_thing){
    var retval = false;
    if( typeof(in_thing) == 'object' &&
	typeof(in_thing.push) == 'function' &&
	typeof(in_thing.length) == 'number' ){
	retval = true;
    }
    return retval;
};

/*
 * Function: is_hash
 *
 * Return the best guess (true/false) for whether ot not a given
 * object is being used as a hash.
 *
 * Parameters: 
 *  in_thing - the thing in question
 *
 * Returns: boolean
 */
bbop.core.is_hash = function(in_thing){
    var retval = false;
    if( typeof(in_thing) == 'object' &&
	(! bbop.core.is_array(in_thing)) ){
	retval = true;
    }
    return retval;
};

/*
 * Function: is_empty
 *
 * Return true/false on whether or not the object in question has any
 * items of interest (iterable?).
 *
 * Parameters: 
 *  in_thing - the thing in question
 *
 * Returns: boolean
 */
bbop.core.is_empty = function(in_thing){
    var retval = false;
    if( bbop.core.is_array(in_thing) ){
	if( in_thing.length == 0 ){
	    retval = true;
	}
    }else if( bbop.core.is_hash(in_thing) ){
	var in_hash_keys = bbop.core.get_keys(in_thing);
	if( in_hash_keys.length == 0 ){
	    retval = true;
	}
    }else{
	// TODO: don't know about this case yet...
	//throw new Error('unsupported type in is_empty');	
	retval = false;
    }
    return retval;
};

/*
 * Function: each
 *
 * Implement a simple iterator so I don't go mad.
 *  array - function(item, index)
 *  object - function(key, value)
 *
 * Parameters: 
 *  in_thing - hash or array
 *  in_function - function to apply to elements
 *
 * Returns: Nothing
 */
bbop.core.each = function(in_thing, in_function){

    // Probably an not array then.
    if( typeof(in_thing) == 'undefined' ){
	// this is a nothing, to nothing....
    }else if( typeof(in_thing) != 'object' ){
	throw new Error('Unsupported type in bbop.core.each: ' +
			typeof(in_thing) );
    }else if( bbop.core.is_hash(in_thing) ){
	// Probably a hash...
	var hkeys = bbop.core.get_keys(in_thing);
	for( var ihk = 0; ihk < hkeys.length; ihk++ ){
	    var ikey = hkeys[ihk];
	    var ival = in_thing[ikey];
	    in_function(ikey, ival);
	}
    }else{
	// Otherwise likely an array.
	for( var iai = 0; iai < in_thing.length; iai++ ){
	    in_function(in_thing[iai], iai);
	}
    }
};

/*
 * Function: pare
 *
 * Take an array or hash and par it down using a couple of functions
 * to what we want.
 * 
 * Both parameters are optional in the sense that you can set them to
 * null and they will have no function; i.e. a null filter will let
 * everything through and a null sort will let things go in whatever
 * order.
 *
 * Parameters: 
 *  in_thing - hash or array
 *  filter_function - hash (function(key, val)) or array (function(item, i)).
 *   This function must return boolean true or false.
 *  sort_function - function to apply to elements: function(a, b)
 *   This function must return an integer as the usual sort functions do.
 *
 * Returns: An array.
 */
bbop.core.pare = function(in_thing, filter_function, sort_function){

    var ret = [];
    
    // Probably an not array then.
    if( typeof(in_thing) == 'undefined' ){
	// this is a nothing, to nothing....
    }else if( typeof(in_thing) != 'object' ){
	throw new Error('Unsupported type in bbop.core.pare: ' +
			typeof(in_thing) );
    }else if( bbop.core.is_hash(in_thing) ){
	// Probably a hash; filter it if filter_function is defined.
	if( filter_function ){	
	    bbop.core.each(in_thing,
			   function(key, val){
			       if( filter_function(key, val) ){
				   // Remove matches to the filter.
			       }else{
				   ret.push(val);
			       }
			   });
	}else{
	    bbop.core.each(in_thing, function(key, val){ ret.push(val); });
	}
    }else{
	// Otherwise, probably an array; filter it if filter_function
	// is defined.
	if( filter_function ){	
	    bbop.core.each(in_thing,
			   function(item, index){
			       if( filter_function(item, index) ){
				   // filter out item if true
			       }else{
				   ret.push(item);
			       }
			   });
	}else{
	    bbop.core.each(in_thing, function(item, index){ ret.push(item); });
	}
    }

    // For both: sort if there is anything.
    if( ret.length > 0 && sort_function ){
	ret.sort(sort_function);	    
    }

    return ret;
};

/*
 * Function: clone
 *
 * Clone an object down to its atoms.
 *
 * Parameters: 
 *  thing - whatever
 *
 * Returns: a new whatever
 */
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

/*
 * Function: to_string
 *
 * Essentially add standard 'to string' interface to the string class
 * and as a stringifier interface to other classes. More meant for
 * output.
 *
 * Parameters: 
 *  in_thing - something
 *
 * Returns: string
 * 
 * Also See: <dump>
 */
bbop.core.to_string = function(in_thing){

    var what = bbop.core.what_is(in_thing);
    if( what == 'number' ){
	return in_thing.toString();
    }else if( typeof(in_thing) == 'string' ){
	return in_thing;
    }else if( in_thing.to_string && typeof(in_thing.to_string) == 'function' ){
	return in_thing.to_string();
    }else{
	throw new Error('to_string interface not defined for this thing');
    }
};

/*
 * Function: dump
 *
 * Dump an object to a string form as best as possible. More meant for
 * debugging. For a slightly different take, see to_string.
 *
 * Parameters: 
 *  in_thing - something
 *
 * Returns: string
 * 
 * Also See: <to_string>
 */
bbop.core.dump = function(thing){

    var retval = '';

    var what = bbop.core.what_is(thing);
    if( what == null ){
	retval = 'null';
    }else if( what == 'null' ){
	retval = 'null';
    }else if( what == 'string' ){
	retval = '"' + thing + '"';
    }else if( what == 'boolean' ){
	if( thing ){
	    retval = "true";
	}else{
	    retval = "false";
	}
    }else if( what == 'array' ){

	var astack = [];
	bbop.core.each(thing, function(item, i){
			   astack.push(bbop.core.dump(item));
		       });
	retval = '[' + astack.join(', ') + ']';

    }else if( what == 'object' ){

	var hstack = [];
	bbop.core.each(thing, function(key, val){
			   hstack.push('"'+ key + '": ' +
				       bbop.core.dump(val));
		       });
	retval = '{' + hstack.join(', ') + '}';

    }else{
	retval = thing;
    }

    return retval;
};

/*
 * Function: has_interface
 *
 * Check to see if an object supplies an "interface".
 *
 * Parameters: 
 *  iobj - the objct in question
 *  iface - the interface (as a string) that we're looking for
 *
 * Returns: boolean
 *
 * TODO: Unit test this to make sure it catches both prototype (okay I
 * think) and uninstantiated objects (harder/impossible?).
 */
// @Object
bbop.core.has_interface = function(iobj, iface){
    var retval = true;
    bbop.core.each(iobj,
		   function(in_key, in_val){
		       if( typeof(in_val[iface]) == 'undefined' &&
			   typeof(in_val.prototype[iface]) == 'undefined'){
			   retval = false;
			   throw new Error(in_key + ' breaks interface ' + 
					   iface);
                       }
		   });
    return retval;
};

/*
 * Function: get_assemble
 *
 * Assemble an object into a GET-like query.
 *
 * Parameters: 
 *  qargs - hash/object
 *
 * Returns: string
 */
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

/*
 * Function: 
 *
 * Random number generator of fixed length. Return a random number
 * string of length len.
 *
 * Parameters: 
 *  len - the number of random character to return.
 *
 * Returns: string
 */
bbop.core.randomness = function(len){

    var random_base =
	['1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
	 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
	 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];
    var length = len || 10;
    var cache = new Array();
    for( var ii = 0; ii < length; ii++ ){
	var rbase_index = Math.floor(Math.random() * random_base.length);
	cache.push(random_base[rbase_index]);
    }
    return cache.join('');
};

/*
 * Function: 
 *
 * RFC 4122 v4 compliant UUID generator.
 * From: http://stackoverflow.com/questions/105034/how-to-create-a-guid-uuid-in-javascript/2117523#2117523
 *
 * Parameters: 
 *
 * Returns: string
 */
bbop.core.uuid = function(){

    // Replace x (and y) in string.
    function replacer(c) {
	var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
	return v.toString(16);
    }
    var target_str = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx';
    return target_str.replace(/[xy]/g, replacer);
};

/*
 * Function: coder
 *
 * TODO doc Functions to encode and decode data that we'll be hiding
 *  in the element ids. This is a
 *
 * Parameters: 
 *   - 
 *
 * Returns: 
 */
bbop.core.coder = function(args){

    var mangle_base_string = "bbop_core_coder_mangle_";
    var mangle_base_space_size = 10;

    var defs = {string: mangle_base_string, size: mangle_base_space_size};
    var final_args = bbop.core.fold(defs, args);
    var mangle_str = final_args['string'];
    var space_size = final_args['size'];

    // TODO/BUG: apparently, html ids can only be of a limited
    // character set.
    //var en_re = new RegExp("/:/", "gi");
    //var de_re = new RegExp("/-_-/", "gi");
    this.encode = function(str){
	// Mangle and encode.
	var new_str = mangle_str + bbop.core.randomness(space_size) +'_'+ str;
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