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


// Clone an object (or return the valued atom).
// Two returns--missing them is very bad. 
bbop.core.clone = function clone(obj){

    if(obj == null || typeof(obj) != 'object'){
        return obj;	
    }else{
	var temp = obj.constructor();
	for( var key in obj ){
            temp[key] = clone(obj[key]);	
	}
	return temp;
    }
    throw new Error("Impossible clone error with: " + obj);
};


// This function extends the global object for easy namespace
// creation.
bbop.core.namespace = function() {

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
bbop.core.require = function() {

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


// // bbop.core.require = function clone(obj){
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
