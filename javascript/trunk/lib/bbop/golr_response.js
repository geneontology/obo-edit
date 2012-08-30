/* 
 * Package: golr_response.js
 * Namespace: bbop.golr.response*
 * 
 * Generic BBOP handler for dealing with the gross parsing of
 * responses from a GOlr server (whereas golr_conf deals with the
 * reported configuration). This is not intended to do anything like
 * modeling the data coming back (golr_manager), but rather to deal
 * with things like checking success, errors, and the like.
 * 
 * This is a methods bundle, with no objects created.
 */

// Setup the internal requirements.
bbop.core.require('bbop', 'core');
bbop.core.namespace('bbop', 'golr', 'response');

/*
 * Function: bbop.golr.response.success
 * 
 * Simple return verification of sane response from server.
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns: boolean
 */
bbop.golr.response.success = function(robj){
    var retval = false;
    if( robj &&
	robj.responseHeader &&
	typeof robj.responseHeader.status != 'undefined' &&
	robj.responseHeader.status == 0 &&
	robj.responseHeader.params &&
	robj.response &&
	typeof robj.response.numFound != 'undefined' &&
	typeof robj.response.start != 'undefined' &&
	typeof robj.response.maxScore != 'undefined' &&
	robj.response.docs &&
	robj.facet_counts &&
	robj.facet_counts.facet_fields ){
	    retval = true;
	}
    return retval;
};

/*
 * Function: bbop.golr.response.callback_type
 * 
 *  Return the callback type if it was specified in the query,
 *  otherwise return null. For example "reset" and "response".
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns: string (or null)
 */
bbop.golr.response.callback_type = function(robj){
    var retval = null;
    if( robj.responseHeader.params.callback_type &&
	typeof robj.responseHeader.params.callback_type != 'undefined' ){
	    retval = robj.responseHeader.params.callback_type;
	}
    return retval;
};

// // Get the parameter chunk--variable stuff we put in.
// // Returns a hash.
// bbop.golr.response.parameters = function(robj){
//     return robj.responseHeader.params;
// };

// // Get the parameter chunk--variable stuff we put in.
// // Returns a hash.
// bbop.golr.response.parameter = function(robj, key){
//     var retval = null;
//     if( robj.responseHeader.params[key] ){
// 	retval = robj.responseHeader.params[key];
//     }
//     return retval;
// };

// // Returns the number of rows requested (int).
// bbop.golr.response.row_step = function(robj){	
//     return parseInt(robj.responseHeader.params.rows);
// };

// // ...
// function _golr_response_total_documents(robj){
//     return parseInt(robj.response.numFound);
// }
// bbop.golr.response.total_documents = _golr_response_total_documents;

// // Returns the start document for this response as an int.
// function _golr_response_start_document(robj){
//     //return parseInt(robj.response.start) + 1;
//     return parseInt(robj.response.start);
// }
// bbop.golr.response.start_document = _golr_response_start_document;

// // Returns the end document for this response as an int.
// bbop.golr.response.end_document = function(robj){
//     return _golr_response_start_document(robj) +
// 	parseInt(robj.response.docs.length);
// };

// // Returns an array of doc hashes.
// bbop.golr.response.documents = function(robj){
//     return robj.response.docs;
// };

// // Return a sorted array of the response's facet fields.
// bbop.golr.response.facet_field_list = function(robj){
//     return bbop.core.get_keys(robj.facet_counts.facet_fields).sort();
// };

// // Return a count-sorted array of a facet field's response.
// bbop.golr.response.facet_field = function(robj, facet_name){
//     return robj.facet_counts.facet_fields[facet_name];
// };

// // For a given facet field, return a hash of that field's items
// // and their counts.
// bbop.golr.response.facet_counts = function(robj, in_field){
    
//     var ret_hash = {};
    
//     var facet_list =
// 	bbop.core.get_keys(robj.facet_counts.facet_fields);
//     for( var fli = 0; fli < facet_list.length; fli++ ){
	
// 	var facet_name = facet_list[fli];
// 	if( ! ret_hash[facet_name] ){
// 	    ret_hash[facet_name] = {};		
// 	}
	
// 	var facet_counts = robj.facet_counts.facet_fields[facet_name];
// 	for( var tc = 0; tc < facet_counts.length; tc = tc + 2 ){
// 	    var faspect = facet_counts[tc];
// 	    var fcount = facet_counts[tc + 1];
// 	    ret_hash[facet_name][faspect] = fcount;
// 	}
//     }
    
//     return ret_hash;
// };

// // Return the raw query "q".
// bbop.golr.response.query = function(robj){
    
//     var retval = null;
    
//     if( robj.responseHeader.params && robj.responseHeader.params.q ){
// 	retval = robj.responseHeader.params.q;
//     }
    
//     return retval;
// };

// /*
//  * Function: query_filters
//  *
//  * fq can be irritating single value or irritating array.
//  * 
//  * Parameters: json_data
//  *
//  * Returns: a hash of {field:{val:(true|false)}}; TODO: true|false
//  */
// bbop.golr.response.query_filters = function(robj){
    
//     //sayer('fq 1a: ' + robj + "\n");
//     //sayer('fq 1b: ' + typeof(robj) + "\n");
//     //sayer('fq 2a: ' + robj.responseHeader + "\n");
//     //sayer('fq 2b: ' + typeof(robj.responseHeader) + "\n");
    
//     var ret_hash = {};
//     if( robj.responseHeader.params && robj.responseHeader.params.fq ){
	
// 	//sayer('fq in' + "\n");
	
// 	var process_list = [];
	
// 	// Check to see if it's not an array and copy it to be
// 	// one. Otherwise, copy over the array contents.
// 	if( typeof robj.responseHeader.params.fq == 'string'){
// 	    process_list.push(robj.responseHeader.params.fq);
// 	    //sayer('fq adjust for single' + "\n");
// 	}else{
// 	    for( var fqi = 0;
// 		 fqi < robj.responseHeader.params.fq.length;
// 		 fqi++ ){
// 		     var new_bit = robj.responseHeader.params.fq[fqi];
// 		     process_list.push(new_bit);
// 		 }
// 	}
	
// 	//sayer('fq go through adjusted incoming' + "\n");
	
// 	// Make the return fq more tolerable.
// 	for( var pli = 0; pli < process_list.length; pli++ ){
// 	    var list_item = process_list[pli];
	    
// 	    //sayer('fq process ' + list_item + "\n");
	    
// 	    // Split on the colon.
// 	    var splits = list_item.split(":");
// 	    var type = splits.shift();
// 	    var value = splits.join(":");
	    
// 	    if( ! ret_hash[type] ){
// 		ret_hash[type] = {};
// 	    }
	    
// 	    // Remove internal quotes.
// 		// Actually, I want just the first quote and the
// 	    // final quote.
// 	    if( value.charAt(0) == '"' &&
// 		value.charAt(value.length -1) == '"' ){
// 		    //sayer('fq needs cropping: ' + value + "\n");
// 		    value = value.substring(1, value.length -1);
// 		    //sayer('fq cropped to: ' + value + "\n");
// 		}
	    
// 	    ret_hash[type][value] = true;
	    
// 	    //sayer('fq done: ' + type + ':' + value + ":true\n");
// 	}
//     }else{
// 	//ll('fq out');
//     }
    
//     return ret_hash;
// };
