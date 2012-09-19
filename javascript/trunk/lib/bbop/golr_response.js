/* 
 * Package: golr_response.js
 * Namespace: bbop.golr.response
 * 
 * Generic BBOP handler for dealing with the gross parsing of
 * responses from a GOlr server (whereas <golr_conf> deals with the
 * reported configuration). This is not intended to do anything like
 * modeling the data in the store (<golr_manager>), but rather to deal
 * with things like checking for success, errors, what paging would
 * look like, what parameterd were passed, etc.
 * 
 * This is a methods bundle for operating on the returned JSON data,
 * with no objects created.
 */

// Setup the internal requirements.
bbop.core.require('bbop', 'core');
bbop.core.namespace('bbop', 'golr', 'response');

/*
 * Function: success
 * 
 * Simple return verification of sane response from server.
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns:
 *  boolean
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
 * Function: callback_type
 * 
 * Return the callback type if it was specified in the query,
 * otherwise return null. For example "reset" and "response".
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns:
 *  string (or null)
 */
bbop.golr.response.callback_type = function(robj){
    var retval = null;
    if( robj.responseHeader.params.callback_type &&
	typeof robj.responseHeader.params.callback_type != 'undefined' ){
	    retval = robj.responseHeader.params.callback_type;
	}
    return retval;
};

/*
 * Function: parameters
 * 
 * Get the parameter chunk--variable stuff we put in.
 * 
 * Pretty general, specialized functions are better.
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns:
 *  hash
 */
bbop.golr.response.parameters = function(robj){
    return robj.responseHeader.params;
};

/*
 * Function: parameter
 * 
 * Get the parameter chunk--variable stuff we put in.
 * 
 * Pretty general, specialized functions are better.
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 *  key - string id for the wanted parameter
 * 
 * Returns:
 *  hash, string, whatever is there at that key (otherwise null)
 */
bbop.golr.response.parameter = function(robj, key){
    var retval = null;
    if( robj.responseHeader.params[key] ){
	retval = robj.responseHeader.params[key];
    }
    return retval;
};

/*
 * Function: row_step
 * 
 * Returns the number of rows requested (integer).
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns:
 *  integer
 */
bbop.golr.response.row_step = function(robj){	
    return parseInt(robj.responseHeader.params.rows);
};

/*
 * Function: total_documents
 * 
 * Return the total number of documents found.
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns:
 *  integer
 */
bbop.golr.response.total_documents = function(robj){
    return parseInt(robj.response.numFound);
};

/*
 * Function: start_document
 * 
 * Returns the start document for this response as an integer.
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns:
 *  integer
 */
bbop.golr.response.start_document = function(robj){
    return parseInt(robj.response.start) + 1;
};

/*
 * Function: end_document
 * 
 * Returns the end document for this response as an integer.
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns:
 *  integer
 */
bbop.golr.response.end_document = function(robj){
    return bbop.golr.response.start_document(robj) +
	parseInt(robj.response.docs.length) - 1;
};

// /*
//  * Function: paging_p
//  * 
//  * Whether or not paging is necessary with the given results set.
//  * 
//  * Arguments:
//  *  robj - JSONized GOlr response
//  * 
//  * Returns:
//  *  boolean
//  */
// bbop.golr.response.paging_p = function(robj){
//     var retval = false;
//     if( bbop.golr.response.total_document(robj) > 
// 	bbop.golr.response.row_step(robj) ){
// 	retval = true;
//     }
//     return retval;
// };

/*
 * Function: documents
 * 
 * Returns an array of document hashes.
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns:
 *  hash
 */
bbop.golr.response.documents = function(robj){
    return robj.response.docs;
};

// /*
//  * Function: facet_fields
//  * 
//  * Return a count sorted array of the response's facet fields and counts.
//  * 
//  * Arguments:
//  *  robj - JSONized GOlr response
//  * 
//  * Returns:
//  *  list of string/integer doublets
//  */
// bbop.golr.response.facet_fields = function(robj){
//     return robj.facet_counts.facet_fields;
// };

/*
 * Function: facet_field_list
 * 
 * Return a count sorted array of the response's facet fields.
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns:
 *  list of strings
 */
bbop.golr.response.facet_field_list = function(robj){
    return bbop.core.get_keys(robj.facet_counts.facet_fields).sort();
};

/*
 * Function: facet_field
 * 
 * Return a count-sorted array of a facet field's response.
 * 
 * : [["foo", 60], ...]
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 *  facet_name - name of the facet to examine
 * 
 * Returns:
 *  list of nested lists
 */
bbop.golr.response.facet_field = function(robj, facet_name){
    return robj.facet_counts.facet_fields[facet_name];
};

/*
 * Function: facet_counts
 * 
 * For a given facet field, return a hash of that field's items and
 * their counts.
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns:
 *  hash of facets to their integer counts
 */
bbop.golr.response.facet_counts = function(robj){
    
    var ret_hash = {};
    
    var each = bbop.core.each;
    var facet_field_list = bbop.golr.response.facet_field_list(robj);
    each(facet_field_list,
	 function(ffield){
	     
	     // Make sure the top field is present,
	     if( ! ret_hash[ffield] ){
		 ret_hash[ffield] = {};		
	     }

	     var facet_field_items =
		 bbop.golr.response.facet_field(robj, ffield);
	     each(facet_field_items,
		 function(item, index){
		     var name = item[0];
		     var count = item[1];
		     ret_hash[ffield][name] = count;
		 });
	 });

    return ret_hash;
};

/*
 * Function: query
 * 
 * Return the raw query parameter "q".
 * 
 * Arguments:
 *  robj - JSONized GOlr response
 * 
 * Returns:
 *  string or null
 */
bbop.golr.response.query = function(robj){
    
    var retval = null;
    
    if( robj.responseHeader.params && robj.responseHeader.params.q ){
	retval = robj.responseHeader.params.q;
    }
    
    return retval;
};

/*
 * Function: query_filters
 *
 * A sensible handling of the not-so-great format of "fq" returned by
 * Solr (fq can be irritating single value or irritating array, along
 * with things like "-" in front of values). Since plus and minus
 * filters are mutually exclusive, we have a return format like:
 * 
 * : {field1: {filter1: (true|false), ...}, ...}
 * 
 * Where the true|false value represents a positive (true) or negative
 * (false) filter.
 * 
 * Parameters: json_data
 *
 * Returns: a hash of keyed hashes
 */
bbop.golr.response.query_filters = function(robj){
    
    //sayer('fq 1a: ' + robj + "\n");
    //sayer('fq 1b: ' + typeof(robj) + "\n");
    //sayer('fq 2a: ' + robj.responseHeader + "\n");
    //sayer('fq 2b: ' + typeof(robj.responseHeader) + "\n");
    
    var ret_hash = {};
    if( robj.responseHeader.params && robj.responseHeader.params.fq ){
	
	//sayer('fq in' + "\n");
	
	var process_list = [];
	
	// Check to see if it's not an array and copy it to be
	// one. Otherwise, copy over the array contents.
	if( typeof robj.responseHeader.params.fq == 'string'){
	    process_list.push(robj.responseHeader.params.fq);
	    //sayer('fq adjust for single' + "\n");
	}else{
	    for( var fqi = 0;
		 fqi < robj.responseHeader.params.fq.length;
		 fqi++ ){
		     var new_bit = robj.responseHeader.params.fq[fqi];
		     process_list.push(new_bit);
		 }
	}
	
	//sayer('fq go through adjusted incoming' + "\n");
	
	// Make the return fq more tolerable.
	for( var pli = 0; pli < process_list.length; pli++ ){
	    var list_item = process_list[pli];
	    
	    //sayer('fq process ' + list_item + "\n");
	    
	    // Split on the colon.
	    var splits = list_item.split(":");
	    var type = splits.shift();
	    var value = splits.join(":");
	    
	    if( ! ret_hash[type] ){
		ret_hash[type] = {};
	    }
	    
	    // Remove internal quotes.
		// Actually, I want just the first quote and the
	    // final quote.
	    if( value.charAt(0) == '"' &&
		value.charAt(value.length -1) == '"' ){
		    //sayer('fq needs cropping: ' + value + "\n");
		    value = value.substring(1, value.length -1);
		    //sayer('fq cropped to: ' + value + "\n");
		}
	    
	    ret_hash[type][value] = true;
	    
	    //sayer('fq done: ' + type + ':' + value + ":true\n");
	}
    }else{
	//ll('fq out');
    }
    
    return ret_hash;
};
