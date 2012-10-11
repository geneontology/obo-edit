/* 
 * Package: golr_response.js
 * 
 * Namespace: bbop.golr.response
 * 
 * Generic BBOP handler for dealing with the gross parsing of
 * responses from a GOlr server (whereas <golr_conf> deals with the
 * reported configuration). This is not intended to do anything like
 * modeling the data in the store (<golr_manager>), but rather to deal
 * with things like checking for success, what paging would look like,
 * what parameters were passed, etc.
 */

// Setup the internal requirements.
bbop.core.require('bbop', 'core');
bbop.core.namespace('bbop', 'golr', 'response');

/*
 * Constructor: response
 * 
 * Contructor for a GOlr query response object.
 * 
 * The constructor argument is an object, not a string.
 * 
 * Arguments:
 *  json_data - the JSON data (as object) returned from a request
 * 
 * Returns:
 *  golr response object
 */
bbop.golr.response = function(json_data){
    this._success = null; // cache for repeated calls to success()
    this._raw = json_data;
};

/*
 * Function: raw
 * 
 * returns a pointer to the initial response object
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  object
 */
bbop.golr.response.prototype.raw = function(){
    return this._raw;
};

/*
 * Function: success
 * 
 * Simple return verification of sane response from server.
 * 
 * Success caches its return value.
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  boolean
 */
bbop.golr.response.prototype.success = function(){

    if( this._success == null ){

	var robj = this._raw;
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
		this._success = true;
	    }else{
		this._success = false;
	    }
    }

    return this._success;
};

/*
 * Function: callback_type
 * 
 * Return the callback type if it was specified in the query,
 * otherwise return null. For example "reset" and "response".
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  string (or null)
 */
bbop.golr.response.prototype.callback_type = function(){
    var robj = this._raw;
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
 *  n/a
 * 
 * Returns:
 *  hash
 */
bbop.golr.response.prototype.parameters = function(){
    var robj = this._raw;
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
 *  n/a
 *  key - string id for the wanted parameter
 * 
 * Returns:
 *  hash, string, whatever is there at that key (otherwise null)
 */
bbop.golr.response.prototype.parameter = function(key){
    var robj = this._raw;
    var retval = null;
    if( robj.responseHeader.params[key] && robj.responseHeader.params[key] ){
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
 *  n/a
 * 
 * Returns:
 *  integer
 */
bbop.golr.response.prototype.row_step = function(){	
    var robj = this._raw;
    return parseInt(robj.responseHeader.params.rows);
};

/*
 * Function: total_documents
 * 
 * Return the total number of documents found.
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  integer
 */
bbop.golr.response.prototype.total_documents = function(){
    var robj = this._raw;
    return parseInt(robj.response.numFound);
};

/*
 * Function: start_document
 * 
 * Returns the start document for this response as an integer.
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  integer
 */
bbop.golr.response.prototype.start_document = function(){
    var robj = this._raw;
    return parseInt(robj.response.start) + 1;
};

/*
 * Function: end_document
 * 
 * Returns the end document for this response as an integer.
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  integer
 */
bbop.golr.response.prototype.end_document = function(){
    var robj = this._raw;
    return this.start_document() +
	parseInt(robj.response.docs.length) - 1;
};

/*
 * Function: packet
 * 
 * Return the packet number of the current response.
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  integer or null (no packet defined)
 */
bbop.golr.response.prototype.packet = function(){
    var robj = this._raw;
    var retval = null;
    var pval = robj.responseHeader.params.packet;
    if( pval ){
	retval = parseInt(pval);
    }
    return retval;
};

/*
 * Function: paging_p
 * 
 * Whether or not paging is necessary with the given results set.
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  boolean
 */
bbop.golr.response.prototype.paging_p = function(){
    var robj = this._raw;
    var retval = false;
    if( this.total_documents() > this.row_step() ){
	retval = true;
    }
    return retval;
};

/*
 * Function: paging_previous_p
 * 
 * Whether or paging backwards is an option right now.
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  boolean
 */
bbop.golr.response.prototype.paging_previous_p = function(){
    // We'll take this as a proxy that a step was taken.
    // Remember: we offset the start_document by one for readability.
    var robj = this._raw;
    var retval = false;
    if( this.start_document() > 1 ){
	retval = true;
    }
    return retval;
};

/*
 * Function: paging_next_p
 * 
 * Whether or paging forwards is an option right now.
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  boolean
 */
bbop.golr.response.prototype.paging_next_p = function(){
    // We'll take this as a proxy that a step was taken.
    var robj = this._raw;
    var retval = false;
    if( this.total_documents() > this.end_document() ){
	retval = true;	
    }
    return retval;
};

/*
 * Function: documents
 * 
 * Returns an array of document hashes.
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  hash
 */
bbop.golr.response.prototype.documents = function(){
    var robj = this._raw;
    return robj.response.docs;
};

// /*
//  * Function: facet_fields
//  * 
//  * Return a count sorted array of the response's facet fields and counts.
//  * 
//  * Arguments:
//  *  n/a
//  * 
//  * Returns:
//  *  list of string/integer doublets
//  */
// bbop.golr.response.prototype.facet_fields = function(){
//     var robj = this._raw;
//     return robj.facet_counts.facet_fields;
// };

/*
 * Function: facet_field_list
 * 
 * Return a count sorted array of the response's facet fields.
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  list of strings
 */
bbop.golr.response.prototype.facet_field_list = function(){
    var robj = this._raw;
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
 *  n/a
 *  facet_name - name of the facet to examine
 * 
 * Returns:
 *  list of nested lists
 */
bbop.golr.response.prototype.facet_field = function(facet_name){
    var robj = this._raw;
    return robj.facet_counts.facet_fields[facet_name];
};

/*
 * Function: facet_counts
 * 
 * For a given facet field, return a hash of that field's items and
 * their counts.
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  hash of facets to their integer counts
 */
bbop.golr.response.prototype.facet_counts = function(){

    var robj = this._raw;
    var ret_hash = {};

    var anchor = this;
    
    var each = bbop.core.each;
    var facet_field_list = this.facet_field_list();
    each(facet_field_list,
	 function(ffield){
	     
	     // Make sure the top field is present,
	     if( ! ret_hash[ffield] ){
		 ret_hash[ffield] = {};		
	     }

	     var facet_field_items = anchor.facet_field(ffield);
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
 *  n/a
 * 
 * Returns:
 *  string or null
 */
bbop.golr.response.prototype.query = function(){
    var robj = this._raw;    
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
 * Parameters:
 *  n/a
 *
 * Returns:
 *  a hash of keyed hashes
 */
bbop.golr.response.prototype.query_filters = function(){
    var robj = this._raw;    
    var ret_hash = {};
    var fq_list = this.parameter('fq');
    if( fq_list ){
	
	// Ensure that it's a list and not just a naked string (as can
	// sometimes happen).
	if( bbop.core.what_is(fq_list) == 'string'){
	    fq_list = [fq_list];
	}
	
	// Make the return fq more tolerable.
	var each = bbop.core.each;
	each(fq_list,
	     function(fq_item){
		 
		 // Split everything on colons. Field is the first
		 // one, and everything else joined back together is
		 // the value of the filter. Best if you think about
		 // the GO id and non-GO id cases.
		 var splits = fq_item.split(":");
		 var field = splits.shift();
		 var value = splits.join(":"); // GO 0022008 -> GO:0022008

		 // First let's just assume that we have a positive
		 // filter.
		 var polarity = true;
		 
		 // Check and see if the first value in our
		 // field is '-' or '+'. If so, edit it out, but
		 // change the polarity in the '-' case.
		 if( field.charAt(0) == '-' ){
		     polarity = false;
		     field = field.substring(1, field.length);
		 }else if( field.charAt(0) == '+' ){
		     field = field.substring(1, field.length);
		 }

		 // Ensure that there is a place in the return hash
		 // for us.
		 if( ! ret_hash[field] ){
		     ret_hash[field] = {};
		 }
		 
		 // I want just the first quote and the final quote
		 // gone from the value if they are matching quotes.
		 if( value.charAt(0) == '"' &&
		     value.charAt(value.length -1) == '"' ){
			 value = value.substring(1, value.length -1);
		     }
		 
		 // The final filter note.
		 ret_hash[field][value] = polarity;
		 
	     });
    }
    
    return ret_hash;
};
