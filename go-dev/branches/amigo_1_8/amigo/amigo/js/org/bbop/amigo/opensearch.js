////////////
////
//// org.bbop.amigo.opensearch
////
//// Purpose: Provide methods for accessing the results of the
////          AmiGO GO opensearch data (amigo format) from completion.
////
//// DEPENDS: org.bbop.amigo
////
//////////


// Module and namespace checking.
if ( typeof org.bbop.amigo.opensearch == "undefined" ){
    org.bbop.amigo.opensearch = {};
}

// A workspace object given a proper response from the server.
org.bbop.amigo.opensearch = function(robj){

    var known_fields =
	{
	    //type: true,
	    id: true,
	    url: true,
	    completion: true,
	    description: true
	};

    // Get the good bits.
    var fixed_query = robj.results.fixed_query || '';
    var raw_query = robj.results.raw_query || '';
    var count = robj.results.count || 0;
    var data = robj.results.data || [];

    //
    this.fixed_query = function(){ return fixed_query; };
    this.raw_query = function(){ return raw_query; };
    this.count = function(){ return count; };
    this.completions = function(){ return data; }
    this.completion = function(i, field){
	var retval = null;

	if( data && data[i] ){
	    if( field && known_fields[field] ){
		retval = data[i][field];
	    }else if ( ! field ){
		retval = data[i];
	    }
	}

	return retval;
    };

    //    
};
