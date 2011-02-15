//////////
//
// File:      ajax.js (org/bbop/ajax.js)
// Namespace: org.bbop.ajax
// Comment:   Very simple AJAX module (for when lib are too heavy or broken).
// Depends:   None.
// Status:    Completed.
// Passed:    FF 1.5+, Safari 2, IE7, IE6.
// Failed:    
// TODO:      Better fails on state change. Is POST correct?
// BUGS:      No known bugs.
//
//////////


// Module and namespace checking.
if ( typeof org == "undefined" ){ org = {}; }
if ( typeof org.bbop == "undefined" ){ org.bbop = {}; }


// Wrap the complexities of MS's non-standard implementation.
org.bbop.ajax = function(args){
    
    //
    var callback = function(){};
    if( args['callback'] ){
	callback = args['callback'];
    }else{
	throw new Error("org.bbop.ajax: callback: required");
    }

    // TODO:
    //var failure = function(msg){};
    //if( args['failure'] ){
    //callback = args['failure'];
    //}
    
    //
    var url = '';
    if( args['url'] ){
	url = args['url'];
    }else{
	throw new Error("org.bbop.ajax: url: required");
    }
    
    //
    var res_type = 'text';
    if( args['response'] && args['response'] == 'xml' ){
	res_type = args['response'];
    }
    
    //
    var req_type = 'get';
    if( args['type'] && args['type'] == 'post' ){
	req_type = args['type'];
    }
    
    // Capture a new XHR.
    var xhr = (function(){
	var new_request_object = undefined;
	if( window.XMLHttpRequest ) {
	    new_request_object = new XMLHttpRequest();
	} else if(window.ActiveXObject) {
	    try {
		new_request_object = new ActiveXObject("Msxml2.XMLHTTP");
	    } catch(e) {
		new_request_object = new ActiveXObject("Microsoft.XMLHTTP");
	    }
	}
	return new_request_object;
    })();
    
    // Tie state changes to functions.
    xhr.onreadystatechange = function(){
	if ( xhr.readyState == 4 ){
	    if ( xhr.status == 200 ){
		if (callback){
		    if( res_type == 'xml' ){
			callback(xhr.responseXML);
		    }else{
			callback(xhr.responseText);
		    }
		}
	    }else{
		//failure('non-200');
	    }
	}else if ( xhr.readyState == 3 ){
	}else if ( xhr.readyState == 2 ){
	}else if ( xhr.readyState == 1 ){
	}else if ( xhr.readyState == 0 ){
	}else{
	    //failure('?');
	} 
    }
 
    // Start the run.
    if ( req_type == "get" ){
	xhr.open("GET", url, true);
	xhr.send(null);
    }else{
	xhr.open("POST", url, true);
	xhr.setRequestHeader("Content-Type",
			     "application/x-www-form-urlencoded");
	//xhr.send(url); // TODO: really?
	xhr.send(null);
    }
};
