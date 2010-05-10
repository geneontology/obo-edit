//////////
//
// ORB JSAPI
//
// Purpose:
//
// Depends: 
//
// Status:
//
// QUIRKs:
//
// WARNINGs:
//
// BUGs:
//   
// TODOs:
//
//////////


var global_cgi_path = 'http://toy.lbl.gov:9002/cgi-bin/amigo2';


// TODO: Specialize for ORB tasks.
ORBAjax = function(callback){
    
  // Capture a new XHR in a closure.
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
  xhr.onreadystatechange = function(){
    if ( xhr.readyState == 4 ){
      if ( xhr.status == 200 ){
	if (callback){
	  callback(xhr.responseXML);
	}
      }
    }else if ( xhr.readyState == 3 ){
    }else if ( xhr.readyState == 2 ){
    }else if ( xhr.readyState == 1 ){
    }else if ( xhr.readyState == 0 ){
    }else{
      throw new Error("Whoa! How did I get here?");
    } 
  }
  
  this.start = function(server, query, type){
    if ( type == "GET" ){
      xhr.open("GET", server + "?" + query, true);
      xhr.send();
    }else{
      xhr.open("POST", server, true);
      xhr.setRequestHeader("Content-Type",
			   "application/x-www-form-urlencoded");
      xhr.send(query);   
    }
  }
};


// Compliments ORBForm.
// NOTE: I use 'escape' and 'unescape'.
ORBServer = function(listeners){

  // Asynchronous event listeners.
  var trackerInformationListener = null;
  var itemsListener = null;

  // Set listeners. TODO: Function reality check.
  if( listeners ){
    if( listeners.trackerInformation ){
      trackerInformationListener = listeners.trackerInformation; }
    if( listeners.items ){
      itemsListener = listeners.items; }
  }

  // Setters for the listeners.
  this.setTrackerInformationListener =
    function(func){ trackerInformationListener = func; };
  this.setItemsListener = function(func){ itemsListener = func; };

  // Return tracker data structure, an array of:
  // { ontology_id, name, description, url, tracker, group_id, atid }
  this.getTrackers = function(){
    return ORBTrackers;
  };


  // Returns to listener array of:
  // { name, value }
  this.getTrackerInformation = function(ont_id){
    var return_val = false;
    if( trackerInformationListener ){      
      var xhr = new ORBAjax(_getTrackerInformationCallback);
      var server = global_cgi_path + "/orb.cgi";
      var query = "request=tracker_information&ontology_id=" + ont_id;
      xhr.start(server, query, "POST");
      return_val = true;
    }
    return return_val;
  };

  //
  function _getTrackerInformationCallback( xml_response ){
    
    var cat_struct = new Array;
    var art_struct = new Array;

    var cats = xml_response.getElementsByTagName('category_id');
    if( cats ){
      for( var i = 0; i < cats.length; i++ ){
	var name = cats[i].getElementsByTagName('name')[0].firstChild.data;
	var value = cats[i].getElementsByTagName('value')[0].firstChild.data;
	cat_struct.push({name: name, value: value});
      }
    }

    var arts = xml_response.getElementsByTagName('artifact_group_id');
    if( arts ){
      for( var i = 0; i < arts.length; i++ ){
	var name = arts[i].getElementsByTagName('name')[0].firstChild.data;
	var value = arts[i].getElementsByTagName('value')[0].firstChild.data;
	art_struct.push({name: name, value: value});
      }
    }

    trackerInformationListener(cat_struct, art_struct);
  }


  // Returns to listener array of:
  // { ontology_id, summary, open_date, priority, assigned_to, submitted_by }
  this.getItems = function(ont_id, details_p, username){

    var return_val = false;
    if( itemsListener ){
      var xhr = new ORBAjax(_getItemsCallback);
      var server = global_cgi_path + "/orb.cgi";
      var query = "request=items";
      query += "&ontology_id=" + ont_id;
      query += "&detailed_information=" + details_p;
      if( username ){
	query += "&username=" + username; }
      xhr.start(server, query, "POST");
      return_val = true;
    }
    return return_val;
  };

  //
  function _getItemsCallback( xml_response ){
    
    var item_struct = new Array;

    var item_tags = [
		     'priority',
		     'resolution_status',
		     'status',
		     'submitted_by',
		     'item_id',
		     'summary',
		     'open_date',
		     'assigned_to',
		     'resolution',
		     'modtype',
		     'details',
		     'definition',
		     'attribution',
		     'agent'
    ];

    var items = xml_response.getElementsByTagName('item');
    if( items ){
      for( var i = 0; i < items.length; i++ ){

	// For each of the items, cycle through all the possible
	// information as defined by item_tags.
	var item = {};
	var number_of_fields = 0;

	for( var j = 0; j < item_tags.length; j++ ){

	  if( items[i].getElementsByTagName(item_tags[j]) &&
	      items[i].getElementsByTagName(item_tags[j])[0] &&
	      items[i].getElementsByTagName(item_tags[j])[0].firstChild &&
	      items[i].getElementsByTagName(item_tags[j])[0].firstChild.data ){
	    
	    item[ item_tags[j] ] =
	      items[i].getElementsByTagName(item_tags[j])[0].firstChild.data;

	    number_of_fields++;
	  }
	}
	
	// Push it on if there was anything to it.
	if( number_of_fields > 0 ){
	  item_struct.push( item );
	}
      }
    }

    itemsListener(item_struct);
  }
};


// Compliments ORBServer.  TODO: Integrate this into main server so
// the checking parameters can be tweaked from a single source.
ORBForm = function(submit_listener){

  // Asynchronous event listener. TODO: Function reality check.
  var submitListener = null;
  if( submit_listener && submit_listener.submit ){
    submitListener = submit_listener.submit; }
  this.setSubmitListener = function(func){ submitListener = func; };

  //
  var param_hash = {};
  param_hash[ 'agent' ] = { value: 'JSAPI v0.1b', required: true };
  param_hash[ 'ontology_id' ] = { value: '', required: true };
  param_hash[ 'category_id' ] = { value: '', required: false };
  param_hash[ 'artifact_group_id' ] = { value: '', required: false };
  param_hash[ 'summary' ] = { value: '', required: true };
  param_hash[ 'definition' ] = { value: '', required: true };
  param_hash[ 'details' ] = { value: '', required: true };
  param_hash[ 'modtype' ] = { value: '', required: false };
  param_hash[ 'username' ] = { value: '', required: false };
  param_hash[ 'login' ] = { value: '', required: false };
  param_hash[ 'password' ] = { value: '', required: false };
  param_hash[ 'attribution' ] = { value: '', required: false };


  // Is it well-defined and sane?
  function _isWellFormed(parameter){
    var retval = false;
    if( parameter &&
	param_hash[ parameter ] &&
	param_hash[ parameter ].value &&
	param_hash[ parameter ].value.length ){
      retval = true; }
    return retval;
  }


  // Proper testing of all conditions.
  function _review(param){

    var retval = false;

    if( param == 'agent' && _isWellFormed(param) ){
      retval = true;
    }else if( param == 'ontology_id' && _isWellFormed(param) ){
      retval = true;
    }else if( param == 'category_id' && _isWellFormed(param) ){
      retval = true;
    }else if( param == 'artifact_group_id' && _isWellFormed(param) ){
      retval = true;
    }else if( param == 'summary' && _isWellFormed(param) ){
      retval = true;
    }else if( param == 'definition' && _isWellFormed(param) ){
      retval = true;
    }else if( param == 'details' && _isWellFormed(param) ){
      retval = true;
    }else if( param == 'modtype' && _isWellFormed(param) ){
      retval = true;
    }else if( param == 'username' && _isWellFormed(param) ){
      retval = true;
    }else if( param == 'login' && _isWellFormed(param) ){
      retval = true;
    }else if( param == 'password' && _isWellFormed(param) ){
      retval = true;
    }else if( param == 'attribution' && _isWellFormed(param) ){
      retval = true;
    }

    return retval;
  }


  // Change the value return true or false depending on whether
  // or not it is a legal value.
  this.set = function(name, value){

    var retval = false;
    if( param_hash[ name ] ){
      param_hash[ name ].value = value;
      retval = _review(name);
    }
    return retval;
  };


  // Return array of properties or just one.
  this.get = function(param){

    var retval = null;
    if( param && param_hash[ param ] ){

      retval = { name: param_hash[ param ].value,
		 ok: _review(param),
		 required: param_hash[ param ].required };

    }else{

      retval = new Array;

      // Summary review.
      for( var key in param_hash ){
	retval.push({ name: key,
	      ok: _review(key),
	      required: param_hash[ key ].required });
      }
    }    
    
    return retval;
  };


  // Can review everything or just a specific paramater. Returns an
  // object or an array of them accordingly.
  this.review = function(param){

    var rev = null;

    if( param && param_hash[ param ] && param_hash[ param ].required ){

      rev = { name: param,
	      ok: _review(param),
	      required: param_hash[ param ].required };

    }else{

      rev = new Array;

      // Summary review.
      for( var key in param_hash ){
	rev.push({ name: key,
	      ok: _review(key),
	      required: param_hash[ key ].required });
      }
    }    

    return rev;
  };


  // Submit the form to the server and let the user catch the returned
  // XML.
  this.submit = function(){
    var return_val = false;
    if( submitListener ){
      var xhr = new ORBAjax(_submitCallback);
      var server = global_cgi_path + "/orb.cgi";
      
      // Add required and optional paramaters.
      var outq = new Array;
      outq.push('request=add');
      for( var name in param_hash ){
	outq.push(name + '=' + param_hash[ name ].value); }

      var query = outq.join('&');      
      xhr.start(server, query, "POST");
      return_val = true;
    }
    return return_val;
  };

  //
  function _submitCallback( xml_response ){

    var response_struct = { success: false,
			    message: 'server not responding',
			    ontology_id: '',
			    item_id: ''};

    if( xml_response ){
      
      var error = xml_response.getElementsByTagName('error');
      var ontology_id = xml_response.getElementsByTagName('ontology_id');
      var item_id = xml_response.getElementsByTagName('item_id');
      if( error && error[0] && error[0].firstChild &&
	  error[0].firstChild.data ){

	response_struct.message = error[0].firstChild.data;

      }else if( item_id && item_id[0] && item_id[0].firstChild &&
		item_id[0].firstChild.data &&
		ontology_id && ontology_id[0] && ontology_id[0].firstChild &&
		ontology_id[0].firstChild.data ){
	
	response_struct.success = true;
	response_struct.message = 'success';
	response_struct.ontology_id = ontology_id[0].firstChild.data;
	response_struct.item_id = item_id[0].firstChild.data;
      } 
    }
    submitListener(response_struct);
  }
};
