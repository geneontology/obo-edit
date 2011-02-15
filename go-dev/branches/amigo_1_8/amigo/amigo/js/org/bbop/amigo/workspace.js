////////////
////
//// org.bbop.amigo.workspace
////
//// Purpose: Provide methods for accessing the results of the
////          AmiGO workspace service.
////
//// DEPENDS: org.bbop.amigo
//// DEPENDS: org.bbop.amigo.go_meta
////
//////////


// Module and namespace checking.
if ( typeof org.bbop.amigo.workspace == "undefined" ){
    org.bbop.amigo.workspace = {};
}

// A workspace object given a proper response from the server.
org.bbop.amigo.workspace = function(robj){

    var ws_hash = robj.results;
    var meta = new org.bbop.amigo.go_meta();

    // True or false on the existance of a workspace.
    this.has_workspace = function(ws_name){
	var retval = false;
	if( robj && ws_hash && ws_hash[ws_name] ){
	    retval = true;
	}
	return retval;
    };

    // True or false on the existance of an item in a workspace.
    this.has_item = function(ws_name, key){
	var retval = false;
	if( robj && ws_hash && ws_hash[ws_name] && ws_hash[ws_name][key] ){
	    retval = true;
	}
	return retval;
    };

    // List all of the terms in a workspace.
    this.list_terms = function(ws_name){
	var retval = new Array();
	if( robj && ws_hash && ws_hash[ws_name] ){
	    var all_items = ws_hash[ws_name];
	    //print("_a_ " + all_items);
	    //print("_l_ " + all_items.length);
	    for( var wsi = 0; wsi < all_items.length; wsi++ ){
		//dump("_k_: " + all_items[wsi].key + "\n");
		if( meta.term_id_p(all_items[wsi].key) ){
		    //dump("\tokay\n");
		    retval.push(all_items[wsi].key);
		}
	    }
	    //retval.sort();
	}
	return retval;
    };

    // List all things that are not terms in the workspace--hopefully
    // gene products...
    this.list_non_terms = function(ws_name){
	var retval = new Array();
	if( robj && ws_hash && ws_hash[ws_name] ){
	    var all_items = ws_hash[ws_name];
	    for( var wsi = 0; wsi < all_items.length; wsi++ ){
		if( meta.term_id_p(all_items[wsi].key) ){
		    // Not these, but the others...
		}else{
		    retval.push(all_items[wsi].key);
		}
	    }
	}
	return retval;
    };

    // List all of the workspaces.
    this.list_workspaces = function(){
	var retval = new Array();
	if( robj && ws_hash ){
	    for( var wsn in ws_hash ){
		retval.push(wsn);
	    }
	    retval.sort();
	}
	return retval;
    };

    // List all of the items in a workspace.
    this.list_items = function(ws_name){
	// core.kvetch("_IN1_ " + ws_name);
	// core.kvetch("_IN2_ " + robj);
	// core.kvetch("_IN3_ " + ws_hash[ws_name]);
	// core.kvetch("_IN4_ " + ws_hash[ws_name].length);
	var retval = new Array();
	if( robj && ws_hash && ws_hash[ws_name] ){
	    var all_items = ws_hash[ws_name];
	    // core.kvetch("_a_ " + all_items);
	    // core.kvetch("_l_ " + all_items.length);
	    for( var wsi = 0; wsi < all_items.length; wsi++ ){
		retval.push(all_items[wsi]);
	    }
	    //retval.sort();
	}
	return retval;
    };

    ///
    /// Workspace item handling.
    /// TODO: Should this even be in here? More sense to spin
    ///       it completely out?
    ///

    this.create_item = function(key, name){
	
	var good_name = '';
	if( name ){
	    good_name = name;
	}

	return {
	    key: key,
	    name: good_name
	};
    };
    this.get_item_key = function(item){
	return item.key || null;
    };
    this.get_item_name = function(item){
	return item.name || '';
    };

    /// (FAR) TODO: as well as links to POST sqlite3 generated files
    /// off of the host.
};
