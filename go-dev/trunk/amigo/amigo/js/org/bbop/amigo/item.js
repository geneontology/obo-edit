////////////
////
//// org.bbop.amigo.item
////
//// Purpose: Provide an "item" wrapper, intended for workspace use.
////
//// DEPENDS: org.bbop.amigo
////
//////////


// // Module and namespace checking.
// if ( typeof org.bbop.amigo.workspace == "undefined" ){
//     org.bbop.amigo.workspace = {};
// }

// A workspace object given a proper response from the server.
org.bbop.amigo.item = function(key, in_name){

    this.key = key;
    this.name = '';
    if( in_name ){
	this.name = in_name;
    }
};

org.bbop.amigo.item.prototype.get_key = function(){
    return this.key || null;
};

org.bbop.amigo.item.prototype.get_name = function(){
    return this.name || '';
};
