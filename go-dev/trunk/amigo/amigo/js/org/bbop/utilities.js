////
//// 
////

if ( typeof org == "undefined" ){ org = {}; }
if ( typeof org.bbop == "undefined" ){ org.bbop = {}; }

//
org.bbop.utilities = function(){

    //
    this.addGS = function(object, name){

	print(object);
	print(object.prototype);

	var mangled_name = '_' + name;
	object.prototype[mangled_name] = function(value){

	    //
	    if(value){ this[mangled_name] = value; }
	
	    var retval = null;
	    if( this[mangled_name] ){
		retval = this[mangled_name];
	    }
	    return retval;
	};
    };
};
