////////////
////
//// org.bbop.amigo.ui.standard
////
//// DEPENDS: org.bbop.amigo
////
//// WARNING: Initialization needs to be done after the document is ready.
////
//////////


// Module and namespace checking.
// TODO: Will we need a ui class up there?
if ( typeof org.bbop.amigo.ui == "undefined" ){
    org.bbop.amigo.ui = {};
}


//
org.bbop.amigo.ui.standard = function(){  

    function ll(str){ core.kvetch(str); }
    ll("");

    //
    var js_links = jQuery("." + "simulated-link");
    if( js_links && js_links.length ){

	for( var jli = 0; jli < js_links.length; jli++ ){
	    var jl = js_links[jli];
	    jQuery(jl).hover(
		function (){
		    jQuery(this).addClass("simulated-link-hover");
		},
		function (){
		    jQuery(this).removeClass("simulated-link-hover");
		}
		
	    );
	}
    }else{
	ll('failed');
    }
};
