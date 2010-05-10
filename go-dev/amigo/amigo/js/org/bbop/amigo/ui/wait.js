////////////
////
//// org.bbop.amigo.ui.wait
////
//// A shield to let people know that action is going on.
////
//// DEPENDS: org.bbop.amigo
//// DEPENDS: org.bbop.amigo.ui.shield
////
//// WARNING: Initialization needs to be done after the document is ready.
////
//// // Usage example:
//// var global_wait = null;
//// function ScratchClientInit(){
////     global_wait = new org.bbop.amigo.ui.wait();
////     jQuery('#bar123').click(global_shop.generate_hook();
//// }
////
//////////

// Module and namespace checking.
// TODO: Will we need a ui class up there?
if ( typeof org.bbop.amigo.ui == "undefined" ){
    org.bbop.amigo.ui = {};
}

//
org.bbop.amigo.ui.wait = function(msg){  

    // We'll be doing a lot of debugging.
    function ll(str){ core.kvetch(str); }
    ll("");
    ll("org.bbop.amigo.ui.wait initial...");

    if( ! msg ){
	msg = '<h4>Generic Shield.</h4>';
    }

    //
    this.generate_hook = function(){

	// Event as argument since it will be used as a jQuery callback.
	return function(event){
	    // Actual shield display up.
	    var shield_html = msg;
	    var shield = new org.bbop.amigo.ui.shield.set(shield_html);
	};
    };

    ll("org.bbop.amigo.ui.wait done...");
};
