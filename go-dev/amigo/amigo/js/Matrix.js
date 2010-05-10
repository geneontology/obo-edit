////
//// Tie some cart actions into the straight HTML mode_matrix page.
//// Not sure if I like doing things this way.
////


// Bring in the AmiGO core.
org.bbop.amigo.DEBUG = true;
var core = new org.bbop.amigo.core();
var cart = null;

// Get the layout done and request ws info.
function MatrixInit(){

    core.kvetch('');
    core.kvetch('MatrixInit start...');

    //
    cart = new org.bbop.amigo.ui.cart('cart-chooser-p');

    ///
    /// Try and get ready for menus by querying the workspace.
    ///

    // Go and get the workspace status information so we can finish the UI.
    function _finish_binding(json_data, status){

	core.kvetch('running: _finish_binding');

	// TODO: Build GUI using things we find in go meta.
	if( ! core.response.success(json_data) ){
	    core.kvetch('We had a data FAILURE.');
	}else{

	    core.kvetch('We have "'+ core.response.type(json_data) +'" data.');

	    // Finish adding workspace necessaries to cart.
	    var ws = new org.bbop.amigo.workspace(json_data);
	    cart.update_workspaces(ws.list_workspaces());

	    // Unwind global map into cart map.
	    for( var cmid in global_cart_map ){
		
		var gps = global_cart_map[cmid];
		for( var g = 0; g < gps.length; g++){
		    
		    var gp_acc = gps[g];
		    cart.add_map(cmid, new org.bbop.amigo.item(gp_acc, ''));
		}
	    }
	    
	    cart.bind('cart-bindable');		
	}
    }
    jQuery.ajax({
	type: "GET",
	url: core.api.workspace.status(), 
	dataType: 'json',
 	success: _finish_binding,
 	error: function (result, status, error) {
 	    alert('Failed server request for workspaces: ' + status); 
 	} 
    });

    
}
