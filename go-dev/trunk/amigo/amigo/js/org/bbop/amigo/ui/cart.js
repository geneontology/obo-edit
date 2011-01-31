////////////
////
//// org.bbop.amigo.ui.cart
////
//// Behind the scenes control for different cart functions.
////
//// DEPENDS: org.bbop.amigo
//// DEPENDS: org.bbop.amigo.go_meta
//// DEPENDS: org.bbop.amigo.workspace
//// DEPENDS: org.bbop.amigo.ui.widgets
////
//// WARNING: Initialization needs to be done after the document is ready.
////
//////////


// Module and namespace checking.
// TODO: Will we need a ui class up there?
if ( typeof org.bbop.amigo.ui == "undefined" ){
    org.bbop.amigo.ui = {};
}


// checkbox_id is the element id for where we'll check to see if we
// want to fall back on last checked or if we want to open the
// workspace menu dialog.
org.bbop.amigo.ui.cart = function(checkbox_id){  

    var short_circut_checkbox_id = checkbox_id;
    var cart_widgets = new org.bbop.amigo.ui.widgets();

    // We'll be doing a lot of debugging.
    function ll(str){ core.kvetch(str); }
    ll("");

    var workspace_list = [];
    var id_to_item_map = [];
    var last_workspace_selected = 'default';

    ///
    /// Handling.
    ///

    this.update_workspaces = function(ws_list){
	workspace_list = ws_list;
    };

    //
    this.reset_map = function(){
	id_to_item_map = {};
    };
    this.clear_map = function(id){
	id_to_item_map[id] = [];
    };
    this.add_map = function(id, item){
	if( ! id_to_item_map[id] ){
	    id_to_item_map[id] = [];
	}
	id_to_item_map[id].push(item);
    };


    //
    function add_item_to_ws(in_ws, in_key, in_name){

	var ws = 'default';
	var key = null;
	var name = '';

	if( in_ws ){ ws = in_ws; }
	if( in_key ){ key = in_key; }
	if( in_name ){ name = in_name; }
	if( key ){

	    var try_url = core.api.workspace.add_item(ws, key, name);

	    core.kvetch("try: " + try_url);			
	    cart_widgets.start_wait('Adding...');

	    jQuery.ajax({
		type: "GET",
		url: try_url,
		dataType: 'json',
		success: function(){
		    core.kvetch("Added to WS.");
		    cart_widgets.notice("Added.");
		    cart_widgets.finish_wait();
		},
		error: function (result, status, error) {
		    core.kvetch('Failed server request: ' +
				status);
		    cart_widgets.finish_wait();
		}
	    });
	}
    }

    //
    function add_items_to_ws(in_ws, item_list){

	var iws = 'default';
	if( in_ws ){ iws = in_ws; }

	if( item_list && item_list.length ){

	    for( var il = 0; il < item_list.length; il++ ){
		var itm = item_list[il];
		add_item_to_ws(iws, itm.get_key(), itm.get_name());
	    }
	}
    }


    //
    this.bind = function(class_id){

	// TODO: bind actions to things in the action column.
	core.kvetch("adding cart bindings to class: " + class_id + "...");

	// Bind to everything that looks like this class.
	var elts = jQuery("." + class_id);
	for( var e = 0; e < elts.length; e++ ){
	    
	    var elt = jQuery(elts[e]);
	    
	    // (Possibly) show menu on click.
	    elt.click(function(){

		// Only run next bit if we've gotten our workspace
		// information back from the callback.
		if( workspace_list.length > 0 ){

		    // Recover the id from the elt and look it up in
		    // our map system.
		    var me_id_mangle = jQuery(this).attr('id');
		    core.kvetch("_clicked_on_id: " + me_id_mangle);
		    var listing = [];
		    if( id_to_item_map[me_id_mangle] &&
			id_to_item_map[me_id_mangle].length ){
			listing = id_to_item_map[me_id_mangle];
			core.kvetch("   found: " + listing.length + ' items');
		    }else{
			core.kvetch('   found _no_ items');
		    }

		    //var me_id = coder.decode(me_id_mangle);

		    // Check to see whether we want to just toss it
		    // into the last used cart or whether we want to do
		    // a menu select (the default option).
		    // TODO:
		    var use_last_cart =
			'#' + short_circut_checkbox_id + ':checked';
		    if( jQuery(use_last_cart) &&
			jQuery(use_last_cart).val() == 'checked'){
			
			core.kvetch("cart short circut: checked");

			add_items_to_ws(last_workspace_selected, listing);

		    }else{

			core.kvetch("cart short circut: unchecked or null");

			// Build the dialog with the list.
			var select_ws_id = 'select_ws_id_' +
			    core.util.randomness(10);
			var ws_buf = new Array();
			ws_buf.push('<form>');
			for( var wsi = 0;
			     wsi < workspace_list.length;
			     wsi++ ){
			    var curr_ws = workspace_list[wsi];
			    core.kvetch(" _ : " + curr_ws);
			    ws_buf.push('<input type="radio" ');
			    if( curr_ws == last_workspace_selected ){
				ws_buf.push(' checked="checked" ');
			    }
			    ws_buf.push('name="'+ select_ws_id +
					'" value="'+ curr_ws +
					'">&nbsp;'+ curr_ws + '</input>');
			    ws_buf.push('<br />');
			}
			ws_buf.push('</form>');
		
			// Create the dialog text.
			var dialog_id =
			    'workspace_dialog_id_' + core.util.randomness(10);
			var dialog_text =
			    '<div ' + 'id="' + dialog_id +
			    '" title="Add to workspace...">'+ ws_buf.join('') +
			    '</div>';
			jQuery("body").append(jQuery(dialog_text));

			// Create the dialog.
			jQuery("#" + dialog_id ).dialog({
			    bgiframe: true,
			    autoOpen: false,
			    height: 300,
			    width: 500,
			    modal: true,
			    buttons: {
				'Add to workspace':
				function() {

				    // Figure out what is selected and
				    // updated the last selected.
				    var checked_ws =
					jQuery('input[name=' + select_ws_id +
					       ']:checked').val() 
				    last_workspace_selected = checked_ws;

				    core.kvetch("dialog_id: " +	dialog_id);
				    core.kvetch("select_ws_id: "+ select_ws_id);

				    add_items_to_ws(checked_ws, listing);

				    //
				    jQuery(this).dialog('close');
				},
				Cancel: function() {
				    jQuery(this).dialog('close');
				}
			    },
			    close: function() {
			      //allFields.val('').removeClass('ui-state-error');
			    }
			});
			// Display the dialog.
			jQuery('#' + dialog_id).dialog('open');
		    }
		    // Here ends the checked/unchecked switch.
		}else{
		    core.kvetch("WS menu impossible until callback completed.");
		}
		
	    });
	}
    };
};
