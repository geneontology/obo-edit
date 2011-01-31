////////////
////
//// org.bbop.amigo.ui.shopping
////
//// The front face of the shopping cart, and what most people
//// should use for it.
////
//// DEPENDS: org.bbop.amigo
//// DEPENDS: org.bbop.amigo.go_meta
//// DEPENDS: org.bbop.amigo.workspace
//// DEPENDS: org.bbop.amigo.ui.widgets
//// DEPENDS: org.bbop.amigo.ui.cart
//// DEPENDS: org.bbop.amigo.ui.shield
////
//// WARNING: Initialization needs to be done after the document is ready.
////
//// // Usage example:
//// var global_shop = null;
//// function ScratchClientInit(){
////     global_shop = new org.bbop.amigo.ui.shopping();
////     jQuery('#bar123').click(global_shop.generate_hook('foo_key', 'NAME'));
//// }
////
//////////

// Module and namespace checking.
// TODO: Will we need a ui class up there?
if ( typeof org.bbop.amigo.ui == "undefined" ){
    org.bbop.amigo.ui = {};
}

//
org.bbop.amigo.ui.shopping = function(){  

    // We'll be doing a lot of debugging.
    function ll(str){ core.kvetch(str); }
    ll("");
    ll("org.bbop.amigo.ui.shopping initial...");

    var go_meta = new org.bbop.amigo.go_meta();
    var widgets = new org.bbop.amigo.ui.widgets();
    //var cart = new org.bbop.amigo.ui.cart();
    
    // Handle current workspace; both internally and externally.
    var current_workspace = 'default';
    this.get_workspace = function(){
	return current_workspace;
    };
    // Why should I let something outside touch this again?
    // this.set_workspace = function(str){
    // 	if( str ){
    // 	    current_workspace = str;
    // 	}else{
    // 	    current_workspace = 'default';
    // 	}
    // 	return current_workspace;
    // };

    //
    var map_key_base = "org_bbop_amigo_ui_widgets_shopping_map_";
    var img_to_id_map = null;
    function _create_item_list_html (ws_item_list){

	// Reset map.
	img_to_id_map = {};

	var buf = new Array();
	buf.push('<div>'); // TODO: needs to be able scroll
	buf.push('<ul>');
	for( var ws_i = 0; ws_i < ws_item_list.length; ws_i++ ){

	    // Jimmy meat out of nut.
	    var ws_item = ws_item_list[ws_i];
	    var key_str  = '';
	    var name_str = '';
	    var date_str = '';
	    if( ws_item.key  ){ key_str = ws_item.key; }
	    if( ws_item.name ){ name_str = ws_item.name; }
	    if( ws_item.date ){ date_str = ws_item.date; }

	    // Create a snapshot map for img clicking later on.
	    var img_map_id = map_key_base + core.util.randomness();
	    img_to_id_map[img_map_id] = key_str;
	    
	    // Create HTML.
	    buf.push('<li>');
	    buf.push('&nbsp;&nbsp;&nbsp;<b>acc</b>:&nbsp;'   + key_str +
		     '&nbsp;<b>label</b>:&nbsp;' + name_str +
		     '&nbsp;<b>date</b>:&nbsp;' + date_str);
	    buf.push('&nbsp;&nbsp;<img id="' +
		     img_map_id + '" title="delete item" src="');
	    buf.push(go_meta.image_base() + '/fatal.png');
	    buf.push('" />');
	    // buf.push('&nbsp;&nbsp;[<span title="delete item" ');
	    // buf.push('href="#" onclick="');
	    // buf.push("_deletable_callback('" +
	    // 	     ws + "','" +
	    // 	     key_str + "');");
	    // buf.push('" class="deletable_item">delete</span>]');
	    buf.push('</li>');
	}
	buf.push('</ul>');
	buf.push('</div>');
	return buf.join('');	
    }

    // First things first, add the cart div with a unique id to the
    // document's bottom.
    var id_base = "org_bbop_amigo_ui_widgets_shopping_";
    var UID = id_base + core.util.randomness();
    var div_text = '<div id="' + UID + '" ' +
	'class="cart-rules climbing">Cart<img class="cart-image" src="'+
	global_cart_image +
	'"/></div>';
    jQuery("body").append(jQuery(div_text));

    // Save a hook for later.
    var elt = jQuery('#' + UID);
    
    // Add a click handler to deal with tossing up the cart shield
    // (including contacting the server for the most recent
    // information).
    elt.click(function(){
	
	var me_id = jQuery(this).attr('id');
	core.kvetch("_clicked_on_id_: " + me_id);
	
	// 
	function _full_shield_generation(){
	    widgets.start_wait("Getting workspace information...");
	    ll("Status URL: " + core.api.workspace.status());
	    jQuery.ajax({
		type: "GET",
		url: core.api.workspace.status(), 
		dataType: 'json',
 		success: _finish_binding,
 		error: function (result, status, error) {
		    widgets.finish_wait();
 		    alert('Failed server request for workspaces: ' + status);
 		} 
	    });
	}

	//
	function _finish_binding(json_data, status){

	    ll("org.bbop.amigo.ui.shopping binding completion...");

	    // TODO: Build GUI using things we find in go meta.
	    if( ! core.response.success(json_data) ){
		core.kvetch('We had a data FAILURE.');
	    }else{

		///
		/// Setup shield for current state.
		///

		core.kvetch('We have "' +
			    core.response.type(json_data) + '" creation data.');
		var ws = new org.bbop.amigo.workspace(json_data);

		// Make select list HTML. Can't wrap because we need
		// access to the selection id for later reference.
		var all_workspaces = ws.list_workspaces();
		var ws_array = new Array();
		for( var ws_i = 0; ws_i < all_workspaces.length; ws_i++ ){
		    var ws_thing = all_workspaces[ws_i];
		    ws_array.push([ws_thing, ws_thing]);
		}
		var selid_base = "org_bbop_amigo_ui_widgets_shopping_select_";
		var select_uid = selid_base + core.util.randomness();
		var sel_form_id = "workspace_select";
		var opt_html =
		    widgets.form.select(select_uid, sel_form_id,
					ws_array, current_workspace,
					'Select workspace:');
		
		// Actual shield display up.
		var shield_html = '<h4>Change current workspace or delete items.</h4>' +
		    'Go to <a href="' +
		    // TODO/BUG: this needs to be internally renerated
		    // in go_meta.
		    go_meta.app_base() + '/amigo_exp?mode=workspace_client' +
		    '">workspace management</a>' +
		    opt_html +
		    '<br />' +
		    'Contents of current workspace:' +
		    '<br />' +
		    _create_item_list_html(ws.list_items(current_workspace));
		var shield = new org.bbop.amigo.ui.shield.set(shield_html);
		
		// Binding to change the current workspace variable.
		// In addition, close shield and reopen with new ws.
		jQuery('#' + select_uid).change(function(event){
		    
		    var marshal_select =
			widgets.form.create_jquery_marshal('#' + select_uid,
							   ['input',
							    'option:selected']);
		    var all_ms_inputs = marshal_select();
		    var new_workspace_name = all_ms_inputs[sel_form_id];
		    current_workspace = new_workspace_name;

		    ll('workspace name: ' + current_workspace);

		    // Call back to redo shield on ws change.
		    shield.close();
		    _full_shield_generation();
		});

		// TODO: Add item deletion binding?
		for( var i2i in img_to_id_map ){
		    var unhashed_key = i2i;
		    var unhashed_val = img_to_id_map[i2i];
		    jQuery('#' + unhashed_key).click(function(event){
			var img_id = jQuery(this).attr('id');
			var iid = img_to_id_map[img_id];
			var cws = current_workspace;
			//ll('map a: ' + img_id);
			//ll('map b: ' + iid);
			// TODO: delete action
			// Call back to redo shield on ws change.
			shield.close();
			//_full_shield_generation();
			jQuery.ajax({
			    type: "GET",
			    url: core.api.workspace.remove_item(cws, iid),
			    dataType: 'json',
			    // success: _finish_binding,
			    success: _full_shield_generation,
			    error: function (result, status, error) {
				alert('Failed server request: ' + status); 
			    }
			});
		    });
		}
	    }
	    widgets.finish_wait();
	}

	// Initial cart shield up on click.
	_full_shield_generation();
    });

    // Generate a function that can be used by clients for callback
    // onclick functions to add items.
    //this.generate_item_addition = function(wrk_space, item_or_items){
    // TODO: generalize out to items.
    // NOTE: largely lifted from cart.js, but there I'm using a very strange
    // keying system with the DOM, probably trying to get around closure
    // issues.
    this.generate_hook = function(in_key, in_name){

	// Event as argument since it will be used as a jQuery callback.
	return function(event){

	    var key = null;
	    var name = '';

	    if( in_key ){ key = in_key; }
	    if( in_name ){ name = in_name; }
	    if( key ){

		// Yank correct URL given our current workspace.
		var try_url =
		    core.api.workspace.add_item(current_workspace, key, name);
		core.kvetch("try: " + try_url);			
		widgets.start_wait('Adding...');
		
		// Asynchronously add item.
		jQuery.ajax({
		    type: "GET",
		    url: try_url,
		    dataType: 'json',
		    success: function(){
			core.kvetch("Added to WS.");
			widgets.notice("Added.");
			widgets.finish_wait();
		    },
		    error: function (result, status, error) {
			core.kvetch('Failed server request: ' +
				    status);
			widgets.finish_wait();
		    }
		});
	    }
	};
    };
};
