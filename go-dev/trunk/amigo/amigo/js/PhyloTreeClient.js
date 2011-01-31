////
//// ...
////


// Bring in the AmiGO core.
org.bbop.amigo.DEBUG = true;
var core = new org.bbop.amigo.core();
var widgets = null;

// Might come in handy for GUI stuff as we go.
//var global_layout = null;
//var global_last_tab = 'default';
//var global_last_data = {};


//
function PhyloTreeClientInit(){

    //global_shopping_cart = new org.bbop.amigo.ui.shopping();

}


// 
function PhyloTreeBuilder(){

    core.kvetch('');
    core.kvetch('PhyloTreeBuilder start.');

    widgets = new org.bbop.amigo.ui.widgets();
    //utool = new widgets.unitary_tooltip();
    core.kvetch('widgets: ' + widgets + '.');

    ///
    /// String to tree format.
    ///
    // Ed's thingy seems borked here, using the original...
    //var tree = NewickTreeUtils.parseNewick(global_raw_data);
    var tree = newickTreeParse(global_raw_data);
    
    ///
    /// Tree into div.
    ///
    //var gtree = new newickTreeGraphic(global_raw_data, "ntree");
    //var gtree = new newickTreeGraphic(tree.getRoot(), "ntree");
    var gtree = new newickTreeGraphic(tree, "ntree");
    gtree.draw();

    ///
    /// Get the layout ready.
    ///

    // //global_layout = jQuery('body').layout({ applyDefaultStyles: true });
    // core.kvetch('Apply layout...');
    // global_layout = jQuery('body').layout({

    // 	//
    // 	applyDefaultStyles: true,

    // 	// North is treated as an island. 
    // 	north__applyDefaultStyles: false,
    // 	north__closable: false,
    // 	north__resizable: false,
    // 	north__slidable: false,

    // 	// South is treated as an island. 
    // 	south__applyDefaultStyles: false,
    // 	south__closable: false,
    // 	south__resizable: false,
    // 	south__slidable: false,

    // 	// We'll need this in certain cases (like map resizing because
    // 	// it's actually donw through CSS).
    // 	// Attach listeners to the center area. TODO: this will be
    // 	// split out into the navi bits later on. For now, just a
    // 	// test.	
    // 	center__onresize: function(){
    // 	    // Apparently, center is not completed yet, so we compute
    // 	    // from all of the compass points.
    // 	    var ymax = global_layout.state.container.innerHeight;
    // 	    ymax = ymax - global_layout.state.north.size;
    // 	    ymax = ymax - global_layout.state.south.size;

    // 	    var xmax = global_layout.state.container.innerWidth;
    // 	    //xmax = xmax - global_layout.state.west.size;
    // 	    //xmax = xmax - global_layout.state.east.size;

    // 	    core.kvetch('_resize_xy_:' + xmax + ':' + ymax);
    // 	}
    // });
    // // core.kvetch('___' + global_layout);
    // // core.kvetch('WorkspaceClient init completed.');
    // // core.kvetch('Contacting server for meta-information.');

    // // Go and get the workspace status information so we can finish the UI.
    // jQuery.ajax({
    // 	type: "GET",
    // 	url: core.api.workspace.status(), 
    // 	dataType: 'json',
    // 	success: WorkspaceCreateGUI,
    // 	error: function (result, status, error) { 
    // 	    alert('Failed server request: ' + status); 
    // 	} 
    // });

    core.kvetch('PhyloTreeBuilder end.');
}


// // Wipe errors. Were there any new errors? Report only the first.
// function HandleErrors(jblob){

//     jQuery("#error").html('');
//     if( core.response.errors(jblob).length > 0 ){
// 	var an_error = core.response.errors(jblob)[0];
// 	jQuery("#error").html("<h2>" + an_error + "</h2>").show().fadeOut(2000);
//     }
// }


// // Build the necessary GUIs from the ws info, and ready the
// // callbacks dor the rest of the application.
// function WorkspaceCreateGUI(json_data, status){
    
//     core.kvetch('WorkspaceCreateGUI start.');
 
//     // TODO: Build GUI using things we find in go meta.
//     if( core.response.success(json_data) ){

// 	core.kvetch('We have "' + core.response.type(json_data) + '" data.');

// 	//
// 	HandleErrors(json_data);

// 	var all_workspaces = json_data.results;
// 	global_last_data = all_workspaces;

// 	// Create and populate the status tabs.
// 	core.kvetch('Tabify...');
// 	//var status_string = _status_html(all_workspaces);
//  	//jQuery("#status").html(status_string);
// 	jQuery("#status").tabs();

// 	// Connect add_workspace to event.
// 	jQuery('#ws-add-form').submit(function(){ 
// 	    var ws = jQuery("#ws-add-form-workspace").val()
//             jQuery.ajax({
//     		type: "GET",
//     		url: core.api.workspace.add(ws),
//     		dataType: 'json',
//      		success: WorkspaceUpdateGUI,
//      		error: function (result, status, error) {
//      		    alert('Failed server request: ' + status); 
//      		}
//             });
// 	    return false;
// 	});

// 	// Connect clear_workspace to event.
// 	jQuery('#ws-clear-form').submit(function(){ 
// 	    var ws = jQuery("#ws-clear-form-workspace").val()
// 	    if( ws ){
// 		jQuery.ajax({
// 		    type: "GET",
// 		    url: core.api.workspace.clear(ws),
// 		    dataType: 'json',
// 		    success: WorkspaceUpdateGUI,
// 		    error: function (result, status, error) {
// 		     	alert('Failed server request: ' + status); 
// 		    }
// 		});
// 	    }
// 	    return false;
// 	});

// 	// Connect remove_workspace to event.
// 	jQuery('#ws-remove-form').submit(function(){ 
// 	    var ws = jQuery("#ws-remove-form-workspace").val()
// 	    if( ws ){
// 		jQuery.ajax({
// 		    type: "GET",
// 		    url: core.api.workspace.remove(ws),
// 		    dataType: 'json',
// 		    success: WorkspaceUpdateGUI,
// 		    error: function (result, status, error) {
// 		     	alert('Failed server request: ' + status); 
// 		    }
// 		});
// 	    }
// 	    return false;
// 	});

// 	// Connect chaining to event.
// 	jQuery('#ws-chain-form').submit(function(){ 

// 	    var ws = jQuery("#ws-chain-form-workspace").val()
// 	    var dest = jQuery("#ws-chain-form-destination").val()

// 	    if( ws && dest && global_last_data[ws] ){

// 		var all_terms =
// 		    core.util.workspace.get_terms(global_last_data[ws]);

// 		// alert("Use " + ws + " on " + dest + " ... " +
// 		// core.link.layers2_graph({
// 		//   terms: all_terms
// 		// }));
// 		// TODO: check this everywhere...
// 		window.location.href = core.link.layers2_graph({
// 		    terms: all_terms
// 		});
// 	    }
// 	    return false;
// 	});

// 	//
// 	WorkspaceUpdateGUI(json_data, status);

//     }else{
// 	alert("response failure in create");
//     }
// }


// // Refresh what is on the screen with the current data.
// function WorkspaceUpdateGUI(json_data, status){
    
//     core.kvetch('WorkspaceUpdateGUI start.');
 
//     // TODO: Build GUI using things we find in go meta.
//     if( core.response.success(json_data) ){

// 	core.kvetch('We have "' + core.response.type(json_data) + '" data.');

// 	//
// 	HandleErrors(json_data);

// 	var all_workspaces = json_data.results;
// 	global_last_data = all_workspaces;

// 	//
// 	core.kvetch('Update workspace operations...');
// 	var ws_opts = _select_options_html(all_workspaces);
// 	jQuery('#ws-clear-form-workspace').html(ws_opts);
// 	jQuery('#ws-remove-form-workspace').html(ws_opts);
// 	jQuery('#ws-chain-form-workspace').html(ws_opts);


// 	// Save raised tab state.
// 	var selected = jQuery('#status').tabs('option', 'selected');
// 	global_last_tab = 0;
// 	if( selected > 0 ){
// 	    global_last_tab = selected;
// 	}

// 	// Destroy and create tabs.
//  	jQuery("#status").tabs('destroy');
//  	jQuery("#status").html(_status_html(all_workspaces));
// 	jQuery("#status").tabs();
// 	// 	var tlen = jQuery("#status").tabs().length;
// 	// 	core.kvetch('_l_' + tlen);
// 	// 	for( var ti = 0; ti < tlen; ti++ ){
// 	// 	    jQuery("#status").tabs('remove', 0);
// 	// 	    core.kvetch('_d_' + ti);
// 	// 	}
// 	//      jQuery('#mrtabs').html('');
// 	// 	for( var ws in all_workspaces ){
// 	// 	    jQuery("#status").tabs('add', '#' + ws, ws);
// 	// 	}

// 	// Raise old working tab.
// 	jQuery("#status").tabs('select', global_last_tab);

// 	// (Re)connect add_item to events.
// 	jQuery('.addable').submit(function(e){ 
// 	    var iid = e.target.id;
// 	    var ikey = jQuery("#" + iid + "-key").val();
// 	    var iname = jQuery("#" + iid + "-name").val();
// 	    var iws = iid.substring("item-form-".length);
// 	    if( ! iws ){ iws = ''; };
// 	    if( ! ikey ){ ikey = ''; };
// 	    if( ! iname ){ iname = ''; };
// 	    if( iid && iws && iws.length > 0 ){
// 		jQuery.ajax({
// 		    type: "GET",
// 		    url: core.api.workspace.add_item(iws, ikey, 'term', iname),
// 		    dataType: 'json',
// 		    success: WorkspaceUpdateGUI,
// 		    error: function (result, status, error) {
// 		     	alert('Failed server request: ' + status); 
// 		    }
// 		});
// 	    }
// 	    //alert(iid + ":" + iws + ":" + ikey + ":" + iname);
// 	    return false;
// 	});

//     }else{
// 	alert("response failure in update");
//     }
// }


// //
// function _deletable_callback(ws, key_str){

//     jQuery.ajax({
// 	type: "GET",
// 	url: core.api.workspace.remove_item(ws, key_str),
// 	dataType: 'json',
//  	success: WorkspaceUpdateGUI,
//  	error: function (result, status, error) {
//  	    alert('Failed server request: ' + status); 
//  	}
//     });
// }


// ///
// /// Helper functions that should be rolled into a new GUI generation
// /// object at some point.
// ///


// function _status_html(rhash){

//     var buf = new Array();

//     // First, status tab headers.
//     buf.push('<ul>');
//     for( var ws in rhash ){

// 	buf.push('<li><a href="#');
// 	buf.push(ws);
// 	buf.push('"><span>');
// 	buf.push(ws);
// 	buf.push('</span></a></li>');
	
//     }
//     buf.push('</ul>');
	
//     //    Next, status tab contents.
//     buf.push('<div class="ui-layout-content">');
//     for( var ws in rhash ){

// 	buf.push('<div id="');
// 	buf.push(ws);
// 	buf.push('">');

// 	buf.push(_add_item_html(ws));

// 	buf.push('<ul>');
// 	for( var ws_i = 0; ws_i < rhash[ws].length; ws_i++ ){

// 	    var ws_item = rhash[ws][ws_i];
	    
// 	    var key_str  = '';
// 	    var type_str = '';
// 	    var name_str = '';
// 	    var date_str = '';
// 	    if( ws_item.key  ){ key_str = ws_item.key; }
// 	    if( ws_item.type ){ type_str = ws_item.type; }
// 	    if( ws_item.name ){ name_str = ws_item.name; }
// 	    if( ws_item.date ){ date_str = ws_item.date; }

// 	    buf.push('<li>');
// 	    buf.push('<b>key</b>: '   + key_str +
// 		     ' <b>type</b>: ' + type_str +
// 		     ' <b>name</b>: ' + name_str +
// 		     ' <b>date</b>: ' + date_str);
// 	    buf.push('&nbsp;&nbsp;[<span title="delete item" ');
// 	    buf.push('href="#" onclick="');
// 	    buf.push("_deletable_callback('" + ws + "','" + key_str + "');");
// 	    buf.push('" class="deletable_item">delete</span>]');
// 	    buf.push('</li>');
// 	}
// 	buf.push('</ul>');
// 	buf.push('</div>');
//     }
//     buf.push('</div>');

//     return buf.join('');
// }


// // 
// function _add_item_html(ws_key){

//     var buf = new Array();
//     var fname = "item-form-" + ws_key;

//     buf.push('<form action="" class="addable" id="'+ fname +'" method="POST">');
//     buf.push('<input type="hidden" name="mode" value="workspace">');
//     buf.push('<input type="hidden" name="action" value="add_item">');
//     buf.push('<input type="hidden" name="workspace" value="' + ws_key + '">');
//     // TODO: BUG: needs to be free or radio...
//     buf.push('<input type="hidden" name="type" value="term">');
 
//     buf.push('<p>');

//     buf.push('Add item: ');
//     buf.push('<label>key</label>');
//     buf.push('<input class="textBox" type="text" ');
//     buf.push(' id="' + fname + '-key" name="key" size="10" value="">');

//     buf.push('<label>name</label>');
//     buf.push('<input class="textBox" type="text" ');
//     buf.push(' id="' + fname + '-name" name="name" size="10" value="">');

//     buf.push('<input id="' + fname + '-submit' +
// 	     '" type="submit" class="button" value="Add">');
//     buf.push('</p>');
	
//     buf.push('</form>');

//     return buf.join('');
// }


// // 
// function _select_options_html(rhash){

//     var buf = new Array();

//     for( var ws in rhash ){
// 	buf.push('<option value="');
// 	buf.push(ws);
// 	buf.push('">');
// 	buf.push(ws);
// 	buf.push('</option>');
//     }

//     return buf.join('');
// }
