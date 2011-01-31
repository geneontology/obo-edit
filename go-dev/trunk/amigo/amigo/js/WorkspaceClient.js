////
//// Experiments with workspace client.
////
////


// Bring in the AmiGO core.
org.bbop.amigo.DEBUG = true;
var core = new org.bbop.amigo.core();

// Might come in handy for GUI stuff as we go.
var global_layout = null;

//
var global_last_tab = 'default';

//
var global_last_data = {};

// Our separate widget and notice object.
var global_widgets = null;
var global_go_meta = null;


// Get the layout done and request ws info.
function WorkspaceClientInit(){

    core.kvetch('');
    core.kvetch('WorkspaceClientInit start.');

    global_go_meta = new org.bbop.amigo.go_meta();

    ///
    /// Get the layout ready.
    ///

    //global_layout = jQuery('body').layout({ applyDefaultStyles: true });
    core.kvetch('Apply layout...');
    global_layout = jQuery('body').layout({

	//
	applyDefaultStyles: true,

	// North is treated as an island. 
	north__applyDefaultStyles: false,
	north__closable: false,
	north__resizable: false,
	north__slidable: false,

	// South is treated as an island. 
	south__applyDefaultStyles: false,
	south__closable: false,
	south__resizable: false,
	south__slidable: false,

	// We'll need this in certain cases (like map resizing because
	// it's actually donw through CSS).
	// Attach listeners to the center area. TODO: this will be
	// split out into the navi bits later on. For now, just a
	// test.	
	center__onresize: function(){
	    // Apparently, center is not completed yet, so we compute
	    // from all of the compass points.
	    var ymax = global_layout.state.container.innerHeight;
	    ymax = ymax - global_layout.state.north.size;
	    ymax = ymax - global_layout.state.south.size;

	    var xmax = global_layout.state.container.innerWidth;
	    //xmax = xmax - global_layout.state.west.size;
	    //xmax = xmax - global_layout.state.east.size;

	    core.kvetch('_resize_xy_:' + xmax + ':' + ymax);
	}
    });
    // core.kvetch('___' + global_layout);
    // core.kvetch('WorkspaceClient init completed.');
    // core.kvetch('Contacting server for meta-information.');
    global_widgets = new org.bbop.amigo.ui.widgets();

    // Go and get the workspace status information so we can finish the UI.
    jQuery.ajax({
	type: "GET",
	url: core.api.workspace.status(), 
	dataType: 'json',
 	success: WorkspaceCreateGUI,
 	error: function (result, status, error) { 
 	    alert('Failed server request: ' + status); 
 	} 
    });    
}


// Wipe errors. Were there any new errors? Report only the first.
function HandleErrors(jblob){
    if( core.response.errors(jblob).length > 0 ){
	global_widgets.error(core.response.errors(jblob).join('<br />'));
    }
}


// Build the necessary GUIs from the ws info, and ready the
// callbacks dor the rest of the application.
function WorkspaceCreateGUI(json_data, status){
    
    core.kvetch('WorkspaceCreateGUI start.');
 
    // TODO: Build GUI using things we find in go meta.
    if( core.response.success(json_data) ){

	core.kvetch('We have "' +
		    core.response.type(json_data) + '" creation data.');

	//
	HandleErrors(json_data);

	var ws_api = new org.bbop.amigo.workspace(json_data);

	var all_workspaces = json_data.results;
	global_last_data = all_workspaces;

	// Create and populate the status tabs.
	core.kvetch('Tabify...');
	//var status_string = _status_html(all_workspaces);
 	//jQuery("#status").html(status_string);
	jQuery("#status").tabs();
	jQuery("#wsp-cont-tabs").tabs();
	//jQuery("#workspace-control-tabs").tabs();
	jQuery("#wsp-cont-tabs").tabs('select', 0);
	
	// Connect add_workspace to event.
	jQuery('#ws-add-form').submit(function(){ 
	    var ws = jQuery("#ws-add-form-workspace").val()
            jQuery.ajax({
    		type: "GET",
    		url: core.api.workspace.add(ws),
    		dataType: 'json',
     		//success: WorkspaceUpdateGUI,
     		success: _chain_to_update,
     		error: function (result, status, error) {
     		    alert('Failed server request: ' + status); 
     		}
            });
	    return false;
	});

	// Connect copy_workspace to event.
	jQuery('#ws-copy-form').submit(function(){ 
	    var ws_from = jQuery("#ws-copy-form-workspace-from").val();
	    var ws_to = jQuery("#ws-copy-form-workspace-to").val();
	    if( ws_from && ws_to ){
		core.kvetch("OP: " + core.api.workspace.copy(ws_from, ws_to));
		jQuery.ajax({
		    type: "GET",
		    url: core.api.workspace.copy(ws_from, ws_to),
		    dataType: 'json',
		    success: _chain_to_update,
		    error: function (result, status, error) {
			alert('Failed server request: ' + status); 
		    }
		});
	    }
	    return false;
	});

	// Connect clear_workspace to event.
	jQuery('#ws-clear-form').submit(function(){ 
	    var ws = jQuery("#ws-clear-form-workspace").val()
	    if( ws ){
		jQuery.ajax({
		    type: "GET",
		    url: core.api.workspace.clear(ws),
		    dataType: 'json',
		    //success: WorkspaceUpdateGUI,
		    success: _chain_to_update,
		    error: function (result, status, error) {
		     	alert('Failed server request: ' + status); 
		    }
		});
	    }
	    return false;
	});

	// Connect remove_workspace to event.
	jQuery('#ws-remove-form').submit(function(){ 
	    var ws = jQuery("#ws-remove-form-workspace").val()
	    if( ws ){
		jQuery.ajax({
		    type: "GET",
		    url: core.api.workspace.remove(ws),
		    dataType: 'json',
		    //success: WorkspaceUpdateGUI,
		    success: _chain_to_update,
		    error: function (result, status, error) {
		     	alert('Failed server request: ' + status); 
		    }
		});
	    }
	    return false;
	});

	// Connect chaining to event.
	jQuery('#ws-chain-form').submit(function(){ 

	    var ws = jQuery("#ws-chain-form-workspace").val()
	    var dest = jQuery("#ws-chain-form-destination").val()

	    core.kvetch("+applying " + ws + ' to ' + dest + "+");

	    // if( ws && dest && global_last_data[ws] ){
	    if( ws && dest && ws_api.has_workspace(ws) ){

		core.kvetch("+" + ws_api.has_workspace(ws) + "+");

		// TODO/BUG: can this somehow be rolled into core?
		// This must be changed to some kind of proxying
		// mechanism at some point anyways because once the
		// arg lists get too large, GETing will fail. I
		// suggest jumping to a form with the intention as the
		// argument, with the form being auto-generated from
		// the intention and the data hidden inside. Once
		// created, automatically POST to the
		// target. Shouldn't be too hard, just harder than
		// this, and I'm time strapped right now.
		if( dest == 'go_navigation' ){
		    var all_ws_terms = ws_api.list_terms(ws);
		    var chain_link = core.link.layers_graph({
			terms: all_ws_terms
		    });
		    core.kvetch("+goto: " + chain_link + "+");
		    window.location.href = chain_link;
		}else if( dest == 'term_enrichment' ){
		    var all_ws_gps = ws_api.list_non_terms(ws);
		    var chain_link = core.link.term_enrichment({
			gp_list: all_ws_gps
		    });
		    core.kvetch("+goto: " + chain_link + "+");
		    window.location.href = chain_link;
		}else if( dest == 'slimmer' ){
		    var all_ws_terms = ws_api.list_terms(ws);
		    var all_ws_gps = ws_api.list_non_terms(ws);
		    var chain_link = core.link.slimmer({
			slim_list: all_ws_terms,
			gp_list: all_ws_gps
		    });
		    core.kvetch("+goto: " + chain_link + "+");
		    window.location.href = chain_link;
		}else if( dest == 'nmatrix' ){
		    var all_ws_terms = ws_api.list_terms(ws);
		    var chain_link = core.link.nmatrix({
			//term_set_1: all_ws_terms,
			terms: all_ws_terms
		    });
		    core.kvetch("+goto: " + chain_link + "+");
		    window.location.href = chain_link;
		}else if( dest == 'blast' ){
		    global_widgets.error('BLAST chain:<br />not yet implemented');
		}
	    }
	    return false;
	});

	//
	WorkspaceUpdateGUI(json_data, status);

    }else{
	alert("response failure in create");
    }
}


// Catch ajax requests with uninteresting results and call out to get
// GUI update information.
function _chain_to_update(json_data, status){

    // TODO: Build GUI using things we find in go meta.
    if( core.response.success(json_data) ){

	HandleErrors(json_data);

	jQuery.ajax({
	    type: "GET",
	    url: core.api.workspace.status(),
	    dataType: 'json',
	    success: WorkspaceUpdateGUI,
	    error: function (result, status, error) {
		alert('Failed server request: ' + status); 
	    }		    
	});

    }else{
	alert('ping');
    }
}


// Refresh what is on the screen with the current data.
function WorkspaceUpdateGUI(json_data, status){
    
    core.kvetch('WorkspaceUpdateGUI start.');
 
    // TODO: Build GUI using things we find in go meta.
    if( core.response.success(json_data) ){

	core.kvetch('We have "' +
		    core.response.type(json_data) + '" update data.');

	//
	HandleErrors(json_data);

	var all_workspaces = json_data.results;
	global_last_data = all_workspaces;

	//
	core.kvetch('Update workspace operations...');
	var ws_opts = _select_options_html(all_workspaces);
	jQuery('#ws-copy-form-workspace-from').html(ws_opts);
	jQuery('#ws-copy-form-workspace-to').html(ws_opts);
	jQuery('#ws-clear-form-workspace').html(ws_opts);
	jQuery('#ws-remove-form-workspace').html(ws_opts);
	jQuery('#ws-chain-form-workspace').html(ws_opts);

	// Save raised tab state.
	var selected = jQuery('#status').tabs('option', 'selected');
	global_last_tab = 0;
	if( selected > 0 ){
	    global_last_tab = selected;
	}

	// Destroy and create tabs.
 	jQuery("#status").tabs('destroy');
 	jQuery("#status").html(_status_html(all_workspaces));
	jQuery("#status").tabs();

	// Raise old working tab.
	jQuery("#status").tabs('select', global_last_tab);

    }else{
	alert("response failure in update");
    }
}


//
function _deletable_callback(ws, key_str){

    jQuery.ajax({
	type: "GET",
	url: core.api.workspace.remove_item(ws, key_str),
	dataType: 'json',
 	//success: WorkspaceUpdateGUI,
 	success: _chain_to_update,
 	error: function (result, status, error) {
 	    alert('Failed server request: ' + status); 
 	}
    });
}


///
/// Helper functions that should be rolled into a new GUI generation
/// object at some point.
///


function _status_html(rhash){

    var buf = new Array();

    // First, status tab headers.
    buf.push('<ul>');
    for( var ws in rhash ){

	buf.push('<li><a href="#');
	buf.push(ws);
	buf.push('"><span>');
	buf.push(ws);
	buf.push('</span></a></li>');
	
    }
    buf.push('</ul>');
	
    //    Next, status tab contents.
    buf.push('<div class="ui-layout-content">');
    for( var ws in rhash ){

	buf.push('<div id="');
	buf.push(ws);
	buf.push('">');

	//
	if( rhash[ws].length == 0 ){
	    buf.push(_empty_message());
	}
	
	buf.push('<ul>');
	for( var ws_i = 0; ws_i < rhash[ws].length; ws_i++ ){

	    var ws_item = rhash[ws][ws_i];
	    
	    var key_str  = '';
	    var name_str = '';
	    var date_str = '';
	    if( ws_item.key  ){ key_str = ws_item.key; }
	    if( ws_item.name ){ name_str = ws_item.name; }
	    if( ws_item.date ){ date_str = ws_item.date; }

	    buf.push('<li>');
	    buf.push('<b>acc</b>: '   + key_str +
		     ' <b>label</b>: ' + name_str +
		     ' <b>date</b>: ' + date_str);
	    // buf.push('&nbsp;&nbsp;[<span title="delete item" ');
	    // buf.push('href="#" onclick="');
	    // buf.push("_deletable_callback('" + ws + "','" + key_str + "');");
	    // buf.push('" class="deletable_item">delete</span>]');
	    buf.push('&nbsp;&nbsp;<img title="delete item" src="');
	    buf.push(global_go_meta.image_base() + '/fatal.png');
	    buf.push('" onclick="');
	    buf.push("_deletable_callback('" + ws + "','" + key_str + "');");
	    buf.push('" class="deletable_item"');
	    buf.push('" />');

	    buf.push('</li>');
	}
	buf.push('</ul>');
	buf.push('</div>');
    }
    buf.push('</div>');

    return buf.join('');
}


//
function _empty_message(){

    var buf = new Array();

    buf.push('<h4>');
    buf.push('Empty.');
    buf.push('</h4>');

    buf.push('<p>');
    buf.push('To fill your cart with items, ');
    buf.push('please click on the "Usables" tab ');
    buf.push('and select and activity.');
    buf.push('</p>');

    buf.push('<p>');
    buf.push('If items you added have not appeared, ');
    buf.push('please try clicking "refresh" under the "Options" tab.');
    buf.push('</p>');

    return buf.join('');
}


// 
function _select_options_html(rhash){

    var buf = new Array();

    for( var ws in rhash ){
	buf.push('<option value="');
	buf.push(ws);
	buf.push('">');
	buf.push(ws);
	buf.push('</option>');
    }

    return buf.join('');
}
