////
//// Tie some cart actions into the straight HTML mode_matrix page.
//// Not sure if I like doing things this way.
////


// Bring in the AmiGO core.
org.bbop.amigo.DEBUG = true;
var core = new org.bbop.amigo.core();
var widgets = new org.bbop.amigo.ui.widgets();
var std_ui = null;
var ws = null;
var cart = null;

// Get the layout done and request ws info.
function NMatrixInit(){

    core.kvetch('');
    core.kvetch('NMatrixInit start...');

    //
    std_ui = new org.bbop.amigo.ui.standard();
    widgets = new org.bbop.amigo.ui.widgets();
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
	    ws = new org.bbop.amigo.workspace(json_data);
	    cart.update_workspaces(ws.list_workspaces());

// 	    // Unwind global map into cart map.
// 	    for( var cmid in global_cart_map ){
		
// 		var gps = global_cart_map[cmid];
// 		for( var g = 0; g < gps.length; g++){
		    
// 		    var gp_acc = gps[g];
// 		    cart.add_map(cmid, new org.bbop.amigo.item(gp_acc, ''));
// 		}
// 	    }
	    
// 	    cart.bind('cart-bindable');		
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

    ///
    /// TODO: tabify 3d in results if we find the class.
    ///

    var three_d_tabs = jQuery("#" + "3d-tabs");
    if( three_d_tabs ){
	core.kvetch('Apply 3d-tabs...');
	jQuery("#three-d-tabs").tabs();
	jQuery("#three-d-tabs").tabs('select', 0);
    }

}


//
function forward_to_term_details (go_id){
    var link = core.link.term({acc: go_id});
    window.location = link;
}


// Assumes the existance and structure of global_matrix (the JSified
// version of the NMatrix output).
function info_shield (axis_list){

//     var wait_id = 'wait_dialog_id_' + core.util.randomness(10);
//     var html_string = 'Creating dialog...';
//     var wait_text = '<div id="' + wait_id + '">' + html_string + '</div>';

//     jQuery("body").append(jQuery(wait_text));    
//     jQuery("#" + wait_id ).dialog({
// 	bgiframe: true,
// 	autoOpen: false,
// 	height: 300,
// 	width: 500,
// 	modal: true,
// 	close: function(){
// 	}
//     });
//     jQuery('#' + wait_id).dialog('open');


    widgets.start_wait("Processing...");
    
    //
    cart.reset_map();

    // Generate shield HTML.
    // Create the shield
    var shield_html = info_html(axis_list);
    new org.bbop.amigo.ui.shield.set(shield_html);
    cart.bind('cart-bindable');

    widgets.finish_wait();

    //jQuery('#' + wait_id).dialog('close');
}


///
/// HTML generation.
/// Might want to move some of this to widgets eventually.
///


// A frame to hold the information.
function info_html(alist){

    var output = '';
//    output = output + '<div class="block">';
//    output = output + '<h2>Intersection Information</h2>';
    output = output + group_info_html(alist); 
    output = output + term_info_html(alist); 
    output = output + gp_info_html(alist);
//    output = output + '</div>';
    return output;
}


//
function group_info_html(alist){

    var mbuf = new Array();

    // Extract information from our global variable.
    var info_bloc = extract_info_block(alist);
    var gp_list = info_bloc['gene_products'];
    var gp_count = info_bloc['gene_product_count'];
    var gp_list_link = info_bloc['link'];

    //
    //mbuf.push('<h4>Group</h4>');

    //
    mbuf.push('<div>View gene products as an');
    mbuf.push('<a href="');
    mbuf.push(gp_list_link);
    mbuf.push('">association list</a>');

    //
    //var cmid = core.util.randomness(10);
    var cmid = 'all_gps';
    for( var gpi = 0; gpi < gp_list.length; gpi++ ){
	var acc = gp_list[gpi];
	cart.add_map(cmid, new org.bbop.amigo.item(acc, ''));
    }
    mbuf.push('&nbsp;');
    mbuf.push(cart_img('all_gps'));

    mbuf.push('</div>');

    //mbuf.push('<br />');

    return mbuf.join("\n");
}


//
function term_info_html(alist){

    var mbuf = new Array();

    mbuf.push('<h4>Terms</h4>');

    var rows = [];
    for( var t = 0; t < alist.length; t++ ){

	var tds = [];
	var acc = alist[t];
	var term_info = global_term_info[acc];

	//
	tds.push('<a href="' +
		 term_info['term_link'] +
		 '">' +
		 acc +
		 '</a>');

	//
	tds.push(term_info['name']);

	//
	tds.push(cart_img(acc));

	//
	//var cmid = core.util.randomness(10);
	var cmid = acc;
	cart.add_map(cmid, new org.bbop.amigo.item(acc, ''));

	rows.push(tds);
    }

    mbuf.push(widgets.table.simple([], rows));

    return mbuf.join("");
}


//
function gp_info_html(alist){

    var mbuf = new Array();

    // Extract information from our global variable.
    var info_bloc = extract_info_block(alist);
    var gp_list = info_bloc['gene_products'];
    var gp_count = info_bloc['gene_product_count'];
    var gp_list_link = info_bloc['link'];

    //
    mbuf.push('<h4>Gene Products</h4>');

    var rows = [];
    for( var g = 0; g < gp_list.length; g++ ){

	var tds = [];
	var acc = gp_list[g];

	//
	tds.push('<a href="' +
		 core.link.gene_product({'acc': acc}) +
		 '">' +
		 global_gene_product_info[acc].symbol +
		 '</a>');

	//
	tds.push(global_gene_product_info[acc].full_name);

	//
	tds.push('<a class="blast-link" title="Use sequence as BLAST query"' +
		 ' href="' +
		 core.link.single_blast({'acc': acc}) +
		 '">BLAST</a>');

	//
	tds.push(cart_img(acc));

	//
	//var cmid = core.util.randomness(10);
	var cmid = acc;
	cart.add_map(cmid, new org.bbop.amigo.item(acc, ''));

	//
	rows.push(tds);
    }

    //
    //mbuf.push(widgets.table.simple(['acc', 'name', 'blast?', 'cart'], rows));
    mbuf.push(widgets.table.simple([], rows));

    //mbuf.push('</div>');

    return mbuf.join("\n");
}


//
function cart_img (uuid){
    var mbuf = new Array();
    mbuf.push('<img width="20px"');
    mbuf.push('height="13px"');
    mbuf.push('id="' + uuid + '"');
    mbuf.push('src="' + global_cart_image + '"');
    mbuf.push('class="cart-bindable"');
    mbuf.push('alt="AmiGO cart."');
    mbuf.push('title="Add to your cart." />');
    return mbuf.join('');
}


//
function extract_info_block(alist){
    var info_bloc = null;
    if( alist.length == 2 ){
	info_bloc = global_matrix[alist[0]][alist[1]];
    }else{
	info_bloc = global_matrix[alist[0]][alist[1]][alist[2]];
    }
    return info_bloc;
}
