////////////
////
//// org.bbop.amigo.ui.autocomplete
////
//// Purpose: Provide autocomplete insertion for forms
////          
//// DEPENDS: org.bbop.amigo(.core)
//// DEPENDS: org.bbop.amigo.opensearch
//// DEPENDS: org.bbop.amigo.go_meta
//// DEPENDS: com.jquery (1.3.2)
////
//////////


// Module and namespace checking.
// TODO: Will we need a ui class up there?
if ( typeof org.bbop.amigo.ui == "undefined" ){
    org.bbop.amigo.ui = {};
}


//
org.bbop.amigo.ui.autocomplete = function(args){  

    // Bring in utilities and a little API.
    org.bbop.amigo.DEBUG = true;
    var core = new org.bbop.amigo.core();
    var meta = new org.bbop.amigo.go_meta();

    // We'll be doing a lot of debugging.
    function ll(str){ core.kvetch(str); }
    ll("");

    // TODO: Changeover to new system.
    //     // Handle arguments.
    //     var final_args =
    // 	core.utils.merge({id: '_there_needs_to_be_an_id_argument_',
    // 			  search_type: 'general',
    // 			  completion_type: 'acc'}, args);
    //     var text_input_id = final_args['id'];
    //     var ac_type = final_args['id'];
    //     var comp_type = final_args['id'];
    var text_input_id = '_there_needs_to_be_an_id_argument_';
    var ac_type = 'general';
    var ontology = null;
    var comp_type = 'acc';
    var narrow_p = false;
    var jump_p = false;
    if( args ){
	// id may be any string.
	if( args['id'] ){
	    text_input_id = args['id'];
	}
	// search_type may be 'term', 'gene_product', or 'general'.
	if( args['search_type'] ){
	    if( args['search_type'] == 'term' ){
		ac_type = 'term';
	    }else if( args['search_type'] == 'gene_product' ){
		ac_type = 'gene_product';
	    }
	}
	// ontology may be one of the ones defined in go_meta, otherwise null.
	var all_onts_array = meta.ontologies();
	var all_onts_hash = {};
	for( var b = 0; b < all_onts_array.length; b++ ){
	    var ont_key = all_onts_array[b][0];
	    //core.kvetch('saw ont: ' + ont_key);
	    all_onts_hash[ont_key] = true;
	}
	if( args['search_type'] == 'term' &&
	    all_onts_hash[args['ontology']] ){
	    ontology = args['ontology'];
	}
	// completion_type may be 'acc', 'description', or 'completion'.
	if( args['completion_type'] ){
	    if( args['completion_type'] == 'description' ){
		comp_type = 'description';
	    }else if( args['completion_type'] == 'completion' ){
		comp_type = 'completion';
	    }
	}
	// narrow_p may be true or false (default false).
	if( args['narrow'] ){
	    if( args['narrow'] == true || args['narrow'] == 'true' ){
		narrow_p = true;
	    }
	}
	// jump_p may be true or false (default false).
	if( args['jump'] ){
	    if( args['jump'] == true || args['jump'] == 'true' ){
		jump_p = true;
	    }
	}
    }

    ll("id: " + text_input_id);
    ll("search_type: " + ac_type);
    ll("ontology: " + ontology);
    ll("comp_type: " + comp_type);
    ll("narrow_p: " + narrow_p);

    // Add a little randomness to the system to allow multiple
    // instances without any chance of confusion...
    var ac_div_id = "org_bbop_amigo_ui_autocomplete_popup_" + text_input_id;
    var mangle_str = "org_bbop_amigo_id_mangle_" + text_input_id + '_';

    ll("popup id: " + ac_div_id);
    ll("mangle head: " + mangle_str);

    // Suppress autocomplete (at least for FF).
    jQuery("#" + text_input_id).attr('autocomplete', 'off');

    ///
    /// Helper functions.
    ///

    // Functions to endoce and decode data that we'll be hiding
    // in the element ids.
    var coder = new core.util.coder({string: mangle_str, size: 10});

    ///
    ///
    ///

    var class_head = "org_bbop_amigo_ui_autocomplete_";
    var BBOP_AC_BASE_CLASS = class_head + "base";
    var BBOP_AC_UNIQUE_ROW_CLASS = class_head + text_input_id + "_row";
    var BBOP_AC_STANDARD_ROW_CLASS = class_head + "row";
    var BBOP_AC_ROW_EVEN_CLASS = class_head + "row_even";
    var BBOP_AC_ROW_ODD_CLASS = class_head + "row_odd";
    var BBOP_AC_ROW_HOVER_CLASS = class_head + "row_hover";

    ll("unique row class: " + BBOP_AC_UNIQUE_ROW_CLASS);

    // Create a hidden container div for the autocomplete's
    // content's and disappear it.
    jQuery('<div class="' + BBOP_AC_BASE_CLASS + 
	   '" id="' + ac_div_id +
	   '"></div>').appendTo("body").hide();
    // '" id="' + ac_div_id +
    // '"></div>').appendTo(jQuery("#" + text_input_id).parent()).hide();

    // DEBUG/BUG?:
    var input_off = jQuery("#" + text_input_id).offset();
    ll("text pos: top: " + input_off.top +
       " left: " + input_off.left +
       " width: " + jQuery("#" + text_input_id).width() +
       " inner-height: " + jQuery("#" + text_input_id).innerHeight() +
       " outer-height: " + jQuery("#" + text_input_id).outerHeight() );
//     $('#abc').position('#' + text_input_id , {
// 	anchor: ['br', 'tr'],
// 	offset: [-5, 5]
//     });

    // TODO: find the location of the input in the universe.
    var input_height = jQuery("#" + text_input_id).outerHeight();
    var input_pos = jQuery("#" + text_input_id).position();
    var input_top = 0;
    if( input_pos && input_pos.top ){
	input_top = input_pos.top;
    }
    var input_left = 0;
    if( input_pos && input_pos.top ){
	input_left = input_pos.left;
    }

    ll("input loc: top: " + input_top +
       " left: " + input_left +
       " height: " + input_height);
    var popup_top = input_top + input_height;
    var popup_left = input_left;
    ll("popup loc: top: " + popup_top + " left: " + popup_left);

    // It should never 
    var closed_response_set = {};

    // Functions that operate on the core display of the popup.
    function show_me(){ jQuery("#" + ac_div_id).show(); }
    function hide_me(){ jQuery("#" + ac_div_id).hide(); }
    function value_me(str){ jQuery("#" + text_input_id).attr('value', str); }
    function feed_me(str){

	// Add string to DOM.
	jQuery("#" + ac_div_id).html(str);

	// Put it at the correct point in space.
	jQuery("#" + ac_div_id).css('top', popup_top);
	jQuery("#" + ac_div_id).css('left', popup_left);

	// Hunt down our divs by class and add events to them.
	ll("rebind: " + ac_div_id);
	var elts = jQuery("." + BBOP_AC_UNIQUE_ROW_CLASS);
	for( var e = 0; e < elts.length; e++ ){

	    var elt = jQuery(elts[e]);

	    // Add/remove events on mouse over/out events.
	    elt.mouseenter(function(){
		jQuery(this).addClass(BBOP_AC_ROW_HOVER_CLASS);
	    });
	    elt.mouseleave(function(){
		jQuery(this).removeClass(BBOP_AC_ROW_HOVER_CLASS);
	    });
	    elt.click(function(){

		jQuery(this).removeClass(BBOP_AC_ROW_HOVER_CLASS);
		var me_id = coder.decode(jQuery(this).attr('id'));
		ll("click_: " + me_id);

		//
		var ret_val = me_id;
		if( comp_type == 'description' ||
		    comp_type == 'completion' ){
		    ret_val = closed_response_set[me_id][comp_type];
		    //ll("\t_alt_: " + comp_type + ' : ' + ret_val);
		}
		value_me(ret_val);

		// Reset back to nothing (don't let it build up).
		closed_response_set = {};

		hide_me();

		// Only if we're got the jump: true argument.
		if( jump_p ){

		    // Check whether we saw a term or gp.
		    //var re_str = meta.term_regexp();
		    //var re = new RegExp(re_str); 
		    var det_url = '';
		    if( meta.term_id_p(ret_val) ){
			det_url = meta.app_base() + '/' +
			    core.link.term({acc: escape(ret_val)});
			ll("looked like a term: " + det_url);		    
		    }else{
			det_url = meta.app_base() + '/' +
			    core.link.gene_product({acc: escape(ret_val)});
			ll("looked like a gp: " + det_url);
		    }
		    window.location.href = det_url;
		}
	    });
	}
    }

// // NOTE: blur fires before click, so blur on input isn't useful
// // because values won't be filled.
//     // Blur to parent might work...
//     // jQuery("#" + text_input_id).blur(
//     jQuery("#" + text_input_id).parent().blur(
// 	function(){
// 	    //jQuery(this).removeClass(BBOP_AC_ROW_HOVER_CLASS);
// 	    hide_me();
// 	});

    // Try and simulate blur with keydown events.
    // These should be safe according to:
    // http://unixpapa.com/js/key.html
    jQuery("#" + text_input_id).keydown(
	function(e){

	    ll("_input keydown_: " + e.keyCode);
	    if( e.keyCode == 9 ||  // tab
		e.keyCode == 38 || // up
		e.keyCode == 40 ){ // down
		ll("\tinput blur on: " + e.keyCode);
 		hide_me();
	    }
	});

    //
    jQuery("#" + text_input_id).keyup(
	function(e){
	    
	    var query = jQuery(this).val();

	    ll("_input keyup_: " + query + " (" + query.length + ")");

	    // Cut down on overhead a little.
	    if( query && query.length >= 3 ){

		var url = meta.app_base() + '/' +
		    core.api.completion({format: 'amigo',
					 type: ac_type,
					 ontology: ontology,
					 narrow: narrow_p,
					 query: query});
		ll("try: " + url);
	    
		jQuery.ajax({
		    type: "GET",
		    url: url,
		    dataType: 'json',
		    success: renderACPopup,
		    error: function (result, status, error) {
		        //alert('Failed server request: ' + status);
			ll('Failed server request: ' + status);
		    }
		});
	    }else{
		// Remove the current popup from the screen.
		hide_me();
	    }
	    
	    // TODO/BUG: What is this again?
	    //return false;
	});


    // Create the html text string to insert into the popup div.
    function renderACPopup(json_data, status){

	// Remove the current popup from the screen.
	hide_me();

	// Verify the incoming data. Only go if we got something.
	if( core.response.success(json_data) &&
	    core.response.type(json_data) == 'completion' ){

	    var os = new org.bbop.amigo.opensearch(json_data);

	    ll("fixed query: " + os.fixed_query());
	    ll("raw query: " + os.raw_query());
	    ll("response got: " + os.count());

	    var output_stack = new Array;
	    output_stack.push('<div class="');
	    //output_stack.push(BBOP_AC_UL_CLASS);
	    output_stack.push('">');
	    for( var f = 0; f < os.count(); f++ ){

		var desc = os.completion(f, 'description');
		var comp = os.completion(f, 'completion');
		var id = os.completion(f, 'id');

		closed_response_set[id] =
		    {
			description: desc,
			completion: comp,
			acc: id
		    };

		// There is a chance that the completion id undefined
		// (e.g. full name is undef, but found stuff in
		// synonym, etc.).
		if( ! comp ||
		    comp == '' ||
		    comp == ' ' ){
		    if( desc ){
			comp = desc;
		    }else{
			comp = '???';
		    }
		}
		
		ll(f + ": " + id + ' _ ' + desc + ' _ ' + comp);

		var cropped_comp = core.util.crop(comp, 40);
		var cropped_desc = core.util.crop(desc, 15);

		output_stack.push('<div id="');
		output_stack.push(coder.encode(id));
		output_stack.push('" class="');
		output_stack.push(BBOP_AC_UNIQUE_ROW_CLASS);
		output_stack.push(' ');
		output_stack.push(BBOP_AC_STANDARD_ROW_CLASS);
		output_stack.push(' ');
		if( f % 2 == 0 ){
		    output_stack.push(BBOP_AC_ROW_EVEN_CLASS);
		}else{
		    output_stack.push(BBOP_AC_ROW_ODD_CLASS);
		}
		output_stack.push('">');
		output_stack.push('<div style="float:left;" >');
		output_stack.push(cropped_comp);
		output_stack.push('</div>');
		output_stack.push('<div style="float:right;">');
		output_stack.push(cropped_desc);
		output_stack.push('</div>');
		output_stack.push('<br />');
		output_stack.push('</div>');
	    }
 	    output_stack.push('</div>');

	    var outstr = output_stack.join('');
	    
	    //
	    feed_me(outstr);

	    //
	    if( os.count() > 0 ){
		show_me();
	    }
	}    
    }
};

    
