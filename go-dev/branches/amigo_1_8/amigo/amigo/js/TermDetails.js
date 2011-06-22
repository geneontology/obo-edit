////
////
////


// Bring in the AmiGO core.
org.bbop.amigo.DEBUG = true;
var core = new org.bbop.amigo.core();

// var global_go_meta = null;
// var global_widgets = null;

//
function TermDetailsInit(){

    core.kvetch('');
    core.kvetch('TermDetailsInit start...');

    // var prev_source_count = 0;
    // var prev_species_count = 0;

    // Bring in meta information.
    var go_meta = new org.bbop.amigo.go_meta();
    var species_data = go_meta.species();
    var source_data = go_meta.sources();

    ///
    /// Tabify the layout if we can (may be in a non-tabby version).
    ///

    var dtabs = jQuery("#display-tabs");
    if( dtabs ){
	core.kvetch('Apply tabs...');
	jQuery("#display-tabs").tabs();
	//dtabs.tabs();
	jQuery("#display-tabs").tabs('select', 0);
    }

    // ///
    // /// Add everything for inferred line graphics.
    // ///

    // // Tired--let's just make a new lexical scope.
    // function _clear_old(addr){
    // 	var elt = jQuery('#' + addr);
    // 	if( elt.children() ){
    // 	    elt.children().remove();
    // 	}
    // }
    // function _draw_vert_line(addr){
    // 	var elt = jQuery('#' + addr);
    // 	elt.html('<table class="override" style="margin: 0px; padding: 0px; width: 1.5em; height: 1.668em; border-collapse: collapse;" cellpadding="0" cellspacing="0"><tr><td class="light-cell"></td><td class="dark-cell"></td><td class="light-cell"></td></tr><tr><td class="light-cell"></td><td class="dark-cell"></td><td class="light-cell"></td></tr><tr><td class="light-cell"></td><td class="dark-cell"></td><td class="light-cell"></td></tr></table>');
    // }
    // function _draw_horiz_line(addr){
    // 	var elt = jQuery('#' + addr);
    // 	elt.html('<table class="override" style="margin: 0px; padding: 0px; width: 1.5em; height: 1.6666666667em; border-collapse: collapse;" cellpadding="0" cellspacing="0"><tr><td class="light-cell"></td><td class="light-cell"></td><td class="light-cell"></td></tr><tr><td class="dark-cell"></td><td class="dark-cell"></td><td class="dark-cell"></td></tr><tr><td class="light-cell"></td><td class="light-cell"></td><td class="light-cell"></td></tr></table>');
    // }
    // function _draw_third_quad(addr){
    // 	var elt = jQuery('#' + addr);
    // 	elt.html('<span>inf</span>');
    // }
    // function _draw_fourth_quad(addr){
    // 	var elt = jQuery('#' + addr);
    // 	elt.html('<table class="override" style="margin: 0px; padding: 0px; width: 1.5em; height: 1.6666666667em; border-collapse: collapse;" cellpadding="0" cellspacing="0"><tr><td class="light-cell"></td><td class="light-cell"></td><td class="light-cell"></td></tr><tr><td class="light-cell"></td><td class="dark-cell"></td><td class="dark-cell"></td></tr><tr><td class="light-cell"></td><td class="dark-cell"></td><td class="light-cell"></td></tr></table>');
    // }
    // function _clear_old(addr){
    // 	var elt = jQuery('#' + addr);
    // 	if( elt.children() ){
    // 	    elt.children().remove();
    // 	}
    // }
    // function _addr_str(one, two){
    // 	return 'addr-' + one + '-' + two;
    // }
    // function _tag_action(iacc){
    // 	return function(event){

    // 	    // Preamble.
    // 	    event.stopPropagation();
    // 	    //core.kvetch('inside: ' + iacc);
    // 	    //core.kvetch('e: ' + event.type);

    // 	    // Build paths.
    // 	    var from_addr = global_addresses[iacc];
    // 	    var to_addr = global_home_address;
    // 	    var from_row = parseInt(from_addr['row']);
    // 	    var from_col =  parseInt(from_addr['column']);
    // 	    var to_row = parseInt(to_addr['row']);
    // 	    var to_col = parseInt(to_addr['column']);
    // 	    //
    // 	    var fourth_quad = [];
    // 	    var third_quad = [];
    // 	    var vert_lines = [];
    // 	    var horiz_lines = [];
	    
    // 	    // Do corners.
    // 	    fourth_quad.push(_addr_str(from_row, from_col));
    // 	    third_quad.push(_addr_str(to_row, from_col));

    // 	    // Figure vertical line.
    // 	    for( var down = from_row + 1; down < to_row; down++ ){
    // 		vert_lines.push(_addr_str(down, from_col));
    // 	    }

    // 	    // Figure horizontal line.
    // 	    for( var across = from_col + 1; across <= to_col; across++ ){
    // 		horiz_lines.push(_addr_str(to_row, across));
    // 	    }

    // 	    // Either draw or erase, depending on the type of event
    // 	    // (need the above information either way).
    // 	    if( event.type == 'mouseenter'){
    // 		var color = 'green';
    // 		(function(){
    // 		    for(var i = 0; i < fourth_quad.length; i++){
    // 			//jQuery('#' + fourth_quad[i]).css({'background-color':color});
    // 			_draw_fourth_quad(fourth_quad[i]);
    // 		    }
    // 		})();
    // 		(function(){
    // 		    for(var i = 0; i < third_quad.length; i++){
    // 			//jQuery('#' + third_quad[i]).css({'background-color':color});
    // 			_draw_third_quad(third_quad[i]);
    // 		    }
    // 		})();
    // 		(function(){
    // 		    for(var i = 0; i < vert_lines.length; i++){
    // 			//jQuery('#' + vert_lines[i]).css({'background-color':color});
    // 			_draw_vert_line(vert_lines[i]);
    // 		    }
    // 		})();
    // 		(function(){
    // 		    for(var i = 0; i < horiz_lines.length; i++){
    // 			//jQuery('#' + horiz_lines[i]).css({'background-color':color});
    // 			_draw_horiz_line(horiz_lines[i]);
    // 		    }
    // 		})();
    // 	    }else{
    // 		var color = 'white';
    // 		(function(){
    // 		    for(var i = 0; i < fourth_quad.length; i++){
    // 			//jQuery('#' + fourth_quad[i]).css({'background-color':color});
    // 			_clear_old(fourth_quad[i]);
    // 		    }
    // 		})();
    // 		(function(){
    // 		    for(var i = 0; i < third_quad.length; i++){
    // 			//jQuery('#' + third_quad[i]).css({'background-color':color});
    // 			_clear_old(third_quad[i]);
    // 		    }
    // 		})();
    // 		(function(){
    // 		    for(var i = 0; i < vert_lines.length; i++){
    // 			// jQuery('#' + vert_lines[i]).css({'background-color':color});
    // 			_clear_old(vert_lines[i]);
    // 		    }
    // 		})();
    // 		(function(){
    // 		    for(var i = 0; i < horiz_lines.length; i++){
    // 			//jQuery('#' + horiz_lines[i]).css({'background-color':color});
    // 			_clear_old(horiz_lines[i]);
    // 		    }
    // 		})();
    // 	    }
    // 	}
    // }
    // (function(){

    // 	// Add hook to every inferred row.
    // 	for( var iacc in global_addresses ){

    // 	    var imap = global_acc_to_rand[iacc];
    // 	    var grab_key = 'grab-' + imap
    // 	    var elt = jQuery('#' + grab_key);
    // 	    // var elt_id = elt.attr('id');

    // 	    // core.kvetch('attaching: ' + grab_key);
    // 	    // core.kvetch('to: ' + elt);
    // 	    // core.kvetch('aka: ' + elt_id);
	    
    // 	    elt.hover(_tag_action(iacc));
    // 	}

    // })();

    ///
    /// Add everything for filters.
    ///

    //
    addFiltersBlock(go_meta.image_base());

    // Add gene product count control panel.
    var widgets = new org.bbop.amigo.ui.widgets();
    var source_text = widgets.form.multiselect('source', 'source', 4, 
					       source_data, 'Data source');
    var species_text = widgets.form.multiselect('species', 'species', 4,
						species_data, 'Species');
    jQuery("#app-term-filter-source").append(source_text);
    jQuery("#app-term-filter-species").append(species_text);

    // TODO/BUG: Add gene product count control panel listener.
    var marshal_filter_form = 
	widgets.form.create_jquery_marshal('#app-term-filter',
					   ['option:selected']);
    function _generate_action(marshaller){
	return function(event){

	    var elt = null;
	    if( event ){
		// Reset the other (unique for now).
		elt =  jQuery(event.currentTarget);
		if( elt.attr('id') == 'app-term-filter-species' ){
		    jQuery('#source').val('');
		}else{
		    jQuery('#species').val('');
		}
	    }

	    // Marshall.
	    var all_inputs = marshaller();
	    var source_array = all_inputs['source'];
	    var species_array = all_inputs['species'];

	    // Array to hash.
	    var source_hash = {};
	    var species_hash = {};
	    for( var i = 0; i < source_array.length; i++ ){
		source_hash[source_array[i]] = true;
	    }
	    for( var i = 0; i < species_array.length; i++ ){
		species_hash[species_array[i]] = true;
		//core.kvetch('_s_ ' + species_array[i]);
	    }

	    // Do we have some kind of filter? We'll do to source
	    // unless it is clear otherwise.
	    var filter = 'species';
	    var no_filter = false;
	    var spec_count = species_array.length;
	    if( spec_count == 1 && species_array[0] == '' ){
		filter = 'source';

		//
		var src_count = source_array.length;
		if( src_count == 1 && source_array[0] == '' ){
		    no_filter = true;
		}
	    }
	    
	    // We'll default to
	    var ccount = {};
	    for( var tacc in global_count_data ){
		// Check and set.
		if( ! ccount[tacc] ){
		    ccount[tacc] = 0;
		}

		// TODO: Increment with filters.
		var curr_term = global_count_data[tacc];
		for( var ind = 0; ind < curr_term.length; ind++ ){
		    var curr_item = curr_term[ind];
		    // No filter drop through on the source side.
		    if( no_filter ){
			if( curr_item['dbname'] ){
			    ccount[tacc] =
				ccount[tacc] + parseInt(curr_item['count']);
			}
		    }else if( filter == 'source' ){
			if( curr_item['dbname'] &&
			    source_hash[curr_item['dbname']] ){
			    ccount[tacc] =
				ccount[tacc] + parseInt(curr_item['count']);
			}
		    }else{
			//core.kvetch('_in_: ' + tacc + ', ' + ind);
			if( curr_item['ncbi_taxa_id'] &&
			    species_hash[curr_item['ncbi_taxa_id']] ){
			    //core.kvetch('_n1_: ' + '');
			    ccount[tacc] =
				ccount[tacc] + parseInt(curr_item['count']);
			}
		    }
		}
		//core.kvetch('_c_ ' + tacc + ': ' + ccount[tacc]);
	    }

	    // ...
	    for( var racc in global_acc_to_rand ){

		//
		var different_ids = [
		    global_acc_to_rand[racc],
		    global_acc_to_rand[racc] + '_a',
		    // global_acc_to_rand[racc] + '_n'
		    global_acc_to_rand[racc] + '_z'
		];
		var match_a = new RegExp("_a$");
		// var match_n = new RegExp("_n$");
		var match_z = new RegExp("_z$");

		for( var did = 0; did <= different_ids.length; did++ ){

		    var did_acc = different_ids[did];
		    
		    //var relt = jQuery('#' + global_acc_to_rand[racc]);
		    var relt = jQuery('#' + did_acc);
		    
		    // Calculate the filtered link to term-assoc.
		    // TODO:
		    // speciesdb
		    // taxid
		    var link_source_filter = [];
		    var link_species_filter = [];
		    if( filter == 'source' ){
			link_source_filter = source_array;
			link_species_filter = ['all'];
		    }else{
			link_species_filter = species_array;
			link_source_filter = ['all'];
		    }
		    var acc_link =
			core.link.term_assoc({
			    speciesdb: link_source_filter,
			    taxid: link_species_filter,
			    acc: racc
			});
		    //core.kvetch('GPC LINK: ' + acc_link);
		    
		    // Clear away old text.
		    // core.kvetch('...' + relt);
		    if( relt.children() ){
			relt.children().remove();
		    }
		    
		    // Add text (different styles for different pages).
		    // First is for inferred tree...
		    if( ! match_a.test(did_acc)	&& ! match_z.test(did_acc) ){
			// && ! match_n.test(did_acc) ){
			if( ccount[racc] && racc == global_acc ){
			    relt.html('<b><a style="" href="' + acc_link + '" title="View gene products associated with this term">[' + ccount[racc] + '&nbsp;gene&nbsp;products]</a></b>');
			}else if( ccount[racc] ){
			    relt.html('<a style="" href="' + acc_link + '" title="View gene products associated with this term">[' + ccount[racc] + '&nbsp;gene&nbsp;products]</a>');
			}else{
			    // Need this, or not?
			    relt.html('[0&nbsp;gene&nbsp;products]');
			}
                    // This is for the others... 
		    }else{
			if( ccount[racc] && racc == global_acc ){
			    // BUG: revert after dealing with
			    // tablesorter/jquery bug.
			    relt.html('<b><a style="" href="' + acc_link + '" title="View gene products associated with this term">' + ccount[racc] + '</a></b>');
			    //relt.html(ccount[racc]);
			}else if( ccount[racc] ){
			    // BUG: revert after dealing with
			    // tablesorter/jquery bug.
			    relt.html('<a style="" href="' + acc_link + '" title="View gene products associated with this term">' + ccount[racc] + '</a>');
			    //relt.html(ccount[racc]);
			}else{
			    // Need this, or not?
			    relt.html('0');
			}
		    }
		}	    
	    }
	    // Trigger sorting action after table is populated.
	    // jQuery("#neighborhood-table-above").trigger('update');
	    // jQuery("#neighborhood-table-below").trigger('update');
	    jQuery("#all-table-above").trigger('update');
	    jQuery("#all-table-below").trigger('update');
	    // TODO/BUG: recolor?
	};
    }

    var filter_saction = _generate_action(marshal_filter_form);

    // Attach to event.
    jQuery("#app-term-filter-source").change(filter_saction);
    jQuery("#app-term-filter-species").change(filter_saction);

    // Sound off for the first time through on its own.
    filter_saction(null);

    // Callback for text extraction work form table.
    core.kvetch('Sorting tables.');
    var ts_callback = function(node) { 
    	var retval = jQuery(node).text();
    	return retval; 
    };
    // jQuery("#neighborhood-table-above").tablesorter(
    // 	{ 
    // 	    textExtraction: ts_callback,
    // 	    // widgets: ['zebra'],
    //         headers: { 3: { sorter:'integer' }} 
    // 	});
    // jQuery("#neighborhood-table-below").tablesorter(
    // 	{ 
    // 	    textExtraction: ts_callback,
    // 	    // widgets: ['zebra'],
    //         headers: { 3: { sorter:'integer' }} 
    // 	});
    jQuery("#all-table-above").tablesorter(
	{ 
	    textExtraction: ts_callback,
	    // widgets: ['zebra'],
            headers: { 3: { sorter:'integer' }} 
	});
    jQuery("#all-table-below").tablesorter(
	{ 
	    textExtraction: ts_callback,
	    // widgets: ['zebra'],
            headers: { 3: { sorter:'integer' }} 
	});

    //
    core.kvetch('TermDetailsInit done.');
}


function addFiltersBlock(base){

    core.kvetch('adding filters...');
    
    var str = '<dl><dt id="filterToggle" class="toggle">Filter lineage gene product counts <a href="http://wiki.geneontology.org/index.php/AmiGO_Manual:_Term_Details#Term_Lineage" class="help-link"><img src="' + base + '/help.png" alt="help!"></a></dt><dd id="filterDiv"><form id="app-term-filter" name="app-term-filter"><fieldset id="app-term-filter-source" class="floatL"></fieldset><fieldset id="app-term-filter-species" class="floatL"></fieldset></form><hr class="clearL"></dd></dl>';
    
    jQuery('#gp-count-filters').html(str);
}
