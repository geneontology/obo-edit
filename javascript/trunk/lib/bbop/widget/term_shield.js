/*
 * Package: term_shield.js
 * 
 * Namespace: bbop.widget.term_shield
 * 
 * BBOP object to produce a self-constructing/self-destructing term
 * information shield.
 * 
 * TODO: A work in progress.
 * 
 * This is a completely self-contained UI and manager.
 */

bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'logger');
//bbop.core.require('bbop', 'model');
//bbop.core.require('bbop', 'model', 'graph', 'bracket');
bbop.core.require('bbop', 'html');
bbop.core.require('bbop', 'golr', 'manager', 'jquery');
bbop.core.namespace('bbop', 'widget', 'term_shield');

/*
 * Constructor: term_shield
 * 
 * Contructor for the bbop.widget.term_shield object.
 * 
 * This is (sometimes) a specialized (and widgetized) subclass of
 * <bbop.golr.manager.jquery>.
 * 
 * TODO: Will display fields with weights ??? through ???.
 * 
 * TODO: A work in progress.
 * 
 * Arguments:
 *  item - string (term id) or solr-returned json response
 *  linker - a "linker" object
 *  golr_conf_class_obj - a <bbop.golr.conf_class> object
 *  golr_loc - *[optional]* string url to GOlr server; not needed if local
 * 
 * Returns:
 *  self
 */
bbop.widget.term_shield = function(item, linker, golr_conf_class_obj, golr_loc){

    // Per-UI logger.
    var logger = new bbop.logger();
    logger.DEBUG = true;
    function ll(str){ logger.kvetch('W (term_shield): ' + str); }

    //bbop.golr.manager.jquery.call(this, golr_loc, golr_conf_obj);
    this._is_a = 'bbop.widget.term_shield';

    // 
    //var anchor = this;
    //var loop = bbop.core.each;
    // Successful callbacks call draw_rich_layout.
    // anchor.register('search', 'do', draw_rich_layout);

    // ...
    function draw_shield(doc){
	
	//ll(doc['id']);

	var txt = 'Nothing here...';
	if( doc ){

	    var tbl = new bbop.html.table();
	    var results_order =
		golr_conf_class_obj.field_order_by_weight('result');
	    var each = bbop.core.each; // conveience
	    each(results_order,
		 function(fid){
		     // 
		     var field = golr_conf_class_obj.get_field(fid);
		     var val = doc[fid];
		     //var link = linker.anchor({id: val}, 'term');
		     var link = null;
		     if( val ){
			 linker.anchor({id: val});
			 if( link ){ val = link; }
		     }else{
			 val = 'n/a';
		     }
		     tbl.add_to([field.display_name(), val]);
		     //tbl.add_to(['link', linker.anchor({id: doc['id']})]);
		 });
	    txt = tbl.to_string();
	}

	// Create div.
	var div = new bbop.html.tag('div', {'generate_id': true});
	var div_id = div.get_id();

	// Append div to body.
	jQuery('body').append(div.to_string());

	// Add text to div.
	jQuery('#' + div_id).append(txt);

	// Modal dialogify div; include self-destruct.
	var dia = jQuery('#' + div_id).dialog({
						  modal: true,
						  draggable: false,
						  width: 700
					      });
	dia.close(function(){ jQuery('#' + div_id).remove(); });
    }

    // BUG/TODO:
    if( bbop.core.what_is(item) == 'string' ){
	// BUG/TODO: Do callback for data.
    }else{
	// It looks like a json data blob.
	// var resp = bbop.golr.response(item);
	// var doc = resp.get_doc(0);
	// draw_shield(doc);
	draw_shield(item);
    }
    
};
//bbop.core.extend(bbop.widget.term_shield, bbop.golr.manager.jquery);
