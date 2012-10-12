/*
 * Package: autocomplete.js
 * 
 * Namespace: bbop.widget.autocomplete
 * 
 * BBOP object to draw various UI elements that have to do with
 * autocompletion.
 * 
 * This is a completely self-contained UI and manager.
 */

bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'logger');
bbop.core.require('bbop', 'golr', 'manager', 'jquery');
bbop.core.namespace('bbop', 'widget', 'autocomplete');

/*
 * Constructor: autocomplete
 * 
 * Contructor for the bbop.widget.autocomplete object.
 * 
 * This is a specialized (and widgetized) subclass of
 * <bbop.golr.manager.jquery>.
 * 
 * The function for the callback argument should either accept a
 * JSONized solr document representing the selected item or null
 * (nothing found).
 * 
 * Arguments:
 *  golr_loc - string url to GOlr server;
 *  golr_conf_obj - a <bbop.golr.conf> object
 *  interface_id - string id of the element to build on
 *  output_type - *[optional]* thing to put in the input box: 'id' or 'label'
 *  list_select_callback - *[serially optional]* function takes a json solr doc
 * 
 * Returns:
 *  this object
 */
bbop.widget.autocomplete = function(golr_loc, golr_conf_obj,
				    interface_id, output_type,
				    list_select_callback){
    bbop.golr.manager.jquery.call(this, golr_loc, golr_conf_obj);
    this._is_a = 'bbop.widget.autocomplete';

    // 
    var anchor = this;
    var loop = bbop.core.each;
    
    // Per-UI logger.
    var logger = new bbop.logger();
    logger.DEBUG = true;
    function ll(str){ logger.kvetch('W (auto): ' + str); }

    // There should be a string interface_id argument.
    this._interface_id = interface_id;
    this._list_select_callback = list_select_callback;
   
    // Select whether we want the id or label to be displayed.
    this._id_p = true;
    if( output_type == 'label' ){
	this._id_p = false;
    }

    // The all-important argument hash. See:
    // http://jqueryui.com/demos/autocomplete/#method-widget
    var auto_args = {
	minLength: 3, // wait for three characters or more
	// Function for a successful data hit.
	// The data getter, which is making it all more complicated
	// than it needs to be...we need to close around those
	// callback hooks so we have to do it inplace here.
	source: function(request_data, response_hook) {
	    anchor.jq_vars['success'] = function(json_data){
		var retlist = [];
		var resp = new bbop.golr.response(json_data);
		if( resp.success() ){
		    loop(resp.documents(),
			 function(doc){
			     var lbl = null;
			     if( anchor._id_p ){
				 lbl = doc['id'];
			     }else{
				 lbl = doc['label'];
			     }
			     var item = {
				 'label': doc['label'],
				 'value': lbl,
				 'document': doc
			     };
			     retlist.push(item);
			 });
		}
		response_hook(retlist);
	    };

	    // Get the selected term into the manager and fire.
	    //anchor.set_query(request_data.term);
	    anchor.set_comfy_query(request_data.term);
	    anchor.JQ.ajax(anchor.get_query_url(), anchor.jq_vars);
	},
	// What to do when an element is selected.
	select: function(event, ui){
	    var doc_to_apply = null;
	    if( ui.item ){
		doc_to_apply = ui.item.document;
	    }

	    // Only do the callback if it is defined.
	    if( bbop.core.is_defined(anchor._list_select_callback) ){
		anchor._list_select_callback(doc_to_apply);
	    }
	}
    };

    // Set the ball rolling (attach jQuery autocomplete to doc).
    jQuery('#' + anchor._interface_id).autocomplete(auto_args);
};
bbop.core.extend(bbop.widget.autocomplete, bbop.golr.manager.jquery);
