/*
 * Package: golr_widget_browse.js
 * 
 * Namespace: bbop.golr.manager.widget.browse
 * 
 * BBOP object to draw various UI elements that have to do with
 * autocompletion.
 * 
 * This is a completely self-contained UI and manager.
 */

bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'logger');
bbop.core.require('bbop', 'html');
bbop.core.require('bbop', 'golr', 'manager', 'jquery');
bbop.core.namespace('bbop', 'golr', 'manager', 'widget', 'browse');

/*
 * Constructor: browse
 * 
 * Contructor for the bbop.golr.manager.widget.browse object.
 * 
 * Yes, that is very long.
 * 
 * This is a specialized (and widgetized) subclass of
 * <bbop.golr.manager.jquery>.
 * 
 * The functions for the callbacks look like function(term_acc,
 * term_doc){}. If no function is given, an empty function is used.
 * 
 * Arguments:
 *  golr_loc - string url to GOlr server;
 *  golr_conf_obj - a <bbop.golr.conf> object
 *  interface_id - string id of the element to build on
 *  button_callback - *[serially optional]* callback for when button is clicked
 * 
 * Returns:
 *  this object
 */
bbop.golr.manager.widget.browse = function(golr_loc, golr_conf_obj,
					   interface_id, button_callback){


    // Per-UI logger.
    var logger = new bbop.logger();
    logger.DEBUG = true;
    function ll(str){ logger.kvetch('W (auto): ' + str); }

    ll(bbop.core.what_is(this));
    bbop.golr.manager.jquery.call(this, golr_loc, golr_conf_obj);
    ll(bbop.core.what_is(this));
    this._is_a = 'bbop.golr.manager.widget.browse';
    ll(bbop.core.what_is(this));

    // 
    var anchor = this;
    var resp = bbop.golr.response;
    var loop = bbop.core.each;
    
    // There should be a string interface_id argument.
    this._interface_id = interface_id;
    this._button_callback = button_callback || function(){};
   
    // The current acc that we are interested in.
    this._current_acc = null;

    // Successful callbacks call draw_rich_layout.
    anchor.register('search', 'do', draw_rich_layout);

    // Recursively draw a rich layout using nested uls.
    function draw_rich_layout(json_data){
	
	///
	/// Get the rich layout from the returned document if
	/// possible.
	///
	var doc = resp.documents(json_data)[0];
	var topo_graph = new bbop.model.graph.bracket();
	topo_graph.read_json(doc['topology_graph']);
	var trans_graph = new bbop.model.graph();
	topo_graph.read_json(doc['transitivity_graph']);
	var rich_layout = topo_graph.rich_bracket_layout(anchor._current_acc,
							 trans_graph);

	///
	/// Next, produce the raw HTML skeleton.
	/// TODO: Keep a cache of the interesting ids for adding
	/// events later.
	///

	// I guess we'll just start by making the list.
	var top_level = new bbop.html.list();

	// Cycle down through the brackets, adding spaces every time
	// we go down another level.
	var spaces = '&nbsp;&nbsp;&nbsp;';
	loop(rich_layout,
	     function(layout_level){
		 loop(layout_level,
		      function(level_item){			  
			  // ...
			  top_level.add_to(spaces,
					   level_item[2],
					   level_item[0],
					   level_item[1]);
		      }); 
		 spaces = spaces + '&nbsp;&nbsp;&nbsp;';
	     }); 

	// Add the skeleton to the doc.
	jQuery('#' + anchor._interface_id).empty();
	jQuery('#' + anchor._interface_id).append(top_level.to_string());

	///
	/// TODO:
	/// Finally, attach any events to the browser HTML doc.
	///
    }
	
    // // The all-important argument hash. See:
    // // http://jqueryui.com/demos/autocomplete/#method-widget
    // var auto_args = {
    // 	minLength: 3, // wait for three characters or more
    // 	// Function for a successful data hit.
    // 	// The data getter, which is making it all more complicated
    // 	// than it needs to be...we need to close around those
    // 	// callback hooks so we have to do it inplace here.
    // 	source: function(request_data, response_hook) {
    // 	    anchor.jq_vars['success'] = function(json_data){
    // 		var retlist = [];
    // 		if( resp.success(json_data) ){
    // 		    loop(resp.documents(json_data),
    // 			 function(doc){
    // 			     var lbl = null;
    // 			     if( anchor._id_p ){
    // 				 lbl = doc['id'];
    // 			     }else{
    // 				 lbl = doc['label'];
    // 			     }
    // 			     var item = {
    // 				 'label': doc['label'],
    // 				 'value': lbl,
    // 				 'document': doc
    // 			     };
    // 			     retlist.push(item);
    // 			 });
    // 		}
    // 		response_hook(retlist);
    // 	    };

    // 	    // Get the selected term into the manager and fire.
    // 	    anchor.set_query(request_data.term);
    // 	    anchor.JQ.ajax(anchor.get_query_url(), anchor.jq_vars);
    // 	},
    // 	// What to do when an element is selected.
    // 	select: function(event, ui){
    // 	    var doc_to_apply = null;
    // 	    if( ui.item ){
    // 		doc_to_apply = ui.item.document;
    // 	    }

    // 	    // Only do the callback if it is defined.
    // 	    if( bbop.core.is_defined(anchor._list_select_callback) ){
    // 		anchor._list_select_callback(doc_to_apply);
    // 	    }
    // 	}
    // };
};

    /*
     * Function: draw_browser
     * 
     * Bootstraps the process.
     * 
     * Parameters:
     *  term_acc - acc of term we want to have as the term of interest
     * 
     * Returns
     *  n/a
     */
//    this.draw_browser = function(term_acc){
bbop.golr.manager.widget.browse.prototype.draw_browser = function(term_acc){
    this._current_acc = term_acc;
    this.set_id(term_acc);
    this.update('search');
};

