/*
 * Package: browse.js
 * 
 * Namespace: bbop.widget.browse
 * 
 * BBOP object to draw various UI elements that have to do with
 * autocompletion.
 * 
 * This is a completely self-contained UI and manager.
 */

bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'logger');
bbop.core.require('bbop', 'model');
bbop.core.require('bbop', 'model', 'bracket', 'graph');
bbop.core.require('bbop', 'html');
bbop.core.require('bbop', 'golr', 'manager', 'jquery');
bbop.core.namespace('bbop', 'widget', 'browse');

/*
 * Constructor: browse
 * 
 * Contructor for the bbop.widget.browse object.
 * 
 * This is a specialized (and widgetized) subclass of
 * <bbop.golr.manager.jquery>.
 * 
 * The functions for the callbacks look like function(term_acc,
 * json_data){}. If no function is given, an empty function is used.
 * 
 * Arguments:
 *  golr_loc - string url to GOlr server;
 *  golr_conf_obj - a <bbop.golr.conf> object
 *  interface_id - string id of the element to build on
 *  info_button_callback - *[serially optional]* callback for when button is clicked
 * 
 * Returns:
 *  this object
 */
bbop.widget.browse = function(golr_loc, golr_conf_obj,
			      interface_id, info_button_callback){

    // Per-UI logger.
    var logger = new bbop.logger();
    logger.DEBUG = true;
    function ll(str){ logger.kvetch('W (browse): ' + str); }

    //ll(bbop.core.what_is(this));
    bbop.golr.manager.jquery.call(this, golr_loc, golr_conf_obj);
    //ll(bbop.core.what_is(this));
    this._is_a = 'bbop.widget.browse';
    //ll(bbop.core.what_is(this));

    // 
    var anchor = this;
    var loop = bbop.core.each;
    
    // There should be a string interface_id argument.
    this._interface_id = interface_id;
    this._info_button_callback = info_button_callback || function(){};
   
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
	var resp = new bbop.golr.response(json_data);
	var doc = resp.documents(json_data)[0];

	var topo_graph = new bbop.model.bracket.graph();
	topo_graph.load_json(JSON.parse(doc['topology_graph']));

	var trans_graph = new bbop.model.graph();
	trans_graph.load_json(JSON.parse(doc['transitivity_graph']));

	//ll('to: ' + doc['topology_graph']);
	//ll('tr: ' + doc['transitivity_graph']);
	//ll('ro: ' + anchor._current_acc);
	//ll('g: ' + topo_graph.get_parent_nodes(anchor._current_acc));
	var rich_layout = topo_graph.rich_bracket_layout(anchor._current_acc,
							 trans_graph);
	//ll("rl: " + bbop.core.dump(rich_layout));

	///
	/// Next, produce the raw HTML skeleton.
	/// TODO: Keep a cache of the interesting ids for adding
	/// events later.
	///

	// I guess we'll just start by making the list.
	var top_level = new bbop.html.list();

	// Store the navigation anf info buttons.
	var nav_button_hash = {};
	var info_button_hash = {};

	// Cycle down through the brackets, adding spaces every time
	// we go down another level.
	var spacing = '&nbsp;&nbsp;&nbsp;&nbsp;';
	var spaces = spacing;
	loop(rich_layout,
	     function(layout_level){
		 loop(layout_level,
		      function(level_item){			  

			  var nid = level_item[0];
			  var rel = level_item[2];

			  //Clickable acc span.
			  var nav_b =
			      new bbop.html.span('<a>[' + nid + ']</a>',
						 {'generate_id': true});
			  nav_button_hash[nav_b.get_id()] = nid;

			  //Clickable info span.
			  var info_b =
			      new bbop.html.span('<b>[i]</b>',
						 {'generate_id': true});
			  info_button_hash[info_b.get_id()] = nid;

			  // "Icon"
			  var icon = '[????]';
			  if(anchor._current_acc == nid){
			      icon = '[[[]]]';
			  }else{
			      if( rel == 'is_a' ){
				  icon = '[is a]';   
			      }else if( rel == 'part_of' ){
				  icon = '[part]';   
			      }else if( rel == 'regulates' ){
				  icon = '[ reg]';   
			      }else if( rel == 'negatively_regulates' ){
				  icon = '[-reg]';   
			      }else if( rel == 'positively_regulates' ){
				  icon = '[+reg]';   
			      }else if( rel == 'develops_from' ){
				  icon = '[devf]';   
			      }
			  }

			  // ...
			  top_level.add_to(spaces,
					   icon,
					   nav_b.to_string(),
					   level_item[1],
					   info_b.to_string());
		      }); 
		 spaces = spaces + spacing;
	     }); 

	// Add the skeleton to the doc.
	jQuery('#' + anchor._interface_id).empty();
	jQuery('#' + anchor._interface_id).append(top_level.to_string());

	///
	/// Finally, attach any events to the browser HTML doc.
	///

	// Navigation.
	loop(nav_button_hash,
	     function(button_id, node_id){

		 jQuery('#' + button_id).click(
		     function(){
			 var tid = jQuery(this).attr('id');
			 var call_time_node_id = nav_button_hash[tid];
			 //alert(call_time_node_id);
			 anchor.draw_browser(call_time_node_id);
		     });
	     });

	// Information.
	loop(info_button_hash,
	     function(button_id, node_id){

		 jQuery('#' + button_id).click(
		     function(){
			 var tid = jQuery(this).attr('id');
			 var call_time_node_id = info_button_hash[tid];
			 anchor._info_button_callback(call_time_node_id,
						      //json_data);
						      doc);
		     });
	     });
    }
	
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
    //bbop.widget.browse.prototype.draw_browser = function(term_acc){
    // this._current_acc = term_acc;
    // this.set_id(term_acc);
    // this.update('search');
    this.draw_browser = function(term_acc){
	anchor._current_acc = term_acc;
	anchor.set_id(term_acc);
	anchor.update('search');
    };
    
};
bbop.core.extend(bbop.widget.browse, bbop.golr.manager.jquery);
