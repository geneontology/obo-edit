/*
 * Package: golr_widget_autocomplete.js
 * 
 * Namespace: bbop.golr.manager.widget.autocomplete
 * 
 * BBOP object to draw various UI elements that have to do with
 * autocompletion.
 * 
 * This is a completely self-contained UI and manager.
 */

bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'logger');
bbop.core.require('bbop', 'golr', 'manager');
bbop.core.namespace('bbop', 'golr', 'manager','widget', 'autocomplete');

/*
 * Constructor: autocomplete
 * 
 * Contructor for the bbop.golr.manager.widget.autocomplete object.
 * 
 * Yes, that is very long.
 * 
 * This is a specialized (and widgetized) subclass of <bbop.golr.manager>.
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
 *  select_callback - *[serially optional]* function takes a jsonized solr doc
 * 
 * Returns:
 *  this object
 */
bbop.golr.manager.widget.autocomplete = function(golr_loc, golr_conf_obj,
						 interface_id, output_type,
						 select_callback){
    bbop.golr.manager.call(this, golr_loc, golr_conf_obj);
    this._is_a = 'bbop.golr.manager.widget.autocomplete';

    // 
    var anchor = this;
    var resp = bbop.golr.response;
    var loop = bbop.core.each;
    
    // Per-UI logger.
    var logger = new bbop.logger();
    logger.DEBUG = true;
    function ll(str){ logger.kvetch('W (auto): ' + str); }

    // There should be a string interface_id argument.
    this._interface_id = interface_id;
    this._select_callback = select_callback;
   
    // Select whether we want the id or label to be displayed.
    this._id_p = true;
    if( output_type == 'label' ){
	this._id_p = false;
    }

    // The all-important argument hash. See:
    // http://jqueryui.com/demos/autocomplete/#method-widget
    var auto_args = {
	minLength: 3,
	// TODO/BUG: The getter which is making it all more
	// complicated than it needs to be...
	source: function(request, response) {
	    // Get the current state into the manager.
	    anchor.set_query(request.term);
	    anchor.JQ.ajax({
			       url: anchor.get_query_url(),
			       type: "GET",
			       dataType: 'jsonp',
			       jsonp: 'json.wrf',
			       // Function for successful hit.
			       success: function(json_data){
				   var retlist = [];
				   if( resp.success(json_data) ){
				       loop(resp.documents(json_data),
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
				   response(retlist);
			       }
			   });
	},
	// What to do when an element is selected.
	select: function(event, ui){
	    var doc_to_apply = null;
	    if( ui.item ){
		doc_to_apply = ui.item.document;
	    }

	    // Only do the callback if it is defined.
	    if( bbop.core.is_defined(anchor._select_callback) ){
		anchor._select_callback(doc_to_apply);
	    }
	    // var msg = ui.item ?
	    // 	"Selected: " + ui.item.label + ui.item.document.id :
	    // 	"Nothing selected, input was " + this.value;
	    // alert(msg);
	}
	// // ???
	// open: function() {
	//     jQuery(this).removeClass("ui-corner-all").addClass("ui-corner-top");
	// },
	// // ???
	// close: function() {
	//     jQuery(this).removeClass("ui-corner-top").addClass("ui-corner-all");
	// }
    };

    /*
     * Function: make_active
     *
     * Makes the autocomplete active. This must be done after
     * everything has been loaded (e.g. onload or whatever for what
     * you're using) or else serious weirdness happens.
     *
     * Again, since this is adding jQuery listeners to a finished
     * document--it must be done "late".
     * 
     * Parameters: 
     *  n/a
     *
     * Returns:
     *  n/a
     */
    this.make_active = function(){
	jQuery('#' + anchor._interface_id).autocomplete(auto_args);
    };
};
//bbop.golr.manager.widget.autocomplete.prototype = new bbop.golr.manager;
