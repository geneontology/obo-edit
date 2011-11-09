////////////
////
//// bbop.render.phylo
////
//// Purpose: Extend the model to be handy for a (phylo)tree.
//// 
//// Width is determined by used div width (style).
//// 
//// Taken name spaces:
////    bbop.render.phylo.*
////
//// TODO: better selection of displayable text
//// TODO: get parser so we can start really checking/use.
//// TODO: make things non-interactive during visible == false?
//// TODO: font and text placement
//// TODO: better text alignment
//// TODO: floating right-hand text (see PAINT)
//// TODO: some "speed-up" refactoring?
////
//// OKAY: FF, Safari, Chrome, Opera
//// TODO: IE a little wonky, but not too bad--easy fix?
////
//// Required:
////    Rafael
////    bbop.core
////    bbop.model
////    bbop.model.tree
////
//////////


// Module and namespace checking.
bbop.core.require('Raphael');
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'model');
bbop.core.require('bbop', 'model', 'tree');
bbop.core.namespace('bbop', 'render', 'phylo');

///
/// PNodes (phylonode) object.
///

// Render out.
// Actually, use this to wrap graph abstraction.
bbop.render.phylo.renderer = function (element_id, info_box_p){

    // Logger.
    var logger = new bbop.logger();
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    var elt_id = element_id;

    var renderer_context = this;

    // Properties that can be manipulated externally.
    //this.animation_time = 100;
    this.animation_time = 200;
    //this.animation_time = 1000; // for debug
    //this.use_animation = true;
    this.use_animation = false;

    // These first two defaults will be overwritten on display.
    this.box_width = 60;
    this.box_height = 30;

    // Internal-only variables.
    this._render_frame_width = 800;
    this._render_interal_width = this._render_frame_width;
    this._render_frame_height = 600;
    this._render_internal_height = this._render_height;
    //this._node_labels = {};
    //this._node_hover_labels = {};
    //this._edge_labels = {};
    //this._floating_labels = {};

    ///
    /// Functions to handle internal graph management.
    ///
    
    var node_cache_hash = {};
    this._graph = new bbop.model.tree.graph();

    //
    this.add_node = function(unique_id){
	var new_node = new bbop.model.tree.node(unique_id, unique_id);
	node_cache_hash[unique_id] = new_node;
	this._graph.add_node(new_node);
    };	

    //
    this.add_edge = function(nid1, nid2, dist){

	var retval = false;

	var n1 = node_cache_hash[nid1];
	var n2 = node_cache_hash[nid2];
	if( n1 && n2 ){
	    var new_edge = new bbop.model.tree.edge(n1, n2, dist);
	    this._graph.add_edge(new_edge);
	    retval = true;	    
	}

	return retval;
    };

    ///
    /// ...
    ///

    // Init: context, label, x-coord, y-coord.
    graph_pnode = function(context, label, px, py, internal_p){

	var pnode_box_width = renderer_context.box_width;
	var pnode_box_height = renderer_context.box_height;

	// Color and size definitions.
	var text_offset_x = pnode_box_width / 2.0;
	var text_offset_y = pnode_box_height / 2.0;
	this.base_node_color = "#00f";

	// Variations if an internal node.
	if( internal_p ){
	    pnode_box_width = pnode_box_width / 2.0;
	    pnode_box_width = 2.0;
	    //pnode_box_height = 2.0;
	    text_offset_x = (pnode_box_width / 2.0);
	}

	// Future visibility.    
	this.visible = true;
	
	// For advanced tree use.
	this.open = true;
	
	// Coloration and style attributes.
	this.shape_base_attr = {
	    "fill": this.base_node_color,
	    "fill-opacity": 0.05,
	    "opacity": 1.0,
	    "stroke": this.base_node_color,
	    "stroke-width": 2,
	    "title": "This is " + label,
	    "cursor": "move"
	};
	this.shape_highlight_attr = {
	    "fill": this.base_node_color,
	    "fill-opacity": 0.5,
	    "opacity": 1.0,
	    "stroke": this.base_node_color,
	    "stroke-width": 3
	};
	this.shape_dim_attr = {
	    "fill": this.base_node_color,
	    "fill-opacity": 0.0,
	    "opacity": 0.5,
	    "stroke": this.base_node_color,
	    "stroke-width": 1
	};
	this.shape_invisible_attr = {
	    "fill": "#000",
	    "fill-opacity": 0.0,
	    "opacity": 0.0,
	    "stroke": "#000",
	    "stroke-width": 0
	};
	// Text in node.
	this.text_base_attr = {
	    //"fill-opacity": 1.0,
	    "opacity" : 1.0,
	    "font-size" : 10
	};
	//this.text_highlight_attr = {"fill-opacity": 1.0, "font-size" : 12};
	this.text_highlight_attr = {
	    //"fill-opacity": 1.0,
	    "opacity" : 1.0,
	    "font-size" : 10
	};
	this.text_dim_attr = {
	    //"fill-opacity": 0.2,
	    "opacity" : 0.2,
	    "font-size" : 10
	};
	this.text_invisible_attr = {
	    //"fill-opacity": 0.0,
	    "opacity" : 0.0,
	    "font-size" : 10
	};

	// Draw out initial node.
	this._context = context;

	this._text = // NOTE: text is *centered* at this point.
	this._context.text(px + text_offset_x, py + text_offset_y, label);
	this._text.toBack(); // make sure it's behind the boxes
	this._shape = this._context.rect(px, py,
					 pnode_box_width, pnode_box_height,
					 2);

	// Proxy properties and functions.
	// This is so wrong, but feels so good...proxy most things through
	// shape.
	this.id = this._shape.id; // Use the shape's UID as our UID.
	this.getBBox = function(){
	    return this._shape.getBBox.call(this._shape);
	};
	// Semi-proxy.
	this.shape_attr = function(arg){
	    return this._shape.attr.call(this._shape, arg);
	};

	// Add to the object the initial position.
	this._start_shape_y = this._shape.attr("y");
	this._start_text_y = this._text.attr("y");

	// Setup shape attributes.
	this._shape.attr(this.shape_base_attr);
    };
    // Call first when you want to move.
    graph_pnode.prototype.update_position = function(){
	this._start_shape_y = this._shape.attr("y");
	this._start_text_y = this._text.attr("y");
    };
    // Move.
    graph_pnode.prototype.move_y = function(arg){
	var d_shape = this._start_shape_y + arg;
	var d_text = this._start_text_y + arg;
	this._shape.attr.call(this._shape, {"y": d_shape});
	this._text.attr.call(this._text, {"y": d_text});
    };
    // Event handler proxies for underlying shapes (text ignored).
    graph_pnode.prototype.drag = function(mv_func,start_func,end_func){
	this._shape.drag(mv_func, start_func, end_func);
    };
    graph_pnode.prototype.dblclick = function(handler){
	this._shape.dblclick.call(this._shape, handler);
    };
    graph_pnode.prototype.mouseover = function(handler){
	this._shape.mouseover.call(this._shape, handler);
    };
    graph_pnode.prototype.mouseout = function(handler){
	this._shape.mouseout.call(this._shape, handler);
    };

    graph_pnode.prototype.update = function(message){

	//
	var shape_attr_to_apply = this.shape_base_attr;
	var text_attr_to_apply = this.text_base_attr;

	// 
	if( this.visible == false ){
	    shape_attr_to_apply = this.shape_invisible_attr;
	    text_attr_to_apply = this.text_invisible_attr;
	}else if( message == 'highlight' ){
	    shape_attr_to_apply = this.shape_highlight_attr;
	    text_attr_to_apply = this.text_highlight_attr;
	}else if( message == 'dim' ){
	    shape_attr_to_apply = this.shape_dim_attr;
	    text_attr_to_apply = this.text_dim_attr;
	}

	// Change border on whether or not it's "opened".
	if( this.open == false ){
	    shape_attr_to_apply['stroke'] = "#070";
	}else{
    	    shape_attr_to_apply['stroke'] = this.base_node_color;	
	}

	// Render with whatever filtered through.
	if( renderer_context.use_animation ){
	    this._shape.animate.call(this._shape,
				     shape_attr_to_apply,
				     renderer_context.animation_time);
	    this._shape.animate.call(this._text,
				     text_attr_to_apply,
				     renderer_context.animation_time);	
	}else{
	    this._shape.attr(shape_attr_to_apply);
	    this._text.attr(text_attr_to_apply);
	}
    };


    ///
    /// Define the edges (connections) to be used for drawing.
    /// Connection (between two pnodes) object.
    ///

    // Init: context, shape, shape, and "distance" representation
    // (optional).
    graph_connection = function(context, obj1, obj2, dist_rep){

	//this.context = context;

	// These need to be set right away for path calculation.    
	this.from = obj1;
	this.to = obj2;

	this.id = this.from.id + '_id_invariant_' + this.to.id;

	// Get path.
	var path_info = this.get_path_between_info();
	var path = path_info['path'];
	var cp = path_info['center_point'];

	// ll("conn: cp: (" + cp[0] + ", " + cp[1] + ")");

	// Future visibility.
	this.visible = true;

	var base_edge_color = "#030";
	var base_edge_width = "3";
	var highlight_edge_color = "#00f";
	var highlight_edge_width = "5";
	var invisible_edge_color = "#000";
	var invisible_edge_width = "0";

	this.edge_base_attr = {
	    "stroke": base_edge_color,
     	    "stroke-width": base_edge_width,
	    "fill": "none",
	    "opacity": 1.0,
	    "fill-opacity": 0.0
	};
	this.edge_highlight_attr = {
	    "stroke": highlight_edge_color,
     	    "stroke-width": highlight_edge_width,
	    "fill": "none",
	    "opacity": 1.0,
	    "fill-opacity": 0.0
	};
	this.edge_dim_attr = {
	    "stroke": base_edge_color,
     	    "stroke-width": 1,
	    "fill": "none",
	    "opacity": 0.5,
	    "fill-opacity": 0.0
	};
	this.edge_invisible_attr = {
	    "stroke": invisible_edge_color,
     	    "stroke-width": invisible_edge_width,
	    "fill": "none",
	    "opacity": 0.0,
	    "fill-opacity": 0.0
	};
	// // As connections.
	// this.text_base_attr = {"fill-opacity": 1.0, "font-size" : 10};
	// this.text_highlight_attr = {"fill-opacity": 1.0, "font-size" : 10};
	// this.text_dim_attr = {"fill-opacity": 0.2, "font-size" : 10};
	// this.text_invisible_attr = {"fill-opacity": 0.0, "font-size" : 10};
	// Highlight-only.
	this.text_base_attr = {
	    "opacity": 0.0,
	    "font-size" : 10
	};
	//this.text_highlight_attr = {"fill-opacity": 1.0, "font-size" : 12};
	this.text_highlight_attr = {
	    "opacity": 1.0,
	    "font-size" : 10
	};
	this.text_dim_attr = {
	    "opacity": 0.0,
	    "font-size" : 10
	};
	this.text_invisible_attr = {
	    "opacity": 0.0,
	    "font-size" : 10
	};

	// Build up text at path centerpoint.
	this.text = null;
	if( dist_rep ){
	    this.text = context.text(cp[0], (cp[1] + 10), dist_rep);
	    this.text.toBack(); // make sure it's behind the boxes
	    this.text.attr(this.text_base_attr);	
	}

	// Colors and lines.
	this.line = context.path(path);
	this.line.attr(this.edge_base_attr);
    };
    // Update line graphic.
    graph_connection.prototype.update = function(message){

	// Get path.
	var path_info = this.get_path_between_info();
	var path = path_info['path'];

	// Update line position.
	this.line.attr({path: path});

	// Update line graphics on message.
	var line_attr_to_apply = null;
	if( this.visible == false ){
	    line_attr_to_apply = this.edge_invisible_attr;
	}else if( message == 'highlight' ){
	    line_attr_to_apply = this.edge_highlight_attr;
	}else if( message == 'dim' ){
	    line_attr_to_apply = this.edge_dim_attr;
	}else{
	    line_attr_to_apply = this.edge_base_attr;
	}

	// Render with whatever filtered through.
	if( renderer_context.use_animation ){	
	    this.line.animate.call(this.line,
				   line_attr_to_apply,
				   renderer_context.animation_time);
	}else{
	    this.line.attr(line_attr_to_apply);
	}

	// Update text position.
	var text_attr_to_apply = null;
	if( this.text ){
	    var cp = path_info['center_point'];
	    this.text.attr({"x": cp[0], "y": (cp[1] + 10)});

	    // Update graphics graphics on message.
	    if( this.visible == false ){
		text_attr_to_apply = this.text_invisible_attr;
	    }else if( message == 'highlight' ){
		text_attr_to_apply = this.text_highlight_attr;
	    }else if( message == 'dim' ){
		text_attr_to_apply = this.text_dim_attr;
	    }else{
		text_attr_to_apply = this.text_base_attr;
	    }

	    // Render with whatever filtered through.
	    if( renderer_context.use_animation ){	
		this.text.animate.call(this.text,
				       text_attr_to_apply,
				       renderer_context.animation_time);
	    }else{
		this.text.attr(text_attr_to_apply);
	    }
	}
    };
    // // Generate path from between the two internally stored objects.
    // graph_connection.prototype.get_path_between_info = function(){

    // 	var bb1 = this.from.getBBox();
    // 	var bb2 = this.to.getBBox();

    // 	//ll("bb1.width: " + bb1.width);
    // 	//ll("bb1.x: " + bb1.x + ", bb1.y: " + bb1.y);
    // 	//ll("bb1.width: "+ bb1.width +", bb1.height: "+ bb1.height);

    // 	var p = [{x: bb1.x + bb1.width / 2, y: bb1.y - 1},
    // 		 {x: bb1.x + bb1.width / 2, y: bb1.y + bb1.height + 1},
    // 		 {x: bb1.x - 1, y: bb1.y + bb1.height / 2},
    // 		 {x: bb1.x + bb1.width + 1, y: bb1.y + bb1.height / 2},
    // 		 {x: bb2.x + bb2.width / 2, y: bb2.y - 1},
    // 		 {x: bb2.x + bb2.width / 2, y: bb2.y + bb2.height + 1},
    // 		 {x: bb2.x - 1, y: bb2.y + bb2.height / 2},
    // 		 {x: bb2.x + bb2.width + 1, y: bb2.y + bb2.height / 2}];
    // 	var d = {};
    // 	var dis = [];
    // 	for (var i = 0; i < 4; i++) {
    //         for (var j = 4; j < 8; j++) {
    // 		var dx = Math.abs(p[i].x - p[j].x);
    // 		var dy = Math.abs(p[i].y - p[j].y);
    // 		if ((i == j - 4) ||
    // 		    (((i != 3 && j != 6) || p[i].x < p[j].x) &&
    // 		     ((i != 2 && j != 7) || p[i].x > p[j].x) &&
    // 		     ((i != 0 && j != 5) || p[i].y > p[j].y) &&
    // 		     ((i != 1 && j != 4) || p[i].y < p[j].y))) {
    //                 dis.push(dx + dy);
    //                 d[dis[dis.length - 1]] = [i, j];
    // 		}
    //         }
    // 	}
    // 	var res = null;
    // 	if (dis.length == 0) {
    //         res = [0, 4];
    // 	}else{
    //         res = d[Math.min.apply(Math, dis)];
    // 	}
    // 	var x1 = p[res[0]].x;
    // 	var y1 = p[res[0]].y;
    // 	var x2 = p[res[1]].x;
    // 	var y2 = p[res[1]].y;
    // 	var dx = Math.max(Math.abs(x1 - x2) / 2, 10);
    // 	var dy = Math.max(Math.abs(y1 - y2) / 2, 10);
    // 	return {"path": [
    // 		    "M", x1.toFixed(3), y1.toFixed(3),
    // 		    "L", x1.toFixed(3), y2.toFixed(3),
    // 		    "L", x2.toFixed(3), y2.toFixed(3)
    // 		].join(","),
    // 		// "center_point": [(x1.toFixed(3) + x1.toFixed(3)),
    // 		// 		     (y1.toFixed(3) + y2.toFixed(3))]
    // 		"center_point": [(x1 + x2) / 2.0, (y2)]
    // 	       };
    // };

    // Generate path from between the two internally stored objects.
    graph_connection.prototype.get_path_between_info = function(){

	var bb1 = this.from.getBBox();
	var bb2 = this.to.getBBox();

	//ll("bb1.width: " + bb1.width);
	//ll("bb1.x: " + bb1.x + ", bb1.y: " + bb1.y);
	//ll("bb1.width: "+ bb1.width +", bb1.height: "+ bb1.height);

	var p =
	    [
		// bb1: middle-top
		{x: bb1.x + bb1.width / 2, y: bb1.y - 1},
		// bb1: middle-bottom
		{x: bb1.x + bb1.width / 2, y: bb1.y + bb1.height + 1},
		// bb1: left-middle
		{x: bb1.x - 1, y: bb1.y + bb1.height / 2},
		// bb1: right-middle
		{x: bb1.x + bb1.width + 1, y: bb1.y + bb1.height / 2},
		// bb2: middle-top
		//{x: bb2.x + bb2.width / 2, y: bb2.y - 1},
		{x: bb2.x - 1, y: bb2.y + bb2.height / 2},
		// bb2: middle-bottom
		//{x: bb2.x + bb2.width / 2, y: bb2.y + bb2.height + 1},
		{x: bb2.x - 1, y: bb2.y + bb2.height / 2},
		// bb2: left-middle
		{x: bb2.x - 1, y: bb2.y + bb2.height / 2},
		// bb2: right-middle
		//{x: bb2.x + bb2.width + 1, y: bb2.y + bb2.height / 2}
		{x: bb2.x - 1, y: bb2.y + bb2.height / 2}
	    ];
	var d = {};
	var dis = [];
	for (var i = 0; i < 4; i++) { // for bb1
            for (var j = 4; j < 8; j++) { // for bb2
		var dx = Math.abs(p[i].x - p[j].x);
		var dy = Math.abs(p[i].y - p[j].y);
		if ((i == j - 4) ||
    		    (((i != 3 && j != 6) || p[i].x < p[j].x) &&
    		     ((i != 2 && j != 7) || p[i].x > p[j].x) &&
    		     ((i != 0 && j != 5) || p[i].y > p[j].y) &&
    		     ((i != 1 && j != 4) || p[i].y < p[j].y))) {
                    dis.push(dx + dy);
                    d[dis[dis.length - 1]] = [i, j];
		}
            }
	}
	var res = null;
	if (dis.length == 0) {
            res = [0, 4];
	}else{
            res = d[Math.min.apply(Math, dis)];
	}
	var x1 = p[res[0]].x;
	var y1 = p[res[0]].y;
	var x2 = p[res[1]].x;
	var y2 = p[res[1]].y;
	//var dx = Math.max(Math.abs(x1 - x2) / 2, 10);
	//var dy = Math.max(Math.abs(y1 - y2) / 2, 10);
	return {"path": [
    		    "M", x1.toFixed(3), y1.toFixed(3),
    		    "L", x1.toFixed(3), y2.toFixed(3),
    		    "L", x2.toFixed(3), y2.toFixed(3)
		].join(","),
		// "center_point": [(x1.toFixed(3) + x1.toFixed(3)),
		// 		     (y1.toFixed(3) + y2.toFixed(3))]
		"center_point": [(x1 + x2) / 2.0, (y2)]
	       };
    };

    ///
    /// Functions and sub-functions for display.
    ///

    // TODO: later, allow display to take args to force size.
    this.display = function () {

	var layout = this._graph.layout();
	var elt = document.getElementById(elt_id);

	// Fudge variables.
	var edge_shift = 1.0; // fudge to allow the last little bit on screen
	var absolute_pull = 15.0; // there seems to be a misjudgement
				  // in width by about this much

	// Adjust vertical scales and display.
	var y_scale = renderer_context.box_height * 2.0; // fixed y-scale
	this._render_frame_height = (layout.max_width * y_scale);
	this._render_internal_height = this._render_frame_height - edge_shift;

	// Adjust for render width based on platform.
	// TODO: later, allow display to take args to force size.
	var x_scale = 1.0;
	//if( window && window.innerWidth && 1 == 2){
	    //this._render_frame_width = window.innerWidth;
	//}else 
	if( elt.clientWidth ){
	    this._render_frame_width = elt.clientWidth;
	}else{
	    ll("UFP: Unidentified Failing Platform.");
	}
	// Now adjust the drawing width to make sure that the boxes
	// fit.
	//this._render_internal_width = this._render_frame_width;
	this._render_internal_width =
	    this._render_frame_width
	    - (1.0 * renderer_context.box_width)
	    - absolute_pull;
	// If we're using the info box, adjust inwards by some amount.
	if( info_box_p ){
	    this._render_internal_width = this._render_internal_width * 0.8;
	}
	// Recalculate x-scale.
	x_scale = this._render_internal_width / layout.max_distance;
	// Get that last pixel column on board.
	ll('internal width: ' + this._render_internal_width);
	ll('frame width: ' + this._render_frame_width);

	// Create context.
	var paper = Raphael(elt_id,
			    this._render_frame_width,
			    this._render_frame_height);
	ll('display: made paper');

	///
	/// Graph helper function definitions.
	/// 

	function get_pnode_from_phynode_id(phynode_id){
	    var ret = null;
	    if( phynode_id_to_index[phynode_id] ){
		ret = phynodes[phynode_id_to_index[phynode_id]];
	    }
	    return ret;
	}

	// Subtree list, including self.
	function gather_list_from_hash(nid, hash){
    	    var retlist = new Array();
    	    retlist.push(nid);
    	    // Get all nodes cribbing from distances.
    	    for( vt in hash[nid] ){
    		//ll("id: " + id + ", v: " + ct);
    		retlist.push(vt);
    	    }
    	    return retlist;	
	}

	// Subtree list, including self.
	function get_descendant_node_list(nid){
	    return gather_list_from_hash(nid, layout.parent_distances);
	}

	// Ancestor list, including self.
	function get_ancestor_node_list(nid){
	    return gather_list_from_hash(nid, layout.child_distances);
	}

	//
	function get_associated(phynode_id, index_kept, getter){

    	    var retlist = new Array();
	    
    	    var node_id = phynode_id_to_node_id[phynode_id];
    	    var subtree_node_list = getter(node_id);
    	    for( var si = 0; si < subtree_node_list.length; si++ ){

    		var subnode_id = subtree_node_list[si];
    		var sindex = node_id_to_index[subnode_id];

    		var thing = index_kept[sindex];
    		retlist.push(thing);
    	    }

    	    return retlist;
	}

	function get_descendant_phynodes(phynode_id){
    	    return get_associated(phynode_id, phynodes, get_descendant_node_list);
	}

	function get_descendant_texts(phynode_id){
    	    return get_associated(phynode_id, texts, get_descendant_node_list);
	}

	function get_ancestor_phynodes(phynode_id){
    	    return get_associated(phynode_id, phynodes, get_ancestor_node_list);
	}

	// General func.
	function get_connections(phynode_id, phynode_getter, conn_hash){

	    var retlist = new Array();

	    // Fish in the connection ancestor hash for edges.
	    var tmp_phynodes = phynode_getter(phynode_id);
	    for( var si = 0; si < tmp_phynodes.length; si++ ){
		var tshp = tmp_phynodes[si];
		var tnid = phynode_id_to_node_id[tshp.id];
		if( tnid && conn_hash[tnid] ){
		    for( var anid in conn_hash[tnid] ){
			var conn_index = conn_hash[tnid][anid];
			var conn = connections[conn_index];
			ll('get_conn: found: [' + conn_index +
					 '] ' + anid + ' <=> ' + tnid +
					 ' ... ' + conn);
			retlist.push(conn);
		    }
		}
	    }
	    return retlist;
	};

	//
	function get_ancestor_connections(phynode_id){
	    return get_connections(phynode_id,
				   get_ancestor_phynodes,
				   conn_hash_ancestor);
	}

	//
	function get_descendant_connections(phynode_id){
	    return get_connections(phynode_id,
				   get_descendant_phynodes,
				   conn_hash_descendant);
	}

	///
	/// Phynode manipulation function definitions.
	/// 

	// Dragging animation (color dimming).
	var start = function () {

    	    var phynode_id = this.id;

	    // Darken boxes and update current position before dragging.
    	    var assoc_phynodes = get_descendant_phynodes(phynode_id);
    	    for( var si = 0; si < assoc_phynodes.length; si++ ){
		var phynode = assoc_phynodes[si];
		phynode.update_position();
		phynode.update("dim");
    	    }

	    // "Dim" edges.
	    var subtree_edges = get_descendant_connections(phynode_id);
	    for( var se = 0; se < subtree_edges.length; se++ ){
		var ste = subtree_edges[se];
		ste.update("dim");
	    }
	};

	// Movement animation (don't allow movement on the x-axis) and
	// redo lines.
	var move = function (dx, dy) {

    	    var phynode_id = this.id;

	    // Move box positions.
    	    var assoc_phynodes = get_descendant_phynodes(phynode_id);
    	    for( var si = 0; si < assoc_phynodes.length; si++ ){
		var mshp = assoc_phynodes[si];
		mshp.move_y(dy);
		//ll('mshp['+si+']:'+' oy: '+mshp.start_y+', dy:'+dy);
    	    }

	    // Collect subtree edges for next bit.
	    var dimmable_subtree = {};
	    var subtree_edges = get_descendant_connections(phynode_id);
	    for( var se = 0; se < subtree_edges.length; se++ ){
		var ste = subtree_edges[se];
		dimmable_subtree[ste.id] = true;
	    }

	    // Update connections; keep subtree dimmed while in transit.
            for (var i = connections.length; i--;) {
		var conn = connections[i];
		if( dimmable_subtree[conn.id] ){
		    conn.update('dim');		
		}else{
		    conn.update();		
		}
            }
            paper.safari();
	};

	// Undrag animation.
	var stop = function () {

    	    var phynode_id = this.id;

	    // Fade boxes.
    	    var assoc_phynodes = get_descendant_phynodes(phynode_id);
    	    for( var si = 0; si < assoc_phynodes.length; si++ ){
		var mshp = assoc_phynodes[si];
		mshp.update();
    	    }

	    // Update connections; bring them all back to normal.
            for (var i = connections.length; i--;) {
		connections[i].update();		
            }
            paper.safari();
	};

	// Experiment with double click.
	function dblclick_event_handler(event){

	    var phynode_id = this.id;

	    // If this is the first double click here...
	    var pn = get_pnode_from_phynode_id(phynode_id);
	    if( pn.open == true ){
		
		// "Vanish" edges.
		var subtree_edges = get_descendant_connections(phynode_id);
		for( var se = 0; se < subtree_edges.length; se++ ){
		    var ste = subtree_edges[se];
		    ste.visible = false;
		    ste.update();
		}

		// "Vanish" nodes and text; not this node though...
		var subtree_nodes = get_descendant_phynodes(phynode_id);
		for( var sn = 0; sn < subtree_nodes.length; sn++ ){
		    var stn = subtree_nodes[sn];
		    if( stn.id != phynode_id ){
			// Turn of visibilty for children.
			stn.visible = false;
		    }else{
			// Mark self as closed.
			stn.open = false;
		    }
		    stn.update();
		}
	    }else{ //Otherwise...
		
		// Reestablish edges.
		var subtree_edges = get_descendant_connections(phynode_id);
		for( var se = 0; se < subtree_edges.length; se++ ){
		    var ste = subtree_edges[se];
		    ste.visible = true;
		    ste.update();
		}

		// Restablish pnodes; clear all history.
		var subtree_nodes = get_descendant_phynodes(phynode_id);
		for( var sn = 0; sn < subtree_nodes.length; sn++ ){
		    var stn = subtree_nodes[sn];
		    stn.open = true;
		    stn.visible = true;
		    stn.update();
		}
	    }
	}

	// Experiment with hover.
	function mouseover_event_handler(event){

    	    var phynode_id = this.id;

	    // Cycle through ancestor phynodes.
    	    var anc_phynodes = get_ancestor_phynodes(phynode_id);
    	    for( var ai = 0; ai < anc_phynodes.length; ai++ ){
		// Change boxes opacity (darken).
		var ashp = anc_phynodes[ai];
		ashp.update("highlight");
	    }
	    // Cycle through descendant phynodes.
    	    var desc_phynodes = get_descendant_phynodes(phynode_id);
    	    for( var di = 0; di < desc_phynodes.length; di++ ){
		// Change boxes opacity (darken).
		var dshp = desc_phynodes[di];
		dshp.update("highlight");
	    }

	    // See if we can fish any edges out and highlight them.
    	    var anc_edges = get_ancestor_connections(phynode_id);
    	    for( var ac = 0; ac < anc_edges.length; ac++ ){
		var aconn = anc_edges[ac];
		aconn.update("highlight");
	    }
    	    var desc_edges = get_descendant_connections(phynode_id);
    	    for( var dc = 0; dc < desc_edges.length; dc++ ){
		var dconn = desc_edges[dc];
		dconn.update("highlight");
	    }
	    paper.safari();
	}
	function mouseout_event_handler(event){

    	    var phynode_id = this.id;

	    // Cycle through ancestor phynodes.
    	    var anc_phynodes = get_ancestor_phynodes(phynode_id);
    	    for( var ai = 0; ai < anc_phynodes.length; ai++ ){
		// Change boxes opacity (lighten).
		var ashp = anc_phynodes[ai];
		ashp.update();
    	    }
	    // Cycle through descendant phynodes.
    	    var desc_phynodes = get_descendant_phynodes(phynode_id);
    	    for( var di = 0; di < desc_phynodes.length; di++ ){
		// Change boxes opacity (lighten).
		var dshp = desc_phynodes[di];
		dshp.update();
    	    }

	    // See if we can fish any edges out and unhighlight them.
    	    var anc_edges = get_ancestor_connections(phynode_id);
    	    for( var ac = 0; ac < anc_edges.length; ac++ ){
		var aconn = anc_edges[ac];
		aconn.update();
	    }
    	    var desc_edges = get_descendant_connections(phynode_id);
    	    for( var dc = 0; dc < desc_edges.length; dc++ ){
		var dconn = desc_edges[dc];
		dconn.update();
	    }
	    paper.safari();
	}

	///
	///  Render info box if wanted.
	///

	if( info_box_p ){
	    
	    //var lnodes = this._graph.get_leaf_nodes();
	    // Get the last ordered cohort and build table from that.
	    var lnodes = layout.cohorts[layout.cohorts.length - 1];
	    for( var ln = 0; ln < lnodes.length; ln++ ){	    
		var lnode = lnodes[ln];

		var pr_xa = paper.width - (paper.width * 0.2) + 20; // x-axis
		var pr_ya = 1.0 + (y_scale * ln); // y-axis
		var bw = (paper.width * 0.2) - 30.0; // width
		var bh = y_scale - 1.0; // height
		var pr = paper.rect(pr_xa, pr_ya,
				    bw, bh,
				    1); // roundness
		pr.attr({
			    "fill": "#eeee99",
			    "fill-opacity": 0.5,
			    "opacity": 1.0,
			    "stroke": "#333388",
			    "stroke-width": 1,
			    "title": "This is " + lnode.id
			    //"cursor": "move"
			});

		var pt = paper.text(pr_xa + (bw / 2.0), pr_ya + (bh / 2.0),
				    "Data for " + lnode.id);
	    }
	}

	///
	/// Phynode creation and placement.
	/// 

	// Add phynodes and create lookup (hash) for use with connections.
	var phynodes = new Array();
	var phynode_hash = {};
	var texts = new Array();
	var phynode_id_to_index = {};
	var phynode_id_to_node_id = {};
	var node_id_to_index = {};
	for( var nidi = 0; nidi < layout.node_list.length; nidi++ ){

	    // Calculate position.
	    var node_id = layout.node_list[nidi];
	    var lpx = (layout.position_x[node_id] * x_scale) + edge_shift;
	    var lpy = (layout.position_y[node_id] * y_scale) + edge_shift;

	    // Create node at place. 
	    var phynode = null;
	    if( ! this._graph.is_leaf_node(node_id) && info_box_p ){
		ll('display: internal node: ' + node_id);
		phynode = new graph_pnode(paper, node_id, lpx, lpy, true);
		//phynode.attr("width") = 10;
		//phynode.attr("height") = 10;
	    }else{
		phynode = new graph_pnode(paper, node_id, lpx, lpy);
	    }

            phynodes.push(phynode);

	    // Indexing for later (edge) use.
	    phynode_hash[node_id] = nidi;

	    // More indexing.
	    var ref_index = phynodes.length -1;
	    var phynode_id = phynode.id;
	    phynode_id_to_index[phynode_id] = ref_index;
	    phynode_id_to_node_id[phynode_id] = node_id;
	    node_id_to_index[node_id] = ref_index;

	    ll('display: indexed (node): node_id: ' + node_id +
			     ', phynode_id: ' + phynode_id +
			     ', ref_index: ' + ref_index);
	}

	// Add listeners.
	for (var i = 0, ii = phynodes.length; i < ii; i++) {
	    phynodes[i].dblclick(dblclick_event_handler);
            phynodes[i].drag(move, start, stop);
	    phynodes[i].mouseover(mouseover_event_handler);
	    phynodes[i].mouseout(mouseout_event_handler);
	}

	// Add stored connections.
	var connections = new Array();
	var conn_hash_ancestor = {};
	var conn_hash_descendant = {};
	for( var ei = 0; ei < layout.edge_list.length; ei++ ){

	    //
	    var edge = layout.edge_list[ei];
	    var e0 = edge[0];
	    var e1 = edge[1];

	    // Push edge onto array.
	    var n0_pnode = phynodes[phynode_hash[e0]];
	    var n1_pnode = phynodes[phynode_hash[e1]];
	    var d_label = layout.parent_distances[e0][e1] + '';
	    var nconn = new graph_connection(paper, n0_pnode, n1_pnode,
					     d_label);
	    connections.push(nconn);

	    // Index edge index for later recall.
	    if( ! conn_hash_descendant[e0] ){ conn_hash_descendant[e0] = {}; }
	    conn_hash_descendant[e0][e1] = ei;
	    if( ! conn_hash_ancestor[e1] ){ conn_hash_ancestor[e1] = {}; }
	    conn_hash_ancestor[e1][e0] = ei;

	    ll('display: indexed (edge): e0: ' + e0 +
	       ', e1: ' + e1 +
	       ', ei: ' + ei);
	}
	
	// See: https://github.com/sorccu/cufon/wiki/about
	// See: http://raphaeljs.com/reference.html#getFont
	// var txt = paper.print(100, 100, "print",
	//  paper.getFont("Museo"), 30).attr({fill: "#00f"});
	//paper.print(100, 100, "Test string", paper.getFont("Times", 800), 30);
	//txt[0].attr({fill: "#f00"});
    };

};
