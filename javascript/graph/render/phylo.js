////////////
////
//// bbop.render.phylo
////
//// Purpose: Extend the model to be handy for a (phylo)tree.
//// 
//// Taken name spaces:
////    bbop.render.phylo.*
////
//// FIX: text animation
//// TODO: togglable visability on nodes
//// STARTED: looks top-level; make functional for application use.
//// STARTED: hide whole subtrees on double-click
//// TODO: font and text placement
//// TODO: better text alignment
//// TODO: floating right-hand text (see PAINT)
//// TODO: add distances at AmiGO-level?
////
//// Required:
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

// Init: context, label, x-coord, y-coord.
Raphael.fn.pnode = function(context, label, px, py){

    // Color and size definitions.
    var base_node_color = "#00f";
    var text_offset_x = box_width / 2.0;
    var text_offset_y = box_height / 2.0;
    var animation_time = 100;
    
    // Draw out initial node.
    this._context = context;

    this._text = // NOTE: text is *centered* at this point.
	this._context.text(px + text_offset_x, py + text_offset_y, label);
    this._text.toBack(); // make sure it's behind the boxes
    this._shape = this._context.rect(px, py, box_width, box_height, 2);

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
    // Event handlers.
    this.drag = function(move_handler, start_handler, stop_handler){
	this._shape.drag(move_handler, start_handler, stop_handler);
    };
    this.dblclick = function(handler){
	this._shape.dblclick.call(this._shape, handler);
    };
    this.mouseover = function(handler){
	this._shape.mouseover.call(this._shape, handler);
    };
    this.mouseout = function(handler){
	this._shape.mouseout.call(this._shape, handler);
    };

    //this.text_attr = this._text.attr;
    this.update = function(attr){
	return this._shape.animate.call(this._shape, attr, animation_time);
    };

    // Add to the object the initial position.
    //this.ox = this._shape.attr("x");
    this._start_shape_y = this._shape.attr("y");
    this._start_text_y = this._text.attr("y");
    this.init_move = function(){
	this._start_shape_y = this._shape.attr("y");
	this._start_text_y = this._text.attr("y");
    };
    this.move_y = function(arg){
	var d_shape = this._start_shape_y + arg;
	var d_text = this._start_text_y + arg;
	this._shape.attr.call(this._shape, {"y": d_shape});
	this._text.attr.call(this._text, {"y": d_text});
	//return this._shape.attr.call(this._shape, {"y": arg});
    };
    //this.text_ox = this._text.attr("x");
    //this.text_oy = this._text.attr("y");

    // Setup shape attributes.
    this._shape.attr({fill: base_node_color,
		      stroke: base_node_color,
		      //"fill-opacity": .25,
		      title: "This is " + label,
		      "fill-opacity": 0.0,
		      "stroke-width": 2,
		      cursor: "move"});
};


// Init: context, shape, shape.
Raphael.fn.connection = function(context, obj1, obj2){

    //this.context = context;

    // These need to be set right away for path calculation.    
    this.from = obj1;
    this.to = obj2;

    // Get path.
    var path = this.get_path_between();

    // Future visibility.
    this.visible = true;

    var base_edge_color = "#030";
    var base_edge_width = "3";
    var hilite_edge_color = "#00f";
    var hilite_edge_width = "5";
    var invisible_edge_color = "#000";
    var invisible_edge_width = "0";

    this.base_attr = {
	"stroke": base_edge_color,
	"fill": "none",
     	"stroke-width": base_edge_width
    };

    this.base_attr = {
	"stroke": base_edge_color,
	"fill": "none",
     	"stroke-width": base_edge_width
    };

    this.hilite_attr = {
	"stroke": hilite_edge_color,
	"fill": "none",
     	"stroke-width": hilite_edge_width
    };

    this.invisible_attr = {
	"stroke": invisible_edge_color,
	"fill": "none",
     	"stroke-width": invisible_edge_width
    };

    // Colors and lines.
    this.line = context.path(path);
    this.line.attr(this.base_attr);
};
// Update line graphic.
Raphael.fn.connection.prototype.update = function(message){

    // Get path.
    var path = this.get_path_between();

    // Update line.
    this.line.attr({path: path});

    // 
    if( this.visible == false ){
	this.line.attr({"stroke": this.color,
			"fill": "none",
     			"stroke-width": this.line_width});	
    }else if( message == 'highlight' ){
	this.line.attr(this.hilite_attr);
	//}else if( message == 'highlight' ){
	//this.line.attr(this.hilite_attr);
    }else{
	this.line.attr(this.base_attr);
    }
};
// Generate path from between the two internally stored objects.
Raphael.fn.connection.prototype.get_path_between = function(){

    // bbop.core.kvetch('1: ' + this);
    // bbop.core.kvetch('2: ' + this.from);
    // bbop.core.kvetch('3: ' + this.from.id);
    // bbop.core.kvetch('4: ' + this.from.getBBox);
    var bb1 = this.from.getBBox();
    var bb2 = this.to.getBBox();

    //bbop.core.kvetch("bb1.width: " + bb1.width);
    //bbop.core.kvetch("bb1.x: " + bb1.x + ", bb1.y: " + bb1.y);
    //bbop.core.kvetch("bb1.width: " + bb1.width +", bb1.height: "+ bb1.height);

    var p = [{x: bb1.x + bb1.width / 2, y: bb1.y - 1},
             {x: bb1.x + bb1.width / 2, y: bb1.y + bb1.height + 1},
             {x: bb1.x - 1, y: bb1.y + bb1.height / 2},
             {x: bb1.x + bb1.width + 1, y: bb1.y + bb1.height / 2},
             {x: bb2.x + bb2.width / 2, y: bb2.y - 1},
             {x: bb2.x + bb2.width / 2, y: bb2.y + bb2.height + 1},
             {x: bb2.x - 1, y: bb2.y + bb2.height / 2},
             {x: bb2.x + bb2.width + 1, y: bb2.y + bb2.height / 2}];
    var d = {};
    var dis = [];
    for (var i = 0; i < 4; i++) {
        for (var j = 4; j < 8; j++) {
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
    var dx = Math.max(Math.abs(x1 - x2) / 2, 10);
    var dy = Math.max(Math.abs(y1 - y2) / 2, 10);
    return [
    	"M", x1.toFixed(3), y1.toFixed(3),
    	"L", x1.toFixed(3), y2.toFixed(3),
    	"L", x2.toFixed(3), y2.toFixed(3)
    ].join(",");
};

// Example graph hand loaded from:
// (http://amigo.berkeleybop.org/amigo/panther/PTHR10004.tree).
// AN0(AN1(AN2(XP_800359:0.687,XP_790652:0.774,XP_800360:0.695):0.473,AN6(Q7RKB3:1.366,Q7RBF2:1.208):0.223):1.0,Q747I8:1.0);
var an0 = new bbop.model.tree.node('AN0', 'AN0');
var an1 = new bbop.model.tree.node('AN1', 'AN1');
var an2 = new bbop.model.tree.node('AN2', 'AN2');
var an6 = new bbop.model.tree.node('AN6', 'AN6');
var xp9 = new bbop.model.tree.node('XP_800359', 'XP_800359');
var xp2 = new bbop.model.tree.node('XP_790652', 'XP_790652');
var xp0 = new bbop.model.tree.node('XP_800360', 'XP_800360');
var q7rk = new bbop.model.tree.node('Q7RKB3', 'Q7RKB3');
var q7rb = new bbop.model.tree.node('Q7RBF2', 'Q7RBF2');
var q747 = new bbop.model.tree.node('Q747I8', 'Q747I8');

var e0 = new bbop.model.tree.edge(an0, an1, 1.0);
var e1 = new bbop.model.tree.edge(an0, q747, 1.0);
var e2 = new bbop.model.tree.edge(an1, an2, 0.473);
var e3 = new bbop.model.tree.edge(an1, an6, 0.223);
var e4 = new bbop.model.tree.edge(an2, xp9, 0.687);
var e5 = new bbop.model.tree.edge(an2, xp2, 0.774);
var e6 = new bbop.model.tree.edge(an2, xp0, 0.695);
var e7 = new bbop.model.tree.edge(an6, q7rk, 1.366);
var e8 = new bbop.model.tree.edge(an6, q7rb, 1.208);

var t = new bbop.model.tree.graph();
t.add_node(an0);
t.add_node(an1);
t.add_node(an2);
t.add_node(an6);
t.add_node(xp9);
t.add_node(xp2);
t.add_node(xp0);
t.add_node(q7rk);
t.add_node(q7rb);
t.add_node(q747);
t.add_edge(e0);
t.add_edge(e1);
t.add_edge(e2);
t.add_edge(e3);
t.add_edge(e4);
t.add_edge(e5);
t.add_edge(e6);
t.add_edge(e7);
t.add_edge(e8);
var layout = t.layout();

// Explicit info dump.
var x_scale = 1.0;
var y_scale = 1.0;

// DEBUG
function info_dump(){
    //   
    for( var ni = 0; ni < layout.node_list.length; ni++ ){
	var noid = layout.node_list[ni];
	bbop.core.kvetch('info(node): (' + noid +
			 ') x:' + (layout.position_x[noid] * x_scale) +
			 ', y:' + (layout.position_y[noid] * y_scale));
    }
    // 
    for( var ei = 0; ei < layout.edge_list.length; ei++ ){
	var edge = layout.edge_list[ei];
	bbop.core.kvetch('info(edge): (' + edge[0] + ', ' + edge[1] + '): ' +
			 (layout.parent_distances[edge[0]][edge[1]]) * y_scale);
    }
    //
    bbop.core.kvetch('info(width): ' + layout.max_width * y_scale);
    //bbop.core.kvetch('info(depth): ' + layout.max_depth);
    bbop.core.kvetch('info(distance): ' + layout.max_distance * x_scale);

    bbop.core.kvetch('info(x_scale): ' + x_scale);
    bbop.core.kvetch('info(y_scale): ' + y_scale);
}

// DEBUG
info_dump();

// Let's play with arbitrary dimensions. Evrything should be
// packed/spread within these bounds (eventually tied to eindow size?).
var box_width = 50;
var box_height = 30;
//var edge_buffer = 0;
var edge_buffer = 100;
var edge_shift = edge_buffer / 2.0;

// Adjust scales.
var a_width = 800; // TODO: pick that out of Raphael
x_scale = a_width / layout.max_distance;
//y_scale = a_height / layout.max_width; // screen-variable y-scale
y_scale = box_height * 2.0; // fixed y-scale
var a_height = layout.max_width * y_scale;

// Render out.
window.onload = function () {

    // Do a screen width calc.
    // Adjust for box width and other extras as well.
    if( window && window.innerWidth ){
	a_width = window.innerWidth - box_width - edge_buffer;
	x_scale = a_width / layout.max_distance;
    }else if( document && document.body && document.body.offsetWidth ){
	a_width = document.body.offsetWidth - box_width - edge_buffer;
	x_scale = a_width / layout.max_distance;
    }
    a_width = a_width + 1; // Get that last pixel column on board.
    bbop.core.kvetch('width: ' + a_width);

    // DEBUG
    info_dump();

    // Create context.
    var paper = Raphael("test1", a_width + edge_buffer, a_height + edge_buffer);
    bbop.core.kvetch('onload: made paper');

    ///
    /// Graph helper function definitions.
    /// 

    // Subtree list, including self.
    function gather_list_from_hash(nid, hash){
    	var retlist = new Array();
    	retlist.push(nid);
    	// Get all nodes cribbing from distances.
    	for( vt in hash[nid] ){
    	    //bbop.core.kvetch("id: " + id + ", v: " + ct);
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
		    bbop.core.kvetch('get_conn: found: [' + conn_index +
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
            //phynode.start_y = phynode.current_y();
	    phynode.init_move();
            phynode.update({"fill-opacity": 0.2});
            //phynode.text_oy = phynode.text_attr("y");
            //     var text = assoc_texts[ti];
            //     text.animate({"fill-opacity": .2}, animation_time);
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
	    //bbop.core.kvetch('mshp['+si+']:'+' oy: '+mshp.start_y+', dy:'+dy);
    	}

	// Update connections.
        for (var i = connections.length; i--;) {
            connections[i].update();
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
            mshp.update({"fill-opacity": 0.0});
	    // TODO: text darken
	    //mshp.animate({"fill-opacity": 1.0});
    	}
    };

    // Experiment with double click.
    function dblclick_event_handler(event){
	// this.animate({"fill": "green",
	// 	      "fill-opacity": 0.5},
	// 	     2000);
	// //sleep(2000);
	// this.animate({"fill": base_node_color,
	// 	      "fill-opacity": 0.0},
	// 	     2000);
	// // this.attr({fill: "red"});
	var phynode_id = this.id;

	// "Vanish" edges.
	var subtree_edges = get_descendant_connections(phynode_id);
	for( var se = 0; se < subtree_edges.length; se++ ){
	    var ste = subtree_edges[se];
	    ste.visible = false;
	    ste.update();
	}

	// TODO: Nodes and text.
	var subtree_nodes = get_descendant_phynodes(phynode_id);
	for( var sn = 0; sn < subtree_nodes.length; sn++ ){
	    var stn = subtree_nodes[sn];
	    stn.update({fill: "red"});
	}
	//bbop.core.kvetch('dblclick: ' + subtree_list.join(', '));
    }

    // Experiment with hover.
    function mouseover_event_handler(event){

    	var phynode_id = this.id;

	// Cycle through ancestro phynodes.
    	var anc_phynodes = get_ancestor_phynodes(phynode_id);
    	for( var ai = 0; ai < anc_phynodes.length; ai++ ){
	    // Change boxes opacity (darken).
	    var ashp = anc_phynodes[ai];
	    ashp.update({"fill-opacity": 0.5});
	}

	// See if we can fish any edges out and highlight them.
    	var anc_edges = get_ancestor_connections(phynode_id);
    	for( var ac = 0; ac < anc_edges.length; ac++ ){
	    var aconn = anc_edges[ac];
	    aconn.update("highlight");
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
	    ashp.update({"fill-opacity": 0.0});
    	}

	// See if we can fish any edges out and unhighlight them.
    	var anc_edges = get_ancestor_connections(phynode_id);
    	for( var ac = 0; ac < anc_edges.length; ac++ ){
	    var aconn = anc_edges[ac];
	    aconn.update();
	}
	paper.safari();
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

	// Indexing for later (edge) use.
	phynode_hash[node_id] = nidi;

	// 
	var phynode = new Raphael.fn.pnode(paper, node_id, lpx, lpy);
        phynodes.push(phynode);

	// Indexing.
	var ref_index = phynodes.length -1;
	var phynode_id = phynode.id;
	phynode_id_to_index[phynode_id] = ref_index;
	phynode_id_to_node_id[phynode_id] = node_id;
	node_id_to_index[node_id] = ref_index;

	bbop.core.kvetch('onload: indexed (node): node_id: ' + node_id +
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
    // TODO: add connections to indexing hash for later access.
    var connections = new Array();
    var conn_hash_ancestor = {};
    var conn_hash_descendant = {};
    for( var ei = 0; ei < layout.edge_list.length; ei++ ){

	//
	var edge = layout.edge_list[ei];
	var e0 = edge[0];
	var e1 = edge[1];

	// Push edge onto array.
	connections.push(new Raphael.fn.connection(paper,
						   phynodes[phynode_hash[e0]],
						   phynodes[phynode_hash[e1]]));

	// Index edge index for later recall.
	if( ! conn_hash_descendant[e0] ){ conn_hash_descendant[e0] = {}; }
	conn_hash_descendant[e0][e1] = ei;
	if( ! conn_hash_ancestor[e1] ){ conn_hash_ancestor[e1] = {}; }
	conn_hash_ancestor[e1][e0] = ei;

	bbop.core.kvetch('onload: indexed (edge): e0: ' + e0 +
			 ', e1: ' + e1 +
			 ', ei: ' + ei);
    }

    // See: https://github.com/sorccu/cufon/wiki/about
    // See: http://raphaeljs.com/reference.html#getFont
    // var txt = paper.print(100, 100, "print", paper.getFont("Museo"), 30).attr({fill: "#00f"});
    //paper.print(100, 100, "Test string", paper.getFont("Times", 800), 30);
    //txt[0].attr({fill: "#f00"});
};
