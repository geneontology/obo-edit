/* 
 * Package: tree.js
 * 
 * Namespace: bbop.model.tree
 * 
 * Purpose: Extend <bbop.model> in <model.js> to be handy for a (phylo)tree.
 * 
 * TODO: /Much/ better documentation. I have no idea what's going on
 * in there anymore...
 * 
 * TODO: See: http://raphaeljs.com/graffle.html
 * 
 * TODO: Subtree calculation during bracket_down.
 */

// Module and namespace checking.
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'logger');
bbop.core.require('bbop', 'model');
bbop.core.namespace('bbop', 'model', 'tree');


// BUG/TODO: remove later...or something...
bbop.model.tree.logger = new bbop.logger();
bbop.model.tree.logger.DEBUG = true;
var _kvetch = bbop.model.tree.logger.kvetch;

/*
 * Namespace: bbop.model.tree.node
 * 
 * Constructor: node
 * 
 * Same as parent, but just takes id in constructor.
 * 
 * Arguments:
 *  new_id - a unique id for the node
 */
bbop.model.tree.node = function(new_id){
    bbop.model.node.call(this, new_id);
    this._is_a = 'bbop.model.tree.node';
};
bbop.core.extend(bbop.model.tree.node, bbop.model.node);

/*
 * Namespace: bbop.model.tree.edge
 * 
 * Constructor: edge
 * 
 * Same as parent, but adds distance.
 */
bbop.model.tree.edge = function(parent, child, distance){
    bbop.model.edge.call(this, child, parent, '');
    this._is_a = 'bbop.model.tree.edge';
    this._distance = distance || 0.0;
};
bbop.core.extend(bbop.model.tree.edge, bbop.model.edge);

/*
 * Function: distance
 *
 * Return an edge's "distance".
 *
 * Parameters: 
 *  n/a
 *
 * Returns: 
 *  number
 */
bbop.model.tree.edge.prototype.distance = function(){
    return this._distance;
};

/*
 * Function: clone
 *
 * Make sure that clone gets distance as well.
 *
 * Parameters: 
 *  n/a
 *
 * Returns: 
 *  <bbop.model.tree.edge>
 */
bbop.model.tree.edge.prototype.clone = function(){
    var tmp_clone = new bbop.model.tree.edge(this.object_id(),
					     this.subject_id(),
					     this.distance());
    tmp_clone.metadata(bbop.core.clone(this.metadata()));
    return tmp_clone;
};

/*
 * Namespace: bbop.model.tree.graph
 * 
 * Constructor: graph
 * 
 * Same as parent.
 * Needs some more functionality...
 */
bbop.model.tree.graph = function(){
    bbop.model.graph.call(this);
    this._is_a = 'bbop.model.tree.graph';

    // Useful for making sure that certain recursive functions keep
    // the desired notion of "this"ness.
    var anchor = this;

    /*
     * Function: default_sort
     *
     * The default comparator function for ordering the
     * brackets. Alphabetical down.
     * 
     * Parameters: 
     *  a - a bracket item
     *  b - a bracket item
     *
     * Returns: 
     *  string
     */
    this.default_sort = function(a, b){
	var sort_val = 0;
	if( a.id < b.id ){
	    sort_val = - 1;
	}else if( a.id > b.id ){
	    sort_val = 1;
	}
	//_kvetch('sort: ' + a.id + ' <?> ' + b.id + ' = ' + sort_val);
	return sort_val;
    };

    // Get information on kids, relations, and distances working our
    // way up from the leaves.
    var max_dist = 0.0;
    var all_dists_parent = {};
    var all_dists_child = {};
    var node_list = new Array();
    var node_hash = {};
    var edge_list = new Array();
    var edge_hash = {};
    function info_up(node_id){
	
	var nid = node_id;
	_kvetch("info_up: working on: " + nid);

	// Node bookkeeping.
	if( ! node_hash[nid] ){
	    node_hash[nid] = true;
	    node_list.push(nid);
	}

	// Can only have at most one parent.
	var node_parent = anchor.get_parent_nodes(nid);
	if( node_parent && node_parent.length ){
	    node_parent = node_parent[0];
	    var pid = node_parent.id();

	    // Edge bookkeeping.
	    var edge_uid = pid + '_!muNge!_' + node_id;
	    if( ! edge_hash[edge_uid] ){
		edge_hash[edge_uid] = true;
		edge_list.push([pid, node_id]);
		_kvetch('info_up: indexing: ' + edge_uid);
	    }

	    // Add new data to globals.
	    _kvetch(" info_up: seems to have parent: " + pid);
	    if( ! all_dists_parent[pid]){
		all_dists_parent[pid] = {};
	    }
	    if( ! all_dists_child[nid]){
		all_dists_child[nid] = {};
	    }

	    if( ! all_dists_parent[pid][nid] ){
		// 
		var dist = anchor.get_edge(nid,pid).distance();
		all_dists_parent[pid][nid] = dist;
		all_dists_child[nid][pid] = dist;
		// Look a little for max.
		if( dist > max_dist ){
		    max_dist = dist;
		}
	    }

	    // Get any data you can from your kids.
	    for( var k_id in all_dists_parent[nid] ){

		var increment = all_dists_parent[pid][nid] +
		    all_dists_parent[nid][k_id];
		all_dists_parent[pid][k_id] = increment;
		all_dists_child[k_id][pid] = increment;

		// Look a little for max.
		if( increment > max_dist ){
		    max_dist = increment;
		}
	    }

	    // Recur on parent.
	    info_up(pid);
	}
    }

    // Recursive comb down (give partitioned ordering).
    // A bracket looks like: "[{id:a, brackets: [...]}, ...]".
    // TODO: subtree calculation during.
    var brackets = new Array();
    var max_depth = 0;
    function bracket_down(in_node, lvl, parent_node_id){
	    
	// Bootstrap lvl to 1.
	if( ! lvl ){ lvl = 1; }
	if( ! parent_node_id ){ parent_node_id = null; }

	var in_node_id = in_node.id();
	_kvetch(' bracket_down: ' + in_node_id);

	// 
	var child_bracket = new Array();
	var child_nodes = anchor.get_child_nodes(in_node_id);
	for( var cb = 0; cb < child_nodes.length; cb++ ){
	    var child_node = child_nodes[cb];
	    child_node_id = child_node.id();
	    _kvetch('  bracket_down: pushing: ' + child_node_id);
	    child_bracket.push(bracket_down(child_node, lvl + 1, in_node_id));
	}

	// Sort the children.
	child_bracket.sort(anchor.default_sort);

	// Grab max depth.
	if( lvl > max_depth ){ max_depth = lvl;	}

	//
	_kvetch(' bracket_down: found kids: ' + child_bracket.length);
	return {
	    id: in_node_id,
	    routing_node: false,
	    level: lvl,
	    parent_id: parent_node_id,
	    brackets: child_bracket
	};
    }

    // Return a layout that can be trivially rendered
    // by...something...
    var max_width = 0;
    var cohort_list = new Array(); // will reinit

    /*
     * Function: layout
     *
     * With the current graph, produce a usable layout object.
     * 
     * TODO: layout should take bracket ordering func
     *
     * Parameters:
     *  n/a
     *
     * Returns: 
     *  a rather complicated layout object
     */
    this.layout = function (){

	// Refresh scope on new layout call.
	brackets = new Array();
	node_list = new Array();
	node_hash = {};
	edge_list = new Array();
	edge_hash = {};
	cohort_list = new Array(); // token--now also reset and sized below

	// Pass one:
	// Collect all of our bracketing information, also order the
	// brackets to some function.
	var base_nodes = anchor.get_root_nodes();
	for( var bb = 0; bb < base_nodes.length; bb++ ){
	    _kvetch('bracket_down: start: ' +
			     base_nodes[bb].id());
	    brackets.push(bracket_down(base_nodes[bb]));
	}
	// The children are ordered--make the top-level one ordered as
	// well.
	brackets.sort(anchor.default_sort);

	// Pass one:
	// Essentially walk the brackets, find brackets that end early
	// (above max_depth) and add routing nodes down.
	function dangle_routing(in_item){
	    if( in_item.level < max_depth ){
		in_item.brackets.push({id: in_item.id,
				       routing_node: true,
				       level: in_item.level + 1,
				       parent_id: in_item.id,
				       brackets: []
				      });
		dangle_routing(in_item.brackets[0]);
	    }
	    return in_item;
	}
	function add_routing(in_brackets){

	    //
	    for( var i = 0; i < in_brackets.length; i++ ){
		var item = in_brackets[i];

		//
		if( item.brackets.length == 0 && item.level < max_depth ){
		    _kvetch(' add_routing: dangle: ' + item.id);
		    dangle_routing(item);
		}else if( item.brackets.length != 0 ){
		    _kvetch(' add_routing: descend: ' + item.id);
		    add_routing(item.brackets);
		}
	    }
	}
	add_routing(brackets);

	// Pass three:
	// Collect global cohort information into a matrix (cohort_list).
	cohort_list = new Array(max_depth);
	for( cli = 0; cli < cohort_list.length; cli++ ){
	    cohort_list[cli] = new Array();
	}
	// Walk down and stack up.
	function order_cohort(in_brackets){	    
	    // Push into global cohort list list.
	    for( var i = 0; i < in_brackets.length; i++ ){
		var bracket_item = in_brackets[i];
		//
		_kvetch(' order_cohort: i: ' + i);
		_kvetch(' order_cohort: lvl: ' + bracket_item.level);
		cohort_list[bracket_item.level - 1].push(bracket_item);
		// Drill down.
		if( bracket_item.brackets.length > 0 ){
		    _kvetch(' order_cohort: down: ' +
				     bracket_item.brackets.length);
		    order_cohort(bracket_item.brackets);
		}
	    }
	}
	order_cohort(brackets);

	// Gather distance info up from leaves.
	var base_info_nodes = anchor.get_leaf_nodes();
	max_width = base_info_nodes.length; // by def, leaves are widest
	for( var bi = 0; bi < base_info_nodes.length; bi++ ){
	    info_up(base_info_nodes[bi].id());
	}

	///
	/// Decide relative y positions by walking backwards through
	/// the cohorts.
	///


	// Walk backwards through the cohorts to find a base Y position. for
	// the final cohort.
	var position_y = {};
	var final_cohort = cohort_list[(max_depth - 1)];
	_kvetch('look at final cohort: ' + (max_depth - 1));
	for( var j = 0; j < final_cohort.length; j++ ){
	    var item = final_cohort[j];
	    //var local_shift = j + 1.0; // correct, but shifts too far down
	    var local_shift = j + 0.0;
	    position_y[item.id] = local_shift;
	    _kvetch('position_y: ' + item.id + ', ' + local_shift);
	}
	// Walk backwards through the remaining cohorts to find the best Y
	// positions.
	for( var i = cohort_list.length - 1; i > 0; i-- ){
	    //
	    var cohort = cohort_list[i - 1];
	    _kvetch('look at cohort: ' + (i - 1));
	    for( var j = 0; j < cohort.length; j++ ){
		var item = cohort[j];

		// Deeper placements always take precedence.
		if( position_y[item.id] != undefined ){
		    _kvetch('position_y (old): '+ item.id);
		}else{

		    // If you have one parent, they have the same Y as you.
		    // This generalizes to: the parent has the average Y of
		    // it's children. This is easy then, once we
		    // start, but how to get the initial leaf
		    // placement? Get item's children and take their
		    // average (by definition, they must already be in
		    // the placed list (even if it was just a routing
		    // node)).
		    var c_nodes = anchor.get_child_nodes(item.id);
		    var position_acc = 0.0;
		    for( var ci = 0; ci < c_nodes.length; ci++ ){
			var node = c_nodes[ci];
			position_acc = position_acc + position_y[node.id()];
		    }
		    // _kvetch(' position_acc: ' + position_acc);
		    // _kvetch(' c_nodes: ' + c_nodes);
		    // _kvetch(' c_nodes.length: ' + c_nodes.length);
		    var avg = position_acc / (c_nodes.length * 1.0);
		    position_y[item.id] = avg;
		    _kvetch('position_y (new): '+ item.id +', '+ avg);
		}
	    }
	}
 
	//
	var x_offset = 0.0;
	var position_x = {};
	var roots = anchor.get_root_nodes();
	for( var r = 0; r < roots.length; r++ ){

	    var root_id = roots[r].id();
	    position_x[root_id] = x_offset;
	    _kvetch('position_x:: ' + root_id + ', '
			     + position_x[root_id]);
    
	    if( item.routing_node == false ){
		// Get kids and their x distance (for placement).
		for( var nid in all_dists_parent[root_id] ){
		    var dist = all_dists_parent[root_id][nid] + x_offset;
		    position_x[nid] = dist;
		    _kvetch('position_x:: ' + nid + ', ' + dist);
		}
	    }
	}

	//
	return {
	    parent_distances: all_dists_parent,
	    child_distances: all_dists_child,
	    max_distance: max_dist,
	    max_depth: max_depth,
	    max_width: max_width,
	    cohorts: cohort_list,
	    //routing: routing_list,
	    brackets: brackets,
	    node_list: node_list,
	    edge_list: edge_list,
	    position_x: position_x,
	    position_y: position_y
	};
    };

    /*
     * Function: dump_cohorts
     *
     * Dump the cohorts; for debugging?
     *
     * Parameters:
     *  n/a
     *
     * Returns: 
     *  n/a
     */
    this.dump_cohorts = function(){
    	for( var i = 0; i < cohort_list.length; i++ ){
    	    for( var j = 0; j < cohort_list[i].length; j++ ){
    		var item = cohort_list[i][j];
    		_kvetch(item.id + ' ' + i + ':' + j +
				 ', ' + item.routing_node);
    	    }
    	}
    };

    /*
     * Function: dump_dist
     *
     * Dump distances; for debugging?
     *
     * Parameters: 
     *  in_arg - string; 'child'/'parent'?
     *
     * Returns: 
     *  n/a
     */
    this.dump_dist = function(in_arg){

	_kvetch(' in ');

	// Argument selection.
	var dists = all_dists_parent;
	if( in_arg == "child" ){
	    dists = all_dists_child;
	}

	// Dump selected dist.
	for( var n_id in dists ){
	    for( var k_id in dists[n_id] ){
		_kvetch(n_id +' : '+ k_id +' => '+ dists[n_id][k_id]);
	    }
	}
    };

    /*
     * Function: dump_brackets
     *
     * Dump brackets; for debugging?
     *
     * Parameters: 
     *  brack - *[optional]* ???
     *
     * Returns: 
     *  n/a
     */
    this.dump_brackets = function(brack){

	// Bootstrap if just starting.
	if( ! brack ){ brack = brackets; }
	//if( ! lvl ){ lvl = 1; }

	// Printer.
	for( var i = 0; i < brack.length; i++ ){

	    var pid = '(null)';
	    if( brack[i].parent_id ){ pid = brack[i].parent_id; }

	    _kvetch('id: ' + brack[i].id +
			     ', parent: ' + pid +
			     ', level: ' + brack[i].level);
	    this.dump_brackets(brack[i].brackets);
	}
    };

};
bbop.core.extend(bbop.model.tree.graph, bbop.model.graph);
// Overload add_node to add label information to new object.