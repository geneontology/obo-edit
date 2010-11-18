////////////
////
//// bbop.model.tree
////
//// Purpose: Extend the model to be handy for a (phylo)tree.
//// 
//// Taken name spaces:
////    bbop.model.tree.*
////
//// TODO: see: http://raphaeljs.com/graffle.html
////
//// Required:
////    bbop.core
////    bbop.model
////
//////////


// Module and namespace checking.
bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'model');
bbop.core.namespace('bbop', 'model', 'tree');


// Same same.
bbop.model.tree.node = function(new_id){
    bbop.model.node.call(this, new_id);
};
bbop.model.tree.node.prototype = new bbop.model.node;


// Add distance 
bbop.model.tree.edge = function(parent, child, distance){
    bbop.model.edge.call(this, child, parent, '');
    this._distance = distance || 0.0;
};
bbop.model.tree.edge.prototype = new bbop.model.edge;
bbop.model.tree.edge.prototype.distance = function(){
    return this._distance;
};
// Make sure that clone gets distance as well.
bbop.model.tree.edge.prototype.clone = function(){
    var tmp_clone = new bbop.model.tree.edge(this.object_id(),
					     this.subject_id(),
					     this.distance());
    tmp_clone.metadata(bbop.core.clone(this.metadata()));
    return tmp_clone;
};

///
///  Graph sub-object.
///

// Needs somemore functionality...
bbop.model.tree.graph = function(){
    bbop.model.graph.call(this);

    var anchor = this;

    var max_dist = 0.0;
    var all_dists_parent = {};
    var all_dists_child = {};
    //var matrix = {};
    function kid_info_up(node_id){
	
	var nid = node_id;
	//print("working on: " + nid);

	// Can only have at most one parent.
	var node_parent = anchor.get_parent_nodes(nid);
	if( node_parent && node_parent.length ){
	    node_parent = node_parent[0];
	    var pid = node_parent.id();

	    // Add new data to globals.
	    //print(" seems to have parent: " + pid);
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
	    kid_info_up(pid);
	}
    }

    // Return a layout that can be trivially rendered
    // by...something...
    var brackets = new Array();
    var max_width = 0;
    this.layout = function (){

	// Collect distance between every direct relative. Start by
	// walking up from the leaves. (needed?)
	// Also supply an intereger coord for every leaf (by def. widest).
	var found_node = {};
	var leaves = this.get_leaf_nodes();
	for( var li = 0; li < leaves.length; li++){
	    var leaf = leaves[li];
	    var leaf_id = leaf.id();
	    kid_info_up(leaf_id);

	    found_node[leaf_id] = li + 1;
	}
	
	// // TODO: 
	// var pig_pen = {};
	// for( li = 0; li < leaves.length; li++){
	//     // Get leaf nodes (with arbitrary order).

	//     // Check to see if any have same parent.

	//     // Check if parent is root.
	// }

	// Bracketing and store a cumulative list of all children as
	// we walk down the tree.
	var current_bracket = this.get_root_nodes();
	while( current_bracket.length > 0 ){
	    
	    //print("Current seed: " + current_bracket.length);

	    // Find max width.
	    if( max_width < current_bracket.length ){
		max_width = current_bracket.length;
	    }

	    //
	    var new_bracket = new Array();
	    var next_bracket = new Array();
	    for( var ni = 0; ni < current_bracket.length; ni++ ){

		//print(" Starting bracket: " + ni);

		// Add ids.
		var node = current_bracket[ni];
		var node_id = node.id();
		new_bracket.push(node_id);
		//print(" Pushing: " + node_id);

		// Find the kids and repeat.
		var cnodes = this.get_child_nodes(node_id);
		for( var ci = 0; ci < cnodes.length; ci++ ){
		    var cnode = cnodes[ci];
		    var cnode_id = cnode.id();
		    next_bracket.push(cnode);
		    //print(" Readying: " + cnode_id);
		}
	    }

	    brackets.push(new_bracket);

	    // Get ready to repeat if there were kids.
	    current_bracket = next_bracket;
	}

	//
	return {
	    parent_distances: all_dists_parent,
	    child_distances: all_dists_child,
	    max_distance: max_dist,
	    brackets: brackets,
	    max_width: max_width
	};
    };

    this.dump_dist = function(){
	for( var n_id in all_dists_parent ){
	    for( var k_id in all_dists_parent[n_id] ){
		//print(n_id + ' : '+ k_id + ' => ' + all_dists_parent[n_id][k_id]);
	    }
	}
    };

    this.dump_brackets = function(){
	for( var i = 0; i < brackets.length; i++ ){
	    for( var j = 0; j < brackets[i].length; j++ ){
		//print(brackets[i][j] + ' : ' + i);
	    }
	}
    };

};
bbop.model.tree.graph.prototype = new bbop.model.graph;
