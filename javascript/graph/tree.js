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

///
///  Graph sub-object.
///

// Needs somemore functionality...
bbop.model.tree.graph = function(){
    bbop.model.graph.call(this);

    var anchor = this;

    var max_dist = 0.0;
    var all_dists = {};
    //var  = {};
    function kid_info_up(node_id){
	
	var nid = node_id;
	print("working on: " + nid);

	// Can only have at most one parent.
	var node_parent = anchor.get_parent_nodes(nid);
	if( node_parent && node_parent.length ){
	    node_parent = node_parent[0];
	    var pid = node_parent.id();

	    // Add new data to global.
	    print(" seems to have parent: " + pid);
	    if( ! all_dists[pid]){
		all_dists[pid] = {};
	    }
	    if( ! all_dists[pid][nid] ){

		// TODO/BUG: get edge needs to return something we can use...
		//anchor.get_edge(nid,pid)._distance;
		var dist = 1.1;
		all_dists[pid][nid] = dist;

		// Look a little for max.
		if( dist > max_dist ){
		    max_dist = dist;
		}
	    }

	    // Get any data you can from your kids.
	    for( var k_id in all_dists[nid] ){

		var increment = all_dists[pid][nid] + all_dists[nid][k_id];
		all_dists[pid][k_id] = increment;

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
    this.layout = function (){

	// Bracketing and store a cumulative list of all children as
	// we walk up the tree and group.
	// Start by walking up from the leaves.
	var leaves = this.get_leaf_nodes();
	for( var li = 0; li < leaves.length; li++){
	    var leaf = leaves[li];
	    kid_info_up(leaf.id());
	}

	return {
	    distances: all_dists,
	    max_distance: max_dist,
	    brackets: []
	};
    };

    this.dump = function(){
	for( var n_id in all_dists){
	    for( var k_id in all_dists[n_id]){
		print(n_id + ' : '+ k_id + ' => ' + all_dists[n_id][k_id]);
	    }
	}
    };

};
bbop.model.tree.graph.prototype = new bbop.model.graph;
