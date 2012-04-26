////////////
////
//// bbop.model
////
//// Purpose: Basic edged graph and operations.
//// 
//// NOTE: A model instance may not the whole graph, just a
//// subgraph--this is the difference between nodes and
//// named_nodes. nodes are real things, while named_nodes are things
//// referenced by edges.
////
//// Check TODOs, we would like everything as linear as possible.
////
//// TODO: memoize everything but add_*. Functional enough that it
//// should work if we just empty the caches after every add_* op.
////
//// Taken name spaces:
////    bbop.model.*
////
//// Required:
////    bbop.core
////
//////////


// Module and namespace checking.
bbop.core.require('bbop', 'core'); // not needed, but want the habit
bbop.core.namespace('bbop', 'model');


// 
bbop.model.default_predicate = 'points_at';


///
///  Node sub-object.
///

bbop.model.node = function(new_id, new_label){
    this._id = new_id || undefined;
    this._label = new_label || undefined;

    // Only have a real type if the constructor went properly.
    this._type = 'node';
    if( ! new_id ){
	this._type = undefined;	
    }

    this._metadata = undefined;
};

// Prototype getter/setters.
bbop.model.node.prototype.id = function(value){
    if(value) this._id = value; return this._id; };
bbop.model.node.prototype.type = function(value){
    if(value) this._type = value; return this._type; };
bbop.model.node.prototype.label = function(value){
    if(value) this._label = value; return this._label; };
bbop.model.node.prototype.metadata = function(value){
    if(value) this._metadata = value; return this._metadata; };

// Clone, using bbop.core.clone for metadata object.
bbop.model.node.prototype.clone = function(){
    var tmp_clone = new bbop.model.node(this.id());
    tmp_clone.type(this.type());
    tmp_clone.label(this.label());
    tmp_clone.metadata(bbop.core.clone(this.metadata()));
    return tmp_clone;
};


///
///  Edge sub-object.
///

// If no predicate is given, default. Predicates are currently treated
// as raw strings.
bbop.model.edge = function(subject, object, predicate){

    // Either a string or a node.
    if( ! subject ){
	this._subject_id = undefined;
    }else if( typeof subject == 'string' ){
	this._subject_id = subject;	
    }else{
	this._subject_id = subject.id();
    }
    // Either a string or a node.
    if( ! object ){
	this._object_id = undefined;
    }else if( typeof object == 'string' ){
	this._object_id = object;	
    }else{
	this._object_id = object.id();
    }
    // Predicate default or incoming.
    this._predicate_id = bbop.model.default_predicate;
    if( predicate ){
	this._predicate_id = predicate;
    }

    // Only have a real type if the constructor went properly.
    this._type = 'edge';
    if( ! subject || ! object ){
	this._type = undefined;	
    }

    //
    this._metadata = undefined;
};

// Prototype getter/setters.
bbop.model.edge.prototype.subject_id = function(){
    return this._subject_id; };
bbop.model.edge.prototype.object_id = function(){
    return this._object_id; };
bbop.model.edge.prototype.predicate_id = function(){
    return this._predicate_id; };
bbop.model.edge.prototype.type = function(value){
    if(value) this._type = value; return this._type; };
bbop.model.edge.prototype.metadata = function(value){
    if(value) this._metadata = value; return this._metadata; };

// Clone, using bbop.core.clone for metadata object.
bbop.model.edge.prototype.clone = function(){
    var tmp_clone = new bbop.model.edge(this.subject_id(),
					this.object_id(),
					this.predicate_id());
    tmp_clone.metadata(bbop.core.clone(this.metadata()));
    return tmp_clone;
};


///
///  Graph sub-object.
///

// TODO: make compilation piecewise with every added node and edge.
bbop.model.graph = function(){

    this._id = undefined;

    // For bbop.model.node and bbop.model.edge (hash not possible for
    // edges--only relation, not "real").
    this._nodes = { array: new Array, hash: {} };
    this._edges = { array: new Array };
    this._predicates = { array: new Array, hash: {} };

    // All things that are referenced by edges (which may not include
    // actual node ids--dangling links).
    this._named_nodes = { array: new Array, hash: {} };

    // Useful forthings like leaves, roots, and singletons.
    this._subjects = { array: new Array, hash: {} };
    this._objects = { array: new Array, hash: {} };     

    // Table structures for quick lookups of relations.
    //this._predicate_subject_table = {};    // [pred][sub] -> bbop.model.edge.
    //this._subject_predicate_table = {};    // [sub][pred] -> bbop.model.edge.
    //this._predicate_object_table = {};     // [pred][obj] -> sub data struct.
    //this._object_predicate_table = {};     // [obj][pred] -> sub data struct.

    // New parallel structures to for our simplified graph.
    this._so_table = {}; // true/undef
    this._os_table = {}; // true/undef
    this._sop_table = {}; // {'rel1': true, 'rel2': true}

    // Table structures for quick lookups of node properties.
    this._is_a_singleton_lookup = {}; // [nid] -> bbop.model.node.    
};


// id getter/setter.
bbop.model.graph.prototype.id = function(value){
    if( value ) this._id = value; return this._id;
};
    
    
//
bbop.model.graph.prototype.add_node = function(node){

    // Check for for anything funny.
    if( ! node.id() ||
	this._nodes.hash[ node.id() ] ||
	this._nodes.hash[ node.id() ] ){
	    //alert("tried to add same node: " + node.id());
	    throw new Error("tried to add same node: " + node.id());
	}else{

	    var nid = node.id();
	    
	    // Add it to all the concerned recall data structures.
	    this._nodes.hash[ nid ] = node;
	    this._nodes.array.push(node);
	    this._named_nodes.hash[ nid ] = node;
	    this._named_nodes.array.push(node);

	    // If this does not belong to any relation so far, then it is a
	    // singleton.
	    if( ! this._subjects.hash[ nid ] && ! this._objects.hash[ nid ] ){
		this._is_a_singleton_lookup[ nid ] = true;
	    }
	}
};


//
bbop.model.graph.prototype.add_edge = function(edge){

    //
    var sub_id = edge.subject_id();
    var obj_id = edge.object_id();
    var pred_id = edge.predicate_id();

    // Subject -> object.
    if( ! this._so_table[ sub_id ] ){ this._so_table[ sub_id ] = {}; }
    this._so_table[ sub_id ][ obj_id ] = true;
    // Object -> subject.
    if( ! this._os_table[ obj_id ] ){ this._os_table[ obj_id ] = {}; }
    this._os_table[ obj_id ][ sub_id ] = true;
    // Their relationships (defined by SOP).
    if( ! this._sop_table[ sub_id ] ){
	this._sop_table[ sub_id ] = {}; }
    if( ! this._sop_table[ sub_id ][ obj_id ] ){
	this._sop_table[ sub_id ][obj_id] = {}; }
    //this._sop_table[ sub_id ][ obj_id ][ pred_id ] = true;
    this._sop_table[ sub_id ][ obj_id ][ pred_id ] = edge;

    // If this is a new predicate add it to all of the necessary data
    // structures.
    if( ! this._predicates.hash[ pred_id ] ){
	this._predicates.hash[ pred_id ] = true; 
	this._predicates.array.push(pred_id); 
    }

    // 
    if( ! this._subjects.hash[ sub_id ] ){
	this._subjects.hash[ sub_id ] = true; 
	this._subjects.array.push(sub_id); 
	//this._subject_predicate_table[ sub_id ] = {};
    }
    if( ! this._objects.hash[ obj_id ] ){
	this._objects.hash[ obj_id ] = true; 
	this._objects.array.push(obj_id); 
	//this._object_predicate_table[ obj_id ] = {};
    }

    // Remove the edge's subject and object from the singleton table.
    if( this._is_a_singleton_lookup[ sub_id ] ){
	delete this._is_a_singleton_lookup[ sub_id ]; }
    if( this._is_a_singleton_lookup[ obj_id ] ){
	delete this._is_a_singleton_lookup[ obj_id ]; }

    // Onto the array and subject and object into named bodies.
    this._edges.array.push(edge);
    if( ! this._named_nodes.hash[ sub_id ] ){
	this._named_nodes.array.push(sub_id); }
    this._named_nodes.hash[ sub_id ] = edge;
    if( ! this._named_nodes.hash[ obj_id ] ){
	this._named_nodes.array.push(obj_id); }
    this._named_nodes.hash[ obj_id ] = edge;
};


// Returns an original list of all added nodes.
bbop.model.graph.prototype.all_nodes = function(){
    return this._nodes.array;
};


// Returns an original list of all added edges.
bbop.model.graph.prototype.all_edges = function(){
    return this._edges.array;
};


// Returns an original list of all added predicates.
bbop.model.graph.prototype.all_predicates = function(){
    return this._predicates.array;
};


// List all external nodes by referenced id.
bbop.model.graph.prototype.all_dangling = function(){
    // Disjoint of named and extant.
    var unnamed = new Array();
    for( var named_id in this._named_nodes.hash ){
	if( ! this._nodes.hash[named_id] ){
	    unnamed.push(named_id);
	}
    }
    return unnamed;
};

// Any bad parts in graph?
bbop.model.graph.prototype.is_complete = function(){
    var retval = true;
    if( this.all_dangling().length > 0 ){
	retval = false;
    }
    return retval;
};


// Return a copy of a node by id (not the original) if extant.
bbop.model.graph.prototype.get_node = function(nid){
    var retnode = null;
    if( this._nodes.hash[ nid ] ){
	var tmp_node = this._nodes.hash[ nid ];
	retnode = tmp_node.clone();
    }
    return retnode;
};


// Get a new edge defined in the graph or null.
bbop.model.graph.prototype.get_edge = function(sub_id, obj_id, pred){	

    if( ! pred ){ pred = bbop.model.default_predicate; }

    var ret_edge = null;
    if( this._sop_table[sub_id] &&
	this._sop_table[sub_id][obj_id] &&
	this._sop_table[sub_id][obj_id][pred] ){
	    // retval = new bbop.model.edge(sub_id, obj_id, pred);
	    var tmp_edge = this._sop_table[sub_id][obj_id][pred];
	    ret_edge = tmp_edge.clone();
	}
    return ret_edge; 
};


// Return all edges of given subject and object ids. Returns
// entirely new edges.
bbop.model.graph.prototype.get_edges = function(sub_id, obj_id){
    var retlist = new Array();
    if( this._sop_table[sub_id] &&
	this._sop_table[sub_id][obj_id] ){
	    for( var pred in this._sop_table[sub_id][obj_id] ){
		retlist.push(new bbop.model.edge(sub_id, obj_id, pred));
	    }
	}		
    return retlist;
};


// Translate an edge array into extant bodies, switching on either
// subject or object.
bbop.model.graph.prototype.edges_to_nodes = function(in_edges, target){
    
    // Double check.
    if( target != 'subject' && target != 'object'){
	throw new Error('Bad target for edges to bodies.');
    }

    // 
    var results = new Array();
    for( var i = 0; i < in_edges.length; i++ ){ 
	var in_e = in_edges[i];

	// Switch between subject and object.
	if( target == 'subject' ){
	    var target_id = in_e.subject_id();
	}else{
	    var target_id = in_e.object_id();
	}
	
	//
	if( target_id && this._nodes.hash[ target_id ] ){
	    results.push(this._nodes.hash[ target_id ]);
	}else{
	    throw new Error(target + ' world issue');
	}
    }
    return results;
};


// Roots are defined as nodes who are the subject of nothing,
// independent of predicate.
bbop.model.graph.prototype.is_root_node = function(nb_id){
    var result = false;	
    if( this._nodes.hash[ nb_id ] &&
	! this._subjects.hash[ nb_id ] ){	    
	    result = true;
	}
    return result;
};


// BUG/TODO: Could I speed this up by my moving some of the
// calculation into the add_node and add_edge methods?
// O(|#nodes|)
bbop.model.graph.prototype.get_root_nodes = function(){
    var results = new Array();
    for( var nb_id in this._nodes.hash ){
	if( this.is_root_node(nb_id) ){
	    results.push( this.get_node(nb_id).clone() );
	}
    }
    return results;
};


// Leaves are defined as nodes who are the object of nothing,
// independent of predicate.
bbop.model.graph.prototype.is_leaf_node = function(nb_id){

    var result = false;
    if( this._nodes.hash[ nb_id ] &&
	! this._objects.hash[ nb_id ] ){	    
	    result = true;
	}
    return result;
};


// BUG/TODO: Could I speed this up by my moving some of the
// calculation into the add_node and add_edge methods?
// O(|#nodes|)
bbop.model.graph.prototype.get_leaf_nodes = function(){
    var results = new Array();
    for( var nb_id in this._nodes.hash ){
	if( this.is_leaf_node(nb_id) ){
	    results.push( this.get_node(nb_id).clone() );
	}
    }
    return results;
};


// Nodes that are roots and leaves over all relations.
bbop.model.graph.prototype.get_singleton_nodes = function(){
    // Translate array into array extant bodies.
    var singleton_array = new Array();
    for( var singleton_id in this._is_a_singleton_lookup ){
	if( this._nodes.hash[ singleton_id ] ){
	    singleton_array.push( this._nodes.hash[ singleton_id ] );
	}else{
	    throw new Error("world issue in get_singletons: "+singleton_id);
	}
    }
    return singleton_array;
};


// Return all parent edges. If no predicate is give, use the
// default one.
// TODO: it might be nice to memoize this since others depend on it.
bbop.model.graph.prototype.get_parent_edges = function(nb_id, in_pred){

    var results = new Array();

    // Get all parents, or just parents from a specific relation.
    var preds_to_use = new Array();
    if( in_pred ){
	preds_to_use.push(in_pred);
    }else{
	preds_to_use = this._predicates.array;
    }

    // Try all of our desired predicates.
    for( var j = 0; j < preds_to_use.length; j++ ){
	var pred = preds_to_use[j];

	// Scan the table for goodies; there really shouldn't be a
	// lot here.
	if( this._so_table[ nb_id ] ){		
	    for( var obj_id in this._so_table[nb_id] ){
		// If it looks like something is there, try to see
		// if there is an edge for our current pred.
		var tmp_edge = this.get_edge(nb_id, obj_id, pred);
		if( tmp_edge ){
		    results.push( tmp_edge );
		}
	    }
	}
    }
    return results;
};


// Return all parents nodes. If no predicate is given, use the
// default one.
bbop.model.graph.prototype.get_parent_nodes = function(nb_id, in_pred){

    var results = new Array();
    var edges = this.get_parent_edges(nb_id, in_pred);
    for( var i = 0; i < edges.length; i++ ){
	// Make sure that any found edges are in our
	// world.
	var obj_id = edges[i].object_id();
	var tmp_node = this.get_node(obj_id);
	if( tmp_node ){
	    results.push( this.get_node(obj_id) );
	}
    }
    return results;
};


// Return all children. If no predicate is given, use the default
// one.
bbop.model.graph.prototype.get_child_nodes = function(nb_id, in_pred){

    var results = new Array();

    // Get all children, or just children from a specific relation.
    var preds_to_use = new Array();
    if( in_pred ){
	preds_to_use.push(in_pred);
    }else{
	preds_to_use = this._predicates.array;
    }

    // Try all of our desired predicates.
    for( var j = 0; j < preds_to_use.length; j++ ){
	var pred = preds_to_use[j];

	// Scan the table for goodies; there really shouldn't be a
	// lot here.
	if( this._os_table[ nb_id ] ){		
	    for( var sub_id in this._os_table[nb_id] ){
		// If it looks like something is there, try to see
		// if there is an edge for our current pred.
		if( this.get_edge(sub_id, nb_id, pred) ){
		    // Make sure that any found edges are in our
		    // world.
		    var tmp_node = this.get_node(sub_id);
		    if( tmp_node ){
			results.push( this.get_node(sub_id) );
		    }
		}
	    }
	}
    }
    return results;
};


// Return new ancestors subgraph. Single id or id list as first
// argument. Predicate string/id as optional second.
bbop.model.graph.prototype.get_ancestor_subgraph = function(nb_id_or_list, pid){

    // Shared data structure to trim multiple paths.
    // Nodes: color to get through the graph quickly and w/o cycles.
    var seen_node_hash = {};
    // Edges: just listed--hashing would be essentially the same
    // as a call to graph.add_edge (I think--benchmark?).
    var seen_edge_list = [];
    var anchor = this;

    // Define recursive ascent.
    function rec_up(nid){

	//print('rec_up on: ' + nid);

    	var results = new Array();
    	var new_parent_edges = anchor.get_parent_edges(nid, pid);

	// Capture edge list for later adding.
	for( var e = 0; e < new_parent_edges.length; e++ ){
	    seen_edge_list.push(new_parent_edges[e]);
	}

	// Pull extant nodes from edges. NOTE: This is a retread
	// of what happens in get_parent_nodes to avoid another
	// call to get_parent_edges (as all this is now
	// implemented).
	var new_parents = new Array();
	for( var n = 0; n < new_parent_edges.length; n++ ){
	    // Make sure that any found edges are in our
	    // world.
	    var obj_id = new_parent_edges[n].object_id();
	    var tmp_node = anchor.get_node(obj_id);
	    if( tmp_node ){
		new_parents.push( tmp_node );
	    }
	}

	// Make sure we're in there too.
	var tmp_node = anchor.get_node(nid);
	if( tmp_node ){
	    new_parents.push( tmp_node );
	}

	// Recur on unseen things and mark the current as seen.
    	if( new_parents.length != 0 ){
    	    for( var i = 0; i < new_parents.length; i++ ){
    		// Only do things we haven't ever seen before.
    		var new_anc = new_parents[i];
    		var new_anc_id = new_anc.id();
    		if( ! seen_node_hash[ new_anc_id ] ){
    		    seen_node_hash[ new_anc_id ] = new_anc;
    		    rec_up(new_anc_id);	
    		}
    	    }
    	}
    	return results;
    }
    
    // Recursive call and collect data from search. Make multiple
    // ids possible.
    if( nb_id_or_list.length && nb_id_or_list.index ){
	for( var l = 0; l < nb_id_or_list.length; l++ ){	    
	    rec_up(nb_id_or_list[l]);
	}
    }else{
    	rec_up(nb_id_or_list);
    }
    
    // Build new graph using data.
    var new_graph = new bbop.model.graph();
    for( var k in seen_node_hash ){
	new_graph.add_node(seen_node_hash[k]);
    }
    for( var x = 0; x < seen_edge_list.length; x++ ){	    
	new_graph.add_edge(seen_edge_list[x]);
    }

    return new_graph;
};