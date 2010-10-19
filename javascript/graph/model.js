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
//// Taken name spaces:
////    bbop.model.*
////
//////////


// Module and namespace checking.
if ( typeof bbop == "undefined" ){ bbop = {}; }
if ( typeof bbop.model == "undefined" ){ bbop.model = {}; }


// 
bbop.model.default_predicate = 'is_a';


///
///  Node sub-object.
///

bbop.model.node = function(new_id){
    this._id = new_id;
    this._type = 'node';
    this._metadata = undefined;
    this._label = undefined;
};

// Prototype getter/setters.
bbop.model.node.prototype.id = function(value){
    if(value) this._id = value; return this._id; };
bbop.model.node.prototype.type = function(value){
    if(value) this._type = value; return this._type; };
bbop.model.node.prototype.metadata = function(value){
    if(value) this._metadata = value; return this._metadata; };
bbop.model.node.prototype.label = function(value){
    if(value) this._label = value; return this._label; };


///
///  Edge sub-object.
///

// While edges can have ids, they are not required (or internally
// generated).
// If no predicate is given, default. Predicates are currently treated
// as raw strings.
bbop.model.edge = function(subject, object, predicate){
    this._id = undefined;
    this._type = 'edge';
    if( typeof subject == 'string' ){
	this._subject_id = subject;	
    }else{
	this._subject_id = subject.id();
    }
    if( typeof object == 'string' ){
	this._object_id = object;	
    }else{
	this._object_id = object.id();
    }
    this._predicate_id = bbop.model.default_predicate;
    if( predicate ){
	this._predicate_id = predicate;
    }
};

// Prototype getter/setters.
bbop.model.edge.prototype.id = function(value){
    if(value) this._id = value; return this._id; };
bbop.model.node.prototype.type = function(value){
    if(value) this._type = value; return this._type; };
bbop.model.edge.prototype.subject_id = function(){
    return this._subject_id; };
bbop.model.edge.prototype.object_id = function(){
    return this._object_id; };
bbop.model.edge.prototype.predicate_id = function(){
    return this._predicate_id; };


///
///  Graph sub-object.
///

// TODO: make compilation piecewise with every added node and edge.
bbop.model.graph = function(){

    var id = undefined;

    ///
    /// Data structures to regenerate from an ID.
    ///

    // TODO: why again isn't it possible?
    // For bbop.model.node and bbop.model.edge (hash not possible for
    // edges).
    var nodes = { array: new Array, hash: {} };
    var named_nodes = { array: new Array, hash: {} };
    var edges = { array: new Array };

    // For bbop.model.edge, bbop.model.edge, and bbop.model.edge.
    var predicates = { array: new Array, hash: {} };
    var subjects = { array: new Array, hash: {} };
    var objects = { array: new Array, hash: {} };     

    // Table structures for quick lookups of relations.
    var predicate_subject_table = {};    // [pred][sub] -> bbop.model.edge.
    var subject_predicate_table = {};    // [sub][pred] -> bbop.model.edge.
    var predicate_object_table = {};     // [pred][obj] -> sub data struct.
    var object_predicate_table = {};     // [obj][pred] -> sub data struct.

    // New parallel structures to for our simplified graph.
    var predicate_table = {};
    var subject_table = {};
    var object_table = {};

    // Table structures for quick lookups of node properties.
    var is_a_singleton_lookup = {}; // [nid] -> bbop.model.node.


    // id getter/setter.
    this.id = function(value){ if( value ) id = value; return id; };
    
    
    //
    this.add_node = function(node){

	// Check for for anything funny.
	if( ! node.id() || nodes.hash[ node.id() ] || nodes.hash[ node.id() ] ){
	    //alert("tried to add same node: " + node.id());
	    throw new Error("tried to add same node: " + node.id());
	}else{

	    var nid = node.id();

	    // Add it to all the concerned recall data structures.
	    nodes.hash[ nid ] = node;
	    nodes.array.push(node);
	    named_nodes.hash[ nid ] = node;
	    named_nodes.array.push(node);

	    // If this does not belong to any relation so far, then it is a
	    // singleton.
	    if( ! subjects.hash[ nid ] && ! objects.hash[ nid ] ){
		is_a_singleton_lookup[ nid ] = true;
	    }
	}
    };


    //
    this.add_edge = function(edge){

	//
	var sub_id = edge.subject_id();
	var obj_id = edge.object_id();
	var pred_id = edge.predicate_id();

	///
	/// New way of trying things:
	///

	// Subject.
	if( ! subject_table[ sub_id ] ){ subject_table[ sub_id ] = {}; }
	subject_table[ sub_id ][ obj_id ] = true;
	// Object.
	if( ! object_table[ obj_id ] ){ object_table[ obj_id ] = {}; }
	object_table[ obj_id ][ sub_id ] = true;
	// Their relationship (defined by SOP).
	if( ! predicate_table[ sub_id ] ){
	    predicate_table[ sub_id ] = {}; }
	if( ! predicate_table[ sub_id ][ obj_id ] ){
	    predicate_table[ sub_id ][obj_id] = {}; }
	predicate_table[ sub_id ][ obj_id ][ pred_id ] = true;

	///
	/// Add edge to all predicate-first data structures. This
	/// section will be heavily commented, while the next two
	/// analogous sections will be crammed together.
	///

	// If this is a new predicate add it to all of the necessary data
	// structures.
	if( ! predicates.hash[ pred_id ] ){
	    
	    // 
	    predicates.hash[ pred_id ] = true; 
	    predicates.array.push(pred_id); 

	    //
	    predicate_subject_table[ pred_id ] = {};
	    predicate_object_table[ pred_id ] = {};
	}

	// Add the edge to the predicate/subject table if it doesn't
	// exist. Every predicate subject combination can have a
	// variable number of objects.
	if( ! predicate_subject_table[ pred_id ][ sub_id ] ){
	    // predicate_subject_table[ pred_id ][ sub_id ] = edge;
	    predicate_subject_table[ pred_id ][ sub_id ] = {};
	    predicate_subject_table[ pred_id ][ sub_id ].object =
		{
		    array: new Array,
		    hash: {}
		};
	}
	// Actually add the new item in. 
	(function(){
	     var tmp_obj = predicate_subject_table[ pred_id ][ sub_id ].object;
	     if( ! tmp_obj.hash[ obj_id ] ){
		 tmp_obj.hash[ obj_id ] = edge;
		 tmp_obj.array.push(edge);
	     }
	 })();

	// Add the edge to the predicate/object table if it doesn't
	// exist. Every predicate object combination can have unlimited
	// subjects, so add them into the hash and array stored in the
	// table.
	//
	// Make sure we have a hash at the end of it.
	if( ! predicate_object_table[ pred_id ][ obj_id ] ){
	    predicate_object_table[ pred_id ][ obj_id ] = {};
	    predicate_object_table[ pred_id ][ obj_id ].subject =
		{
		    array: new Array,
		    hash: {}
		};
	}
	// Actually add the new item in. 
	(function(){
	     var tmp_sub = predicate_object_table[ pred_id ][ obj_id ].subject;
	     if( ! tmp_sub.hash[ sub_id ] ){
		 tmp_sub.hash[ sub_id ] = edge;
		 tmp_sub.array.push(edge);
	     }
	 })();

	///
	/// Next, add to all subject-first data structures.
	///

	if( ! subjects.hash[ sub_id ] ){
	    subjects.hash[ sub_id ] = true; 
	    subjects.array.push(sub_id); 
	    subject_predicate_table[ sub_id ] = {};
	}
	if( ! subject_predicate_table[ sub_id ][ pred_id ] ){
	    // subject_predicate_table[ sub_id ][ pred_id ] = edge;
	    // // edge_have_new_sub_p = true;
	    subject_predicate_table[ sub_id ][ pred_id ] = {};
	    subject_predicate_table[ sub_id ][ pred_id ].object =
		{
		    array: new Array,
		    hash: {}
		};
	}
	// Now that it is ensured, actually add new item in.
	(function(){
	     var tmp_obj = subject_predicate_table[ sub_id ][ pred_id ].object;
	     if( ! tmp_obj.hash[ obj_id ] ){
		 tmp_obj.hash[ obj_id ] = edge;
		 tmp_obj.array.push(edge);
	     }
	 })();	     

	///
	/// Next, add to all object-first data structures.
	///

	if( ! objects.hash[ obj_id ] ){
	    objects.hash[ obj_id ] = true; 
	    objects.array.push(obj_id); 
	    object_predicate_table[ obj_id ] = {};
	}
	if( ! object_predicate_table[ obj_id ][ pred_id ] ){
	    object_predicate_table[ obj_id ][ pred_id ] = {};
	    object_predicate_table[ obj_id ][ pred_id ].subject =
		{
		    array: new Array,
		    hash: {}
		};
	}
	// Now that it is ensured, actually add new item in.
	(function(){
	     var tmp_sub = object_predicate_table[ obj_id ][ pred_id ].subject;
	     if( ! tmp_sub.hash[ sub_id ] ){
		 tmp_sub.hash[ sub_id ] = edge;
		 tmp_sub.array.push(edge);
	     }
	 })();

	///
	/// Finally, remove the subject and objects from all appropriate
	/// "is_a_" tables (which were added during the add_node menthod).
	///   

	// Remove the edge's subject and object from the singleton table.
	if( is_a_singleton_lookup[ sub_id ] ){
	    delete is_a_singleton_lookup[ sub_id ]; }
	if( is_a_singleton_lookup[ obj_id ] ){
	    delete is_a_singleton_lookup[ obj_id ]; }

	// Onto the array and subject and object into named bodies.
	edges.array.push(edge);
	if( ! named_nodes.hash[ sub_id ] ){
	    named_nodes.array.push(sub_id); }
	named_nodes.hash[ sub_id ] = edge;
	if( ! named_nodes.hash[ obj_id ] ){
	    named_nodes.array.push(obj_id); }
	named_nodes.hash[ obj_id ] = edge;
    };
    

    //
    this.get_node = function(nid){
	var returnval = null;
	if( nodes.hash[ nid ] ){
	    returnval = nodes.hash[ nid ]; }
	return returnval;
    };


    //
    this.get_nodes = function(){
	return nodes.array;
    };


    // This depends on the user giving edges ids.
    this.get_edge = function(lid){
	var returnval = null;
	if( edges.hash[ lid ] ){
	    returnval = edges.hash[ lid ]; }
	return returnval;
    };


    //
    this.get_edge_by_sop = function(sub_id, obj_id, pred){

	if( ! pred ){ pred = bbop.model.default_predicate; }
	var returnval = null;
	
	if( predicate_object_table[ pred ] &&
	    predicate_object_table[ pred ][ obj_id ] &&
	    predicate_object_table[ pred ][ obj_id ].subject &&
	    predicate_object_table[ pred ][ obj_id ].subject.hash &&
	    predicate_object_table[ pred ][ obj_id ].subject.hash[ sub_id ] ){
		returnval =
		    predicate_object_table[pred][obj_id].subject.hash[sub_id];
	    }
	
	return returnval;
    }; 


    // // TODO: will need for creating subgraphs.
    // // Return a list of edges from subject and object id.
    // this.get_edges_by_so = function(sub_id, obj_id){

    // 	if( ! pred ){ pred = bbop.model.default_predicate; }
    // 	var returnval = null;
	
    // 	if( predicate_object_table[ pred ] &&
    // 	    predicate_object_table[ pred ][ obj_id ] &&
    // 	    predicate_object_table[ pred ][ obj_id ].subject &&
    // 	    predicate_object_table[ pred ][ obj_id ].subject.hash &&
    // 	    predicate_object_table[ pred ][ obj_id ].subject.hash[ sub_id ] ){
    // 		returnval =
    // 		    predicate_object_table[pred][obj_id].subject.hash[sub_id];
    // 	    }
	
    // 	return returnval;
    // }; 


    //
    this.get_edges = function(){
	return edges.array;
    };


    //
    this.get_predicates = function(){
	return predicates.array;
    };


    // Translate an edge array into extant bodies, switching on either
    // subject ot object.
    this.edges_to_nodes = function(result_edges, target){
	
	// Double check.
	if( target != 'subject' && target != 'object'){
	    throw new Error('bad target for edges to bodies');
	}

	// 
	var results = new Array();
	for( var i = 0; i < result_edges.length; i++ ){ 
	    var re_i = result_edges[i];

	    if( re_i ){
		// Switch between subject and object.
		if( target == 'subject' ){
		    var target_id = re_i.subject_id();
		}else{
		    var target_id = re_i.object_id();
		}

		//
		if( target_id && nodes.hash[ target_id ] ){
		    results.push(nodes.hash[ target_id ]);
		}else{
		    throw new Error(target + ' world issue');
		}
	    }else{
		throw new Error('sanity issue');
	    }
	}
	return results;
    };

    //
    this.is_root = function(nb_id, pred){

	if( ! pred ){ pred = bbop.model.default_predicate; }
	var result = false;
	
	// After making sure that our arguments are sanely defined, tick
	// off the definitions of whether a node is root over a relation:
	// 1) not a subject to anything, 2) the predicate doesn't exist,
	// 3) is not a subject of the predicate.
	if( named_nodes.hash[ nb_id ] && pred &&
	    ( ! subjects.hash[ nb_id ] ||
	      ! predicate_subject_table[ pred ] ||
	      ! predicate_subject_table[ pred ][ nb_id ] )
	  ){
	      result = true;
	  }
	return result;
    };

    
    // TODO: Could I speed this up by my moving some of the calculation
    // into the add_node and add_edge methods?
    this.get_root_nodes = function(pred){

	if( ! pred ){ pred = bbop.model.default_predicate; }
	var results = new Array;

	for( var nb_id in named_nodes.hash ){

	    // After making sure that our arguments are sanely defined, tick
	    // off the definitions of whether a node is root over a
	    // relation: 1) not a subject to anything, 2) the
	    // predicate doesn't exist, 3) is not a subject of the
	    // predicate.
	    if( named_nodes.hash[ nb_id ] && pred &&
		( ! subjects.hash[ nb_id ] ||
		  ! predicate_subject_table[ pred ] ||
		  ! predicate_subject_table[ pred ][ nb_id ] ) ){

		      if( nodes.hash[ nb_id ] ){
			  results.push( nodes.hash[ nb_id ] );
		      }else{
			  // BUG: while this should be the correct
			  // behavior, there are some cases where due
			  // to part_of children, the referenced node
			  // will be a singleton is_a root that
			  // doesn't have an extant body.
			  //throw new Error("world issue in get_roots: "+nb_id);
		      }
		  }
	}
	
	return results;
    };


    //
    this.is_leaf = function(nb_id, pred){

	if( ! pred ){ pred = bbop.model.default_predicate; }
	var result = false;
	
	// After making sure that our arguments are sanely defined, tick
	// off the definitions of whether a node is leaf over a relation:
	// 1) not an object to anything, 2) the predicate doesn't exist,
	// 3) is not an object of the predicate.
	if( named_nodes.hash[ nb_id ] && pred &&
	    ( ! objects.hash[ nb_id ] ||
	      ! predicate_object_table[ pred ] ||
	      ! predicate_object_table[ pred ][ nb_id ] )
	  ){
	      result = true;
	  }	
	return result;
    };


    // TODO: Could I speed this up by my moving some of the calculation
    // into the add_node and add_edge methods?
    this.get_leaf_nodes = function(pred){
	
	if( ! pred ){ pred = bbop.model.default_predicate; }
	var leaves_array = new Array;

	for( var nb_id in named_nodes.hash ){

	    // After making sure that our arguments are sanely defined, tick
	    // off the definitions of whether a node is leaf over a relation:
	    // 1) not an object to anything, 2) the predicate doesn't exist,
	    // 3) is not an object of the predicate.
	    if( named_nodes.hash[ nb_id ] && pred &&
		( ! objects.hash[ nb_id ] ||
		  ! predicate_object_table[ pred ] ||
		  ! predicate_object_table[ pred ][ nb_id ] )
	      ){		  
		  if( nodes.hash[ nb_id ] ){
		      leaves_array.push( nodes.hash[ nb_id ] );
		  }else{
		      throw new Error("world issue in get_leaf_nodes: "+nb_id);
		  }
	      }
	}

	return leaves_array;
    };


    // Nodes that are roots and leaves over all relations.
    this.get_singleton_nodes = function(){

	var singleton_array = new Array;

	// Translate array into array extant bodies.
	for( var singleton_id in is_a_singleton_lookup ){
	    if( nodes.hash[ singleton_id ] ){
		singleton_array.push( nodes.hash[ singleton_id ] );
	    }else{
		throw new Error("world issue in get_singletons: "+singleton_id);
	    }
	}

	return singleton_array;
    };


    // Return all parents.
    this.get_parent_nodes = function(nb_id, pred_id){

	if( ! pred_id ){ pred_id = bbop.model.default_predicate; }
	var results = new Array;

	// Get all parents, or just parents from a specific relation.
	var preds = new Array;
	if( pred_id ){
	    preds.push(pred_id);
	}else{
	    preds = predicates.array;
	}

	for( var j = 0; j < preds.length; j++ ){
	    var pred = preds[j];

	    if( named_nodes.hash[ nb_id ] &&
		predicate_subject_table[ pred ] &&
		predicate_subject_table[ pred ][ nb_id ] &&
		predicate_subject_table[ pred ][ nb_id ].object.array &&
		predicate_subject_table[pred][nb_id].object.array.length > 0 ){
		    
		    // Translate result edges into extant bodies.
		    var result_edges =
			predicate_subject_table[ pred ][ nb_id ].object.array;
		    results =
			this.edges_to_nodes(result_edges, 'object');
		}
	}
	
	return results;
    };
    

    // Return all children.
    this.get_child_nodes = function(nb_id, pred_id){

	if( ! pred_id ){ pred_id = bbop.model.default_predicate; }
	var results = new Array;

	// Get all children, or just children from a specific relation.
	var preds = new Array;
	if( pred_id ){
	    preds.push(pred_id);
	}else{
	    preds = predicates.array;
	}

	for( var j = 0; j < preds.length; j++ ){
	    var pred = preds[j];

	    if( named_nodes.hash[ nb_id ] &&
		predicate_object_table[ pred ] &&
		predicate_object_table[ pred ][ nb_id ] &&
		predicate_object_table[ pred ][ nb_id ].subject.array &&
		predicate_object_table[pred][nb_id].subject.array.length > 0 ){
		    
		    var result_edges =
			predicate_object_table[ pred ][ nb_id ].subject.array;
		    results =
			this.edges_to_nodes(result_edges, 'subject');
		}
	}
	
	return results;
    };
    

    // Return all ancestors by id--work up to the roots and return an
    // id
    // list.
    this.get_ancestor_list = function(nb_id, pred_id){

	if( ! pred_id ){ pred_id = bbop.model.default_predicate; }

	// Shared data structure to trim multiple paths.
	var seen_hash = {};
	var anchor = this;

	// Define recursive ascent.
	function rec_up(nid){

	    var results = new Array;
	    var new_parents = anchor.get_parent_nodes(nid);

	    if( new_parents.length != 0 ){
		for( var i = 0; i < new_parents.length; i++ ){
		    // Only do things we haven't ever seen before.
		    var new_anc = new_parents[i].id();
		    if( ! seen_hash[ new_anc ] ){
			seen_hash[ new_anc ] = true;
			rec_up(new_anc);			
		    }
		}
	    }
	    return results;
	}
	
	// Recursive call and collect keys from seen_hash.
	rec_up(nb_id);
	var all_anc = []; for (var k in seen_hash) all_anc.push(k);
	return all_anc;
    };
    

    // TODO: Allow us to either get all dangling edges, or just the
    // dangling edges asscoiated with an id.
    //this.get_dangling = function(){
    //  // disjoint of named and extant
    //}


    // //
    // this.get_extant_body = function(ebid){
    // 	var returnval = null;
    // 	if( nodes.hash[ nbid ] ){
    // 	    returnval = nodes.hash[ nbid ]; }
    // 	return returnval;
    // };


    // // Returns an array of named bodies.
    // this.get_nodes = function(){
    // 	return nodes.array;
    // };


    // // List all ids for children seen, but not accounted for.
    // this.missing_children = function(nb_id, pred_id){

    // 	if( ! pred_id ){ pred_id = bbop.model.default_predicate; }
    // 	var results = new Array;

    // 	// Get all children, or just children from a specific relation.
    // 	var preds = new Array;
    // 	if( pred_id ){
    // 	    preds.push(pred_id);
    // 	}else{
    // 	    preds = predicates.array;
    // 	}

    // 	for( var j = 0; j < preds.length; j++ ){
    // 	    var pred = preds[j];

    // 	    if( named_nodes.hash[ nb_id ] &&
    // 		predicate_object_table[ pred ] &&
    // 		predicate_object_table[ pred ][ nb_id ] &&
    // 		predicate_object_table[ pred ][ nb_id ].subject.array &&
    // 		predicate_object_table[pred][nb_id].subject.array.length > 0 ){
		    
    // 		    var result_edges =
    // 			predicate_object_table[ pred ][ nb_id ].subject.array;
		    
    // 		    // Translate result edges into extant bodies.
    // 		    for( var i = 0; i < result_edges.length; i++ ){
			
    // 			if( result_edges[i] &&
    // 			    result_edges[i].subject_id() ){

    // 			    // Missing child.
    // 			    var rei_sid = result_edges[i].subject_id();
    // 			    if( ! nodes.hash[ rei_sid ] ){
    // 				results.push( rei_sid );
    // 			    }

    // 			}else{
    // 			    throw new Error("world issue in missing_children");
    // 			}
    // 		    }
    // 		}
    // 	}
	
    // 	return results;
    // };
    
};
