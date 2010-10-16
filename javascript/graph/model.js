////////////
////
//// bbop.model
////
//// Purpose: Basic edged graph and operations.
//// 
//// TODO: Ideally, The only objects externally available should be:
//// Model, Graph, Node, and Link.
////
//// NOTE: A model instance may not the whole graph, just a
//// subgraph. Given this fact, part of the model needs to be aware of
//// whether a node is a leaf over a given relation. Dangling edges
//// indicate that there is more.
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

    // TODO: why again isn;t it possible?
    // For bbop.model.node and bbop.model.edge (hash not possible for
    // edges).
    var nodes = { array: new Array, hash: {} };
    var edges = { array: new Array };

    // For bbop.model.edge or bbop.model.node and bbop.model.edge or
    // bbop.model.node.
    var extant_bodies = { array: new Array, hash: {} };
    var named_bodies = { array: new Array, hash: {} };

    // For bbop.model.edge, bbop.model.edge, and bbop.model.edge.
    var predicates = { array: new Array, hash: {} };
    var subjects = { array: new Array, hash: {} };
    var objects = { array: new Array, hash: {} };     

    // Table structures for quick lookups of relations.
    var predicate_subject_table = {};    // [pred][sub] -> bbop.model.edge.
    var subject_predicate_table = {};    // [sub][pred] -> bbop.model.edge.
    var predicate_object_table = {};     // [pred][obj] -> sub data struct.
    var object_predicate_table = {};     // [obj][pred] -> sub data struct.

    // Table structures for quick lookups of node properties.
    var is_a_singleton_lookup = {}; // [nid] -> bbop.model.node.


    // id getter/setter.
    this.id = function(value){ if( value ) id = value; return id; };
    
    
    //
    this.add_node = function(node){

	// Check for for anything funny.
	if( ! node.id() ||
	    extant_bodies.hash[ node.id() ] ||
	    nodes.hash[ node.id() ] ){
		//alert("tried to add same node: " + node.id());
		throw new Error("tried to add same node: " + node.id());
	    }else{

		var nid = node.id();

		// Add it to all the concerned recall data structures.
		extant_bodies.hash[ nid ] = node;
		extant_bodies.array.push(node);
		named_bodies.hash[ nid ] = node;
		named_bodies.array.push(node);
		nodes.hash[ nid ] = node;
		nodes.array.push(node);

		// If this does not belong to any relation so far, then it is a
		// singleton.
		if( ! subjects.hash[ nid ] &&
		    ! objects.hash[ nid ] ){
			is_a_singleton_lookup[ nid ] = true;
		    }
	    }
    };


    //
    this.add_edge = function(edge){

	//
	//var edge_have_new_pred_p = false;
	//var edge_have_new_sub_p = false;
	//var edge_have_new_obj_p = false;

	var pred_id = edge.predicate_id();
	var sub_id = edge.subject_id();
	var obj_id = edge.object_id();

	//
	// First, add to the named bodies if appropriate.
	//

	// Check on whether a pure id or named body.
	if( edge.id() &&
	    extant_bodies.hash[ edge.id() ] ){
		//alert("tried to add same (extant) edge: " + edge.id());
		throw new Error("tried to add same (extant) edge: "+ edge.id());
	    }else if( edge.id() ){
		// Add the id to the extant and named bodies if appropriate.
		extant_bodies.hash[ edge.id() ] = edge;
		extant_bodies.array.push(edge);
		if( ! named_bodies.hash[ edge.id() ] ){
		    named_bodies.array.push(edge); }
		named_bodies.hash[ edge.id() ] = edge;
	    }

	///
	/// Next, Add edge to all predicate-first data
	/// structures. This section will be heavily commented, while
	/// the next two analogous sections will be crammed together.
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

	    //
	    //edge_have_new_pred_p = true;
	}

	// Add the edge to the predicate/subject table if it doesn't
	// exist. Every predicate subject combination can have at most one
	// object--we'll nuke anything in our way.
	if( ! predicate_subject_table[ pred_id ][ sub_id ] ){
	    predicate_subject_table[ pred_id ][ sub_id ] = edge;
	}

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
	// 
	if( ! predicate_object_table[ pred_id ][ obj_id ].subject.hash[ sub_id ] ){
	    predicate_object_table[ pred_id ][ obj_id ].subject.hash[ sub_id ] = edge;
	    predicate_object_table[ pred_id ][ obj_id ].subject.array.push(edge);
	}

	///
	/// Next, add to all subject-first data structures.
	///

	if( ! subjects.hash[ sub_id ] ){
	    subjects.hash[ sub_id ] = true; 
	    subjects.array.push(sub_id); 
	    subject_predicate_table[ sub_id ] = {};
	}
	if( ! subject_predicate_table[ sub_id ][ pred_id ] ){
	    subject_predicate_table[ sub_id ][ pred_id ] = edge;
	    //edge_have_new_sub_p = true;
	}

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
	    object_predicate_table[ obj_id ][ pred_id ].subject = { array: new Array,
								    hash: {} };
	    //edge_have_new_obj_p = true;
	}
	if( ! object_predicate_table[ obj_id ][ pred_id ].subject.hash[ sub_id ] ){
	    object_predicate_table[ obj_id ][ pred_id ].subject.hash[ sub_id ] = edge;
	    object_predicate_table[ obj_id ][ pred_id ].subject.array.push(edge);
	}

	///
	/// Finally, remove the subject and objects from all appropriate
	/// 'is_a' tables (which were added during the add_node menthod).
	///   

	// Remove the edge's subject and object from the singleton table.
	if( is_a_singleton_lookup[ sub_id ] ){
	    delete is_a_singleton_lookup[ sub_id ]; }
	if( is_a_singleton_lookup[ obj_id ] ){
	    delete is_a_singleton_lookup[ obj_id ]; }

	// TODO: this was stupid--it's easy to have a situation where
	// there was a edge that had the same sub, and there was a edge
	// that had the same obj, which is what this is really asking. And
	// frankly, what do I care if we readd an anonymous edge?  Final
	// error check--this had better be unique.  if( !
	// edge_have_new_pred_p && ! edge_have_new_obj_p && !
	// edge_have_new_sub_p ){ //alert("tried to add same edge: " +
	// pred_id + ' ' + sub_id+ ' ' + obj_id); throw new Error("tried
	// to add same edge: " + pred_id + ' ' + sub_id+ ' ' + obj_id);
	// }else{

	// Onto the array and subject and object into named bodies.
	edges.array.push(edge);
	if( ! named_bodies.hash[ sub_id ] ){
	    named_bodies.array.push(sub_id); }
	named_bodies.hash[ sub_id ] = edge;
	if( ! named_bodies.hash[ obj_id ] ){
	    named_bodies.array.push(obj_id); }
	named_bodies.hash[ obj_id ] = edge;
	//}
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


    //
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
		    predicate_object_table[ pred ][ obj_id ].subject.hash[ sub_id ];
	    }
	
	return returnval;
    }; 


    //
    this.get_edges = function(){
	return edges.array;
    };


    //
    this.get_extant_body = function(ebid){
	var returnval = null;
	if( extant_bodies.hash[ nbid ] ){
	    returnval = extant_bodies.hash[ nbid ]; }
	return returnval;
    };


    // Returns an array of named bodies.
    this.get_extant_bodies = function(){
	return extant_bodies.array;
    };


    //
    this.get_predicates = function(){
	return predicates.array;
    };


    //
    this.is_root = function(nb_id, pred){

	if( ! pred ){ pred = bbop.model.default_predicate; }
	var result = false;
	
	// After making sure that our arguments are sanely defined, tick
	// off the definitions of whether a node is root over a relation:
	// 1) not a subject to anything, 2) the predicate doesn't exist,
	// 3) is not a subject of the predicate.
	if( named_bodies.hash[ nb_id ] && pred &&
	    ( ! subjects.hash[ nb_id ] ||
	      ! predicate_subject_table[ pred ] ||
	      ! predicate_subject_table[ pred ][ nb_id ] ) ){
		  result = true;
	      }
	return result;
    };

    
    // TODO: Could I speed this up by my moving some of the calculation
    // into the add_node and add_edge methods?
    this.get_roots = function(pred){

	if( ! pred ){ pred = bbop.model.default_predicate; }
	var results = new Array;

	for( var nb_id in named_bodies.hash ){

	    // After making sure that our arguments are sanely defined, tick
	    // off the definitions of whether a node is root over a relation:
	    // 1) not a subject to anything, 2) the predicate doesn't exist,
	    // 3) is not a subject of the predicate.
	    if( named_bodies.hash[ nb_id ] && pred &&
		( ! subjects.hash[ nb_id ] ||
		  ! predicate_subject_table[ pred ] ||
		  ! predicate_subject_table[ pred ][ nb_id ] ) ){

		      if( extant_bodies.hash[ nb_id ] ){
			  results.push( extant_bodies.hash[ nb_id ] );
		      }else{
			  // BUG: while this should be the correct behavior, there are
			  // some cases where due to part_of children, the referenced
			  // node will be a singleton is_a root that doesn't have an
			  // extant body.
			  //throw new Error("world issue in get_roots: " + nb_id);
		      }
		  }
	}

	return results;
    };


    // Clips to extant nodes.
    //   // TODO: Could I speed this up by my moving some of the calculation
    //   // into the add_node and add_edge methods?
    //   this.get_root_nodes = function(pred){

    //     var results = new Array;

    //     for( var nb_id in nodes.hash ){

    //       // Tick off the deifinitions of whether a node is root over a
    //       // relation: 1) not a subject to anything, 2) the predicate
    //       // doesn't exist, 3) is not a subject of the predicate.
    //       if( ! subjects.hash[ nb_id ] ||
    // 	  ! predicate_subject_table[ pred ] ||
    // 	  ! predicate_subject_table[ pred ][ nb_id ] ){

    // 	// Nodes are in world by definition.
    // 	results.push( nodes.hash[ nb_id ] );
    //       }
    //     }

    //     return results;
    //   };


    //
    this.is_leaf = function(nb_id, pred){

	if( ! pred ){ pred = bbop.model.default_predicate; }
	var result = false;
	
	// After making sure that our arguments are sanely defined, tick
	// off the definitions of whether a node is leaf over a relation:
	// 1) not an object to anything, 2) the predicate doesn't exist,
	// 3) is not an object of the predicate.
	if( named_bodies.hash[ nb_id ] && pred &&
	    ( ! objects.hash[ nb_id ] ||
	      ! predicate_object_table[ pred ] ||
	      ! predicate_object_table[ pred ][ nb_id ] ) ){
		  result = true;
	      }
	
	return result;
    };


    // TODO: Could I speed this up by my moving some of the calculation
    // into the add_node and add_edge methods?
    this.get_leaves = function(pred){
	
	if( ! pred ){ pred = bbop.model.default_predicate; }
	var leaves_array = new Array;

	for( var nb_id in named_bodies.hash ){

	    // After making sure that our arguments are sanely defined, tick
	    // off the definitions of whether a node is leaf over a relation:
	    // 1) not an object to anything, 2) the predicate doesn't exist,
	    // 3) is not an object of the predicate.
	    if( named_bodies.hash[ nb_id ] && pred &&
		( ! objects.hash[ nb_id ] ||
		  ! predicate_object_table[ pred ] ||
		  ! predicate_object_table[ pred ][ nb_id ] ) ){
		      
		      if( extant_bodies.hash[ nb_id ] ){
			  leaves_array.push( extant_bodies.hash[ nb_id ] );
		      }else{
			  throw new Error("world issue in get_leaves: " + nb_id);
		      }
		  }
	}

	return leaves_array;
    };


    // Nodes that are roots and leaves over all relations.
    this.get_singletons = function(){

	var singleton_array = new Array;

	// Translate array into array extant bodies.
	for( var singleton_id in is_a_singleton_lookup ){
	    if( extant_bodies.hash[ singleton_id ] ){
		singleton_array.push( extant_bodies.hash[ singleton_id ] );
	    }else{
		throw new Error("world issue in get_singletons: " + singleton_id);
	    }
	}

	return singleton_array;
    };


    // TODO:
    //this.getDangling = function(){
    //  // disjoint of named and extant
    //}


    //
    this.get_parent = function(node_id, pred){

	if( ! pred ){ pred = bbop.model.default_predicate; }
	var result = null;
	
	if( named_bodies.hash[ node_id ] &&
	    predicate_subject_table[ pred ] &&
	    predicate_subject_table[ pred ][ node_id ] ){
		
		var found_edge = predicate_subject_table[ pred ][ node_id ];
		if( extant_bodies.hash[ found_edge.object_id() ] ){
		    result = extant_bodies.hash[ found_edge.object_id() ];
		}else{
		    throw new Error("world issues in get_parent: " +
				    found_edge.object_id());
		}
	    }

	return result;
    };


    // List all ids for children seen, but not accounted for.
    this.missing_children = function(nb_id, pred_id){

	if( ! pred_id ){ pred_id = bbop.model.default_predicate; }
	var results = new Array;

	// Get all children. or just children from a specific relation.
	var preds = new Array;
	if( pred_id ){
	    preds.push(pred_id);
	}else{
	    preds = predicates.array;
	}

	for( var j = 0; j < preds.length; j++ ){
	    var pred = preds[j];

	    if( named_bodies.hash[ nb_id ] &&
		predicate_object_table[ pred ] &&
		predicate_object_table[ pred ][ nb_id ] &&
		predicate_object_table[ pred ][ nb_id ].subject.array &&
		predicate_object_table[ pred ][ nb_id ].subject.array.length > 0 ){
		    
		    var result_edges =
			predicate_object_table[ pred ][ nb_id ].subject.array;
		    
		    // Translate result edges into extant bodies.
		    for( var i = 0; i < result_edges.length; i++ ){
			
			if( result_edges[i] &&
			    result_edges[i].subject_id() ){

			    // Missing child.
			    if( ! extant_bodies.hash[ result_edges[i].subject_id() ] ){
				results.push( result_edges[i].subject_id() );
			    }

			}else{
			    throw new Error("world issue in missing_children");	    
			}
		    }
		}
	}
	
	return results;
    };
    

    // Return all children.
    this.get_children = function(nb_id, pred_id){

	if( ! pred_id ){ pred_id = bbop.model.default_predicate; }
	var results = new Array;

	// Get all children. or just children from a specific relation.
	var preds = new Array;
	if( pred_id ){
	    preds.push(pred_id);
	}else{
	    preds = predicates.array;
	}

	for( var j = 0; j < preds.length; j++ ){
	    var pred = preds[j];

	    if( named_bodies.hash[ nb_id ] &&
		predicate_object_table[ pred ] &&
		predicate_object_table[ pred ][ nb_id ] &&
		predicate_object_table[ pred ][ nb_id ].subject.array &&
		predicate_object_table[ pred ][ nb_id ].subject.array.length > 0 ){
		    
		    var result_edges =
			predicate_object_table[ pred ][ nb_id ].subject.array;
		    
		    // Translate result edges into extant bodies.
		    for( var i = 0; i < result_edges.length; i++ ){
			
			if( result_edges[i] &&
			    result_edges[i].subject_id() &&
			    extant_bodies.hash[ result_edges[i].subject_id() ] ){
				results.push(extant_bodies.hash[ result_edges[i].subject_id() ]);
			    }else{
				throw new Error("world issue (for: " +
						nb_id +
						" , '" +
						pred_id +
						"') in get_children: " +
						result_edges[i].subject_id() );
			    }
		    }
		}
	}
	
	return results;
    };
    

    // Return all the extant (in world) children.
    this.get_extant_children = function(nb_id, pred_id){

	if( ! pred_id ){ pred_id = bbop.model.default_predicate; }
	var results = new Array;

	// Get all children. or just children from a specific relation.
	var preds = new Array;
	if( pred_id ){
	    preds.push(pred_id);
	}else{
	    preds = predicates.array;
	}

	for( var j = 0; j < preds.length; j++ ){
	    var pred = preds[j];

	    if( named_bodies.hash[ nb_id ] &&
		predicate_object_table[ pred ] &&
		predicate_object_table[ pred ][ nb_id ] &&
		predicate_object_table[ pred ][ nb_id ].subject.array &&
		predicate_object_table[ pred ][ nb_id ].subject.array.length > 0 ){
		    
		    var result_edges =
			predicate_object_table[ pred ][ nb_id ].subject.array;
		    
		    // Translate result edges into extant bodies.
		    for( var i = 0; i < result_edges.length; i++ ){
			
			if( result_edges[i] &&
			    result_edges[i].subject_id() &&
			    extant_bodies.hash[ result_edges[i].subject_id() ] ){
				results.push(extant_bodies.hash[ result_edges[i].subject_id() ]);
			    }
		    }
		}
	}
	
	return results;
    };
    

    // like get_children, but just does checking.
    this.number_of_children = function(nb_id, pred_id){

	if( ! pred_id ){ pred_id = bbop.model.default_predicate; }
	var result = 0;

	// Get all children. or just children from a specific relation.
	var preds = new Array;
	if( pred_id ){
	    preds.push(pred_id);
	}else{
	    preds = predicates.array;
	}

	for( var j = 0; j < preds.length; j++ ){
	    var pred = preds[j];

	    if( named_bodies.hash[ nb_id ] &&
		predicate_object_table[ pred ] &&
		predicate_object_table[ pred ][ nb_id ] &&
		predicate_object_table[ pred ][ nb_id ].subject.array &&
		predicate_object_table[ pred ][ nb_id ].subject.array.length > 0 ){
		    
		    var result_edges =
			predicate_object_table[ pred ][ nb_id ].subject.array;
		    
		    // Translate result edges into bool.
		    for( var i = 0; i < result_edges.length; i++ ){
			
			if( result_edges[i] &&
			    result_edges[i].subject_id() ){
			    result++;
			}
		    }
		}
	}
	
	return result;
    };
};
