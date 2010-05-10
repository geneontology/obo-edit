////
//// Graph-based model.
////
//// Purpose: The basic graph for AmiGO JS API.
//// 
//// TODO: Ideally, The only objects externally available should be:
//// model, graph, node, and link.
////
//// TODO: This model is not the whole ontology, it is a subgraph. Given
//// this fact, part of the model needs to be aware of whether a node is
//// a leaf over a given relation. You know what? Let's just say that a
//// leaf is a leaf (no dangling links means that you are ).
////
//// NOTE: We're using a rather flat model namespace (lifted from the
//// original OBD code from back in the day.) We take:
////
////     org.bbop.amigo.model
////     org.bbop.amigo.model.graph
////     org.bbop.amigo.model.node
////     org.bbop.amigo.model.link
////     org.bbop.amigo...etc...
////

// Requires the model, handle, and my ajax.
if ( typeof org.bbop.amigo == "undefined" ){
    throw new Error("need org.bbop.amigo");
}

// Module and namespace checking.
if ( typeof org.bbop.amigo.model == "undefined" ){
    org.bbop.amigo.model = {};
}

///
/// graph
///

// TODO: Raise relations into this graph.
// TODO: make compilation piecewise with every added node and edge.
org.bbop.amigo.model.graph = function(){

    var id = undefined;

    // Data structures to regenerate from an ID.
    var nodes = { array: new Array, hash: {} }; // node
    var links = { array: new Array };          // link (hash not possible)
    var extant_bodies = { array: new Array, hash: {} }; // link or node
    var named_bodies = { array: new Array, hash: {} }; // link or node
    var predicates = { array: new Array, hash: {} };  // link
    var subjects = { array: new Array, hash: {} };   // link
    var objects = { array: new Array, hash: {} };   // link
    
    // Table structures for quick lookups of relations.
    var pred_sub_tbl = {}; // [pred][sub] -> link.
    var sub_pred_tbl = {}; // [sub][pred] -> link.
    var pred_obj_tbl = {}; // [pred][obj] -> sub data struct.
    var obj_pred_tbl = {}; // [obj][pred] -> sub data struct.
    
    // Table structures for quick lookups of node properties.
    var is_a_singleton_lookup = {}; // [nid] -> node.
    

    //
    this.id = function(value){
	if( value ) id = value; else return id;
    };
    
    //
    this.addNode = function(node){
	
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
    this.addLink = function(link){

	//
	//var link_have_new_pred_p = false;
	//var link_have_new_sub_p = false;
	//var link_have_new_obj_p = false;
	
	var pred_id = link.getPredicateId();
	var sub_id = link.getSubjectId();
	var obj_id = link.getObjectId();
	
	///
	/// First, add to the named bodies if appropriate.
	///
	
	// Check on whether a pure id or named body.
	if( link.id() &&
	    extant_bodies.hash[ link.id() ] ){
	    //alert("tried to add same (extant) link: " + link.id());
	    throw new Error("tried to add same (extant) link: " + link.id());
	}else if( link.id() ){
	    // Add the id to the extant and named bodies if appropriate.
	    extant_bodies.hash[ link.id() ] = link;
	    extant_bodies.array.push(link);
	    if( ! named_bodies.hash[ link.id() ] ){
		named_bodies.array.push(link); }
	    named_bodies.hash[ link.id() ] = link;
	}

	///
	/// Next, Add link to all predicate-first data structures. This
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
	    pred_sub_tbl[ pred_id ] = {};
	    pred_obj_tbl[ pred_id ] = {};
	    
	    //
	    //link_have_new_pred_p = true;
	}
	
	// Add the link to the predicate/subject table if it doesn't
	// exist. Every predicate subject combination can have at most
	// one object--we'll nuke anything in our way.
	if( ! pred_sub_tbl[ pred_id ][ sub_id ] ){
	    pred_sub_tbl[ pred_id ][ sub_id ] = link;
	}
	
	// Add the link to the predicate/object table if it doesn't
	// exist. Every predicate object combination can have
	// unlimited subjects, so add them into the hash and array
	// stored in the table.
	//
	// Make sure we have a hash at the end of it.
	if( ! pred_obj_tbl[ pred_id ][ obj_id ] ){
	    pred_obj_tbl[ pred_id ][ obj_id ] = {};
	    pred_obj_tbl[ pred_id ][ obj_id ].subject =
		{ array: new Array,
		  hash: {} };
	}
	// 
	if( ! pred_obj_tbl[ pred_id ][ obj_id ].subject.hash[ sub_id ] ){
	    pred_obj_tbl[ pred_id ][ obj_id ].subject.hash[ sub_id ] = link;
	    pred_obj_tbl[ pred_id ][ obj_id ].subject.array.push(link);
	}
	
	///
	/// Next, add to all subject-first data structures.
	///
	
	if( ! subjects.hash[ sub_id ] ){
	    subjects.hash[ sub_id ] = true; 
	    subjects.array.push(sub_id); 
	    sub_pred_tbl[ sub_id ] = {};
	}
	if( ! sub_pred_tbl[ sub_id ][ pred_id ] ){
	    sub_pred_tbl[ sub_id ][ pred_id ] = link;
	    //link_have_new_sub_p = true;
	}
	
	///
	/// Next, add to all object-first data structures.
	///
	
	if( ! objects.hash[ obj_id ] ){
	    objects.hash[ obj_id ] = true; 
	    objects.array.push(obj_id); 
	    obj_pred_tbl[ obj_id ] = {};
	}
	if( ! obj_pred_tbl[ obj_id ][ pred_id ] ){
	    obj_pred_tbl[ obj_id ][ pred_id ] = {};
	    obj_pred_tbl[ obj_id ][ pred_id ].subject = { array: new Array,
							  hash: {} };
	    //link_have_new_obj_p = true;
	}
	if( ! obj_pred_tbl[ obj_id ][ pred_id ].subject.hash[ sub_id ] ){
	    obj_pred_tbl[ obj_id ][ pred_id ].subject.hash[ sub_id ] = link;
	    obj_pred_tbl[ obj_id ][ pred_id ].subject.array.push(link);
	}
	
	///
	/// Finally, remove the subject and objects from all appropriate
	/// 'is_a' tables (which were added during the addNode menthod).
	///    
	
	// Remove the link's subject and object from the singleton table.
	if( is_a_singleton_lookup[ sub_id ] ){
	    delete is_a_singleton_lookup[ sub_id ]; }
	if( is_a_singleton_lookup[ obj_id ] ){
	    delete is_a_singleton_lookup[ obj_id ]; }
	
	///
	/// TODO: this was stupid--it's easy to have a situation where
	/// there was a link that had the same sub, and there was a
	/// link that had the same obj, which is what this is really
	/// asking. And frankly, what do I care if we readd an
	/// anonymous link?  Final error check--this had better be
	/// unique.  if( !  link_have_new_pred_p && !
	/// link_have_new_obj_p && !  link_have_new_sub_p ){
	/// //alert("tried to add same link: " + pred_id + ' ' +
	/// sub_id+ ' ' + obj_id); throw new Error("tried to add same
	/// link: " + pred_id + ' ' + sub_id+ ' ' + obj_id); }else{
	///

	// Onto the array and subject and object into named bodies.
	links.array.push(link);
	if( ! named_bodies.hash[ sub_id ] ){
	    named_bodies.array.push(sub_id); }
	named_bodies.hash[ sub_id ] = link;
	if( ! named_bodies.hash[ obj_id ] ){
	    named_bodies.array.push(obj_id); }
	named_bodies.hash[ obj_id ] = link;
	//}
    };

    
    //
    this.getNode = function(nid){
	var returnval = null;
	if( nodes.hash[ nid ] ){
	    returnval = nodes.hash[ nid ]; }
	return returnval;
    };

    
    //
    this.getNodes = function(){
	return nodes.array;
    };
    
    
    //
    this.getLink = function(lid){
	var returnval = null;
	if( links.hash[ lid ] ){
	    returnval = links.hash[ lid ]; }
	return returnval;
    };
    
    
    //
    this.getLinkByPSO = function(pred, sub_id, obj_id){
	
	var returnval = null;
	
	if( pred_obj_tbl[ pred ] &&
	    pred_obj_tbl[ pred ][ obj_id ] &&
	    pred_obj_tbl[ pred ][ obj_id ].subject &&
	    pred_obj_tbl[ pred ][ obj_id ].subject.hash &&
	    pred_obj_tbl[ pred ][ obj_id ].subject.hash[ sub_id ] ){
	    returnval =
		pred_obj_tbl[ pred ][ obj_id ].subject.hash[ sub_id ];
	}
	
	return returnval;
    }; 
    
    
    //
    this.getLinks = function(){
	return links.array;
    };
    
    
    //
    this.getExtantBody = function(ebid){
	var returnval = null;
	if( extant_bodies.hash[ nbid ] ){
	    returnval = extant_bodies.hash[ nbid ]; }
	return returnval;
    };
    
    
    // Returns an array of named bodies.
    this.getExtantBodies = function(){
	return extant_bodies.array;
    };
    

    //
    this.getPredicates = function(){
	return predicates.array;
    };
    
    
    // Just going through all the nodes and checking.
    //this.getRoots = function(pred){
    this.getRoots = function(){

	var results = new Array();
	
	//for( var nb_id in named_bodies.hash ){
	var possible_roots = this.getNodes();
	for( var pr = 0; pr < possible_roots.length; pr++ ){
	    
	    var n = possible_roots[pr];
	    var n_id = n.id();

	    // Pass responsibility for checking on...
	    //if( this.isRoot(n_id, pred) ){
	    if( this.isRoot(n_id) ){
		results.push( extant_bodies.hash[ n_id ] );
	    }else{
		// BUG: while this should be the correct behavior,
		// there are some cases where due to part_of
		// children, the referenced node will be a
		// singleton is_a root that doesn't have an extant
		// body.  throw new Error("world issue in
		// getRoots: " + nb_id);
	    }
	}
	return results;
    };

    
    // TODO: Could I speed this up by my moving some of the
    // calculation into the addNode and addLink methods?
    //this.isRoot = function(nb_id, pred){
    this.isRoot = function(nb_id){
	
	var result = false;
	
	// After making sure that our arguments are sanely defined,
	// tick off the definitions of whether a node is root over a
	// relation: 1) not a subject to anything, 2) the predicate
	// doesn't exist, 3) is not a subject of the predicate.
	// 	if( pred ){
	// 	    if( named_bodies.hash[ nb_id ] &&
	// 		( ! subjects.hash[ nb_id ] ||
	// 		  ! pred_sub_tbl[ pred ] ||
	// 		  ! pred_sub_tbl[ pred ][ nb_id ] ) ){	
	// 		result = true;
	// 	    }
	// 	}else{
	if( named_bodies.hash[ nb_id ] &&
	    ! subjects.hash[ nb_id ] ){
	    result = true;
	}
	//	}
	
	return result;
    };
    
    
    // Clips to extant nodes.
    //   // TODO: Could I speed this up by my moving some of the calculation
    //   // into the addNode and addLink methods?
    //   this.getRootNodes = function(pred){    
    //     var results = new Array;
    //     for( var nb_id in nodes.hash ){
    //       // Tick off the deifinitions of whether a node is root over a
    //       // relation: 1) not a subject to anything, 2) the predicate
    //       // doesn't exist, 3) is not a subject of the predicate.
    //       if( ! subjects.hash[ nb_id ] ||
    // 	  ! pred_sub_tbl[ pred ] ||
    // 	  ! pred_sub_tbl[ pred ][ nb_id ] ){
    // 	// Nodes are in world by definition.
    // 	results.push( nodes.hash[ nb_id ] );
    //       }
    //     }
    //     return results;
    //   };
    

    // TODO: Could I speed this up by my moving some of the
    // calculation into the addNode and addLink methods?
    //this.getLeaves = function(pred){
    this.getLeaves = function(){
	
	var leaves_array = new Array;

	//for( var nb_id in named_bodies.hash ){
	var possible_leaves = this.getNodes();
	for( var pr = 0; pr < possible_leaves.length; pr++ ){
	    
	    var n = possible_leaves[pr];
	    var n_id = n.id();

	    // After making sure that our arguments are sanely
	    // defined, tick off the definitions of whether a node is
	    // leaf over a relation: 1) not an object to anything, 2)
	    // the predicate doesn't exist, 3) is not an object of the
	    // predicate.
	    // Pass responsibility for checking on...
	    //if( this.isLeaf(n_id, pred) ){
	    if( this.isLeaf(n_id) ){
		leaves_array.push( extant_bodies.hash[ n_id ] );
		//}else{
		//throw new Error("world issue in getLeaves: " + n_id);
	    }
	}	
	return leaves_array;
    };
    

    //
    //this.isLeaf = function(nb_id, pred){
    this.isLeaf = function(nb_id){
	
	var result = false;
	
	// After making sure that our arguments are sanely defined,
	// tick off the definitions of whether a node is leaf over a
	// relation: 1) not an object to anything, 2) the predicate
	// doesn't exist, 3) is not an object of the predicate.
	//if( named_bodies.hash[ nb_id ] && pred &&
	if( named_bodies.hash[ nb_id ] &&
	    ! objects.hash[ nb_id ] ){
	    // subjects.hash[ nb_id ] ){
	    result = true;
	}
	
	//print("     ___: " + nb_id + ' _ ' + result);

	return result;
    };
    

    // Nodes that are roots and leaves over all relations.
    this.getSingletons = function(){

	var singleton_array = new Array;

	// Translate array into array extant bodies.
	for( var singleton_id in is_a_singleton_lookup ){
	    if( extant_bodies.hash[ singleton_id ] ){
		singleton_array.push( extant_bodies.hash[ singleton_id ] );
	    }else{
		throw new Error("world issue in getSingletons: " +
				singleton_id);
	    }
	}
	
	return singleton_array;
    };


    // TODO:
    //this.getDangling = function(){
    //  // disjoint of named and extant
    //}
    
    
    //
    this.getParent = function(node_id, pred){
	
	var result = null;
	
	if( named_bodies.hash[ node_id ] &&
	    pred_sub_tbl[ pred ] &&
	    pred_sub_tbl[ pred ][ node_id ] ){
	    
	    var found_link = pred_sub_tbl[ pred ][ node_id ];
	    if( extant_bodies.hash[ found_link.getObjectId() ] ){
		result = extant_bodies.hash[ found_link.getObjectId() ];
	    }else{
		throw new Error("world issues in getParent: " +
				found_link.getObjectId());
	    }
	}
	
	return result;
    };


    // List all ids for children seen, but not accounted for.
    this.missingChildren = function(nb_id, pred_id){
	
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
		pred_obj_tbl[ pred ] &&
		pred_obj_tbl[ pred ][ nb_id ] &&
		pred_obj_tbl[ pred ][ nb_id ].subject.array &&
		pred_obj_tbl[ pred ][ nb_id ].subject.array.length > 0 ){
		
		var result_links =
		    pred_obj_tbl[ pred ][ nb_id ].subject.array;
		
		// Translate result links into extant bodies.
		for( var i = 0; i < result_links.length; i++ ){
		    
		    if( result_links[i] &&
			result_links[i].getSubjectId() ){
			
			// Missing child.
			if( ! extant_bodies.hash[result_links[i].getSubjectId()] ){
			    results.push( result_links[i].getSubjectId() );
			}
			
		    }else{
			throw new Error("world issue in missingChildren");	    
		    }
		}
	    }
	}
	
	return results
    };
    
    
    // Return all children.
    this.getChildren = function(nb_id, pred_id){
	
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
		pred_obj_tbl[ pred ] &&
		pred_obj_tbl[ pred ][ nb_id ] &&
		pred_obj_tbl[ pred ][ nb_id ].subject.array &&
		pred_obj_tbl[ pred ][ nb_id ].subject.array.length > 0 ){
		
		var result_links =
		    pred_obj_tbl[ pred ][ nb_id ].subject.array;
		
		// Translate result links into extant bodies.
		for( var i = 0; i < result_links.length; i++ ){
		    
		    if( result_links[i] &&
			result_links[i].getSubjectId() &&
			extant_bodies.hash[ result_links[i].getSubjectId() ] ){
			results.push(extant_bodies.hash[ result_links[i].getSubjectId() ]);
		    }else{
			throw new Error("world issue (for: " +
					nb_id +
					" , '" +
					pred_id +
					"') in getChildren: " +
					result_links[i].getSubjectId() );
		    }
		}
	    }
	}
	
	return results
    };
    
    
    // Return all the extant (in world) children.
    this.getExtantChildren = function(nb_id, pred_id){
	
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
		pred_obj_tbl[ pred ] &&
		pred_obj_tbl[ pred ][ nb_id ] &&
		pred_obj_tbl[ pred ][ nb_id ].subject.array &&
		pred_obj_tbl[ pred ][ nb_id ].subject.array.length > 0 ){
		
		var result_links =
		    pred_obj_tbl[ pred ][ nb_id ].subject.array;
		
		// Translate result links into extant bodies.
		for( var i = 0; i < result_links.length; i++ ){
		    
		    if( result_links[i] &&
			result_links[i].getSubjectId() &&
			extant_bodies.hash[ result_links[i].getSubjectId() ] ){
			results.push(extant_bodies.hash[ result_links[i].getSubjectId() ]);
		    }
		}
	    }
	}
	
	return results
    };
    

    // like getChildren, but just does checking.
    this.numberOfChildren = function(nb_id, pred_id){

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
		pred_obj_tbl[ pred ] &&
		pred_obj_tbl[ pred ][ nb_id ] &&
		pred_obj_tbl[ pred ][ nb_id ].subject.array &&
		pred_obj_tbl[ pred ][ nb_id ].subject.array.length > 0 ){
		
		var result_links =
		    pred_obj_tbl[ pred ][ nb_id ].subject.array;
		
		// Translate result links into bool.
		for( var i = 0; i < result_links.length; i++ ){
		    
		    if( result_links[i] &&
			result_links[i].getSubjectId() ){
			result++;
		    }
		}
	    }
	}
	
	return result
    };
    

    //   // Clips to extant nodes.
    //   this.getChildNodes = function(nid, pred_id){

    //     var results = new Array;

    //     // Get all children. or just children from a specific relation.
    //     var preds = new Array;
    //     if( pred_id ){
    //       preds.push(pred_id);
    //     }else{
    //       preds = predicates.array;
    //     }

    //     for( var j = 0; j < preds.length; j++ ){
    //       var pred = preds[j];

    //       if( nodes.hash[ nid ] &&
    // 	  pred_obj_tbl[ pred ] &&
    // 	  pred_obj_tbl[ pred ][ nid ] &&
    // 	  pred_obj_tbl[ pred ][ nid ].subject.array &&
    // 	  pred_obj_tbl[ pred ][ nid ].subject.array.length > 0 ){
    
    // 	var result_links =
    // 	  pred_obj_tbl[ pred ][ nid ].subject.array;
    
    // 	// Translate result links into extant bodies.
    // 	for( var i = 0; i < result_links.length; i++ ){
    // 	  // Just our nodes, clip and ignore problems.
    // 	  if( nodes.hash[ result_links[i].getSubjectId() ] ){
    // 	    results.push(nodes.hash[ result_links[i].getSubjectId() ]);
    // 	  }
    // 	}
    //       }
    //     }
    
    //     return results
    //   };
};

///
/// node
///

org.bbop.amigo.model.node = function(new_id){
    this._id = new_id;
    this._type = 'node';
    this._metadata = undefined;
    this._metatype = undefined;
    this._is_anonymous = undefined;
    this._is_transitive = undefined;
    this._label = undefined;
    this._source = undefined;
    this._tagvals = new Array;
    this._aliases = new Array;
    this._descriptions = new Array;
    this._xrefs = new Array;
};


//
org.bbop.amigo.model.node.prototype.id = function(value){
    if(value) this._id = value; else return this._id; };


//
org.bbop.amigo.model.node.prototype.type = function(value){
    if(value) this._type = value; else return this._type; };


//
org.bbop.amigo.model.node.prototype.metadata = function(value){
    if(value) this._metadata = value; else return this._metadata; };


//
org.bbop.amigo.model.node.prototype.metatype = function(value){
    if(value) this._metatype = value; else return this._metatype; };


//
org.bbop.amigo.model.node.prototype.label = function(value){
    if(value) this._label = value; else return this._label; };


//
org.bbop.amigo.model.node.prototype.isAnonymous = function(value){
    if(value) this._is_anonymous = value; else return this._is_anonymous; };


//
org.bbop.amigo.model.node.prototype.isTransitive = function(value){
    if(value) this._is_transitive = value; else return this._is_transitive; };


//
org.bbop.amigo.model.node.prototype.setSource = function(about){
    this._source = new org.bbop.amigo.model.linkRef(about); };


//
org.bbop.amigo.model.node.prototype.getSource = function(){
    var returnval = undefined;
    if( this._source ){
	returnval = this._source.about();
    }
    return returnval;
};


//
org.bbop.amigo.model.node.prototype.addTagval = function(tagval){
    this._tagvals.push(tagval); };


//
org.bbop.amigo.model.node.prototype.addAlias = function(alias){
    this._aliases.push(alias); };


//
// org.bbop.amigo.model.node.prototype.getAlias = function(index){
//   var returnval = null;
//   if( index < this._aliases.length && index >= 0){
//     returnval = this._aliases[index];
//   }
//   return returnval;
// };


//
org.bbop.amigo.model.node.prototype.getAliases = function(){
    return this._aliases;
};


//
org.bbop.amigo.model.node.prototype.addDescription = function(desc){
    this._descriptions.push(desc); };


//
// org.bbop.amigo.model.node.prototype.getDescription = function(index){
//   var returnval = null;
//   if( index < this._descriptions.length && index >= 0){
//     returnval = this._descriptions[index];
//   }
//   return returnval;
// };


//
org.bbop.amigo.model.node.prototype.getDescriptions = function(){
    return this._descriptions.length;
};


//
org.bbop.amigo.model.node.prototype.addXref = function(xref){
    this._xrefs.push(xref); };


//
// org.bbop.amigo.model.node.prototype.get_xref = function(index){
//   var returnval = null;
//   if( index < this._xrefs.length && index >= 0){
//     returnval = this._xrefs[index];
//   }
//   return returnval;
// };


//
org.bbop.amigo.model.node.prototype.getXrefs = function(){
    return this._xrefs;
};



// Return an org.bbop.amigo.modelTagval by searching for the key in the val.
org.bbop.amigo.model.node.prototype.getTagval = function(key){

    var returnval = null;
    
    // TODO: a scan? fix this!
    for( var i = 0; i < this._tagvals.length; i++ ){
	if( key ==  this._tagvals[i].tag() ){
	    returnval = this._tagvals[i];
	    i = this._tagvals.length;
	}
    }
    
    return returnval;
};

///
/// link
///

org.bbop.amigo.model.link = function(subject, predicate, object){
    this._id = undefined;
    this._type = 'link';
    this._combinator = undefined;
    this._subject = new org.bbop.amigo.model.linkRef(subject);
    this._predicate = new org.bbop.amigo.model.linkRef(predicate);
    this._object = new org.bbop.amigo.model.linkRef(object);
};

org.bbop.amigo.model.link.prototype.id = function(value){
    if(value) this._id = value; else return this._id; };

org.bbop.amigo.model.node.prototype.type = function(value){
    if(value) this._type = value; else return this._type; };

org.bbop.amigo.model.link.prototype.combinator = function(value){
    if(value) this._combinator = value; else return this._combinator; };

org.bbop.amigo.model.link.prototype.getSubjectId = function(){
    return this._subject.about(); };

org.bbop.amigo.model.link.prototype.getPredicateId = function(){
    return this._predicate.about(); };

org.bbop.amigo.model.link.prototype.getObjectId = function(){
    return this._object.about(); };

///
/// linkRef
///

org.bbop.amigo.model.linkRef = function(about){
    this._about = about;
};


//
org.bbop.amigo.model.linkRef.prototype.about = function(value){
    if(value) this._about = value; else return this._about; };

///
/// tagval
///

org.bbop.amigo.model.tagval = function(tag, val){
    this._tag = tag;
    this._val = val;
};


//
org.bbop.amigo.model.tagval.prototype.val = function(val){
    if(val){ this._val = val;
	   }else{ return this._val; }
};


//
org.bbop.amigo.model.tagval.prototype.tag = function(tag){
    if(tag){ this._tag = tag;
	   }else{ return this._tag; }
};


///
/// alias
///


org.bbop.amigo.model.alias = function(label){
    this._alias = new org.bbop.amigo.model.StructuredInfo(label);
};

///
/// description
///

org.bbop.amigo.model.description = function(label){
    this._description = new org.bbop.amigo.model.StructuredInfo(label);
};

///
///  structuredInfo
///

org.bbop.amigo.model.structuredInfo = function(label){
    this._label = label;
    this._id = undefined;
    this._scope = undefined;
    this._type = undefined;
    this._xrefs = new Array;
};


//
org.bbop.amigo.model.structuredInfo.prototype.id = function(value){
    if(value) this._id = value; else return this._id; };


//
org.bbop.amigo.model.structuredInfo.prototype.label = function(value){
    if(value) this._label = value; else return this._label; };


//
org.bbop.amigo.model.structuredInfo.prototype.scope = function(value){
    if(value) this._scope = value; else return this._scope; };


//
org.bbop.amigo.model.structuredInfo.prototype.type = function(value){
    if(value) this._type = value; else return this._type; };

///
/// xref
///

org.bbop.amigo.model.xref = function(about){
    this._context = undefined;
    this._linkref = new org.bbop.amigo.model.linkRef(about);
};


//
org.bbop.amigo.model.xref.prototype.context = function(value){
    if(value) this._context = value; else return this._context; };

//
org.bbop.amigo.model.xref.prototype.about = function(value){
    return this._linkref.about(value) };

///
///  literal
///

org.bbop.amigo.model.literal = function(string){
    this._id = undefined;
    this._string = string;
};


//
org.bbop.amigo.model.literal.prototype.id = function(value){
    if(value) this._id = value; else return this._id; };
