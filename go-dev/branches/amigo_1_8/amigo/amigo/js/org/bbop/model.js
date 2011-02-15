////////////
////
//// org.bbop.model
////
//// This code is most likely a branch of the most recent amigo2
//// version (in lib/javascript/beta) and should be the jumping off
//// point for further work.
//// 
//// Purpose: The basic internal data structure for OBD/BBOP graphs.
//// 
//// TODO: Ideally, The only objects externally available should be:
//// Model, Graph, Node, and Link.
////
//// TODO: This model is not the whole ontology, it is a subgraph. Given
//// this fact, part of the model needs to be aware of whether a node is
//// a leaf over a given relation. You know what? Let's just say that a
//// leaf is a leaf (no dangling links means that you are ).
////
//// TODO/BUG: We're not really doing OBD models anymore--can be
//// stripped way back and simplified.
////
//// Taken name spaces:
////    org.bbop.model.*
////
//////////


// Module and namespace checking.
if ( typeof org == "undefined" ){ org = {}; }
if ( typeof org.bbop == "undefined" ){ org.bbop = {}; }
if ( typeof org.bbop.model == "undefined" ){ org.bbop.model = {}; }


// Constructor for the model.
org.bbop.model.base = function(){
  var id = undefined;
  var version = undefined;
  var metadata = new Array;
  //var metadata = new org.bbop.model.metadata();
  var graphs = new Array;

  //
  this.add_graph = function(graph){ graphs.push(graph); };

  //
  this.get_graphs = function(){ return graphs; };

  //
  this.add_metadata = function(idspace){
    metadata.push(idspace) };
  //this.addMetadata = function(label, id, uri_base){
  //  metadata.add_idSpace(label, id, uri_base); };

  //
  this.id = function(value){
    if( value ) id = value; else return id; };

  //
  this.version = function(val){
    if( val ) version = val; else return version; };
};

  
//////////
//
//  Metadata sub-object
//
//////////
  
// org.bbop.model.metadata = function(){

//   var idspaces = new Array;

//   org.bbop.model.metadata.prototype.add_idSpace = function(label, id, uri_base){
//     var idspace = new org.bbop.model.idspace(label, id, uri_base);
//     idspaces.push(idspace);
//   };
// };


//////////
//
//  IdSpace sub-object
//
//////////

org.bbop.model.idspace = function(in_label, in_id, in_uri_base){
  var label = in_label;
  var id = in_id;
  var uri_base = in_uri_base;

  //
  this.id = function(value){
    if( value ) id = value; else return id; };

  //
  this.label = function(value){
    if( value ) label = value; else return label; };

  //
  this.uri_base = function(val){
    if( val ) uri_base = val; else return uri_base;};
};


//////////
//
//  Graph sub-object
//
//////////

// TODO: Raise org.bbop.model.Relations into this OBDGraph structure.
// TODO: make compilation piecewise with every added node and edge.
org.bbop.model.graph = function(){

  var id = undefined;

  // Data structures to regenerate from an ID.
  var nodes = { array: new Array, hash: {} };        // org.bbop.model.node
  var links = { array: new Array };                 // org.bbop.model.link (hash
						     // not possible)
  var extant_bodies = { array: new Array, hash: {} };// org.bbop.model.link or org.bbop.model.node
  var named_bodies = { array: new Array, hash: {} }; // org.bbop.model.link or org.bbop.model.node
  var predicates = { array: new Array, hash: {} };   // org.bbop.model.link
  var subjects = { array: new Array, hash: {} };     // org.bbop.model.link
  var objects = { array: new Array, hash: {} };      // org.bbop.model.link

  // Table structures for quick lookups of relations.
  var predicate_subject_table = {};    // [pred][sub] -> org.bbop.model.link.
  var subject_predicate_table = {};    // [sub][pred] -> org.bbop.model.link.
  var predicate_object_table = {};     // [pred][obj] -> sub data struct.
  var object_predicate_table = {};     // [obj][pred] -> sub data struct.

  // Table structures for quick lookups of node properties.
  var is_a_singleton_lookup = {}; // [nid] -> org.bbop.model.node.


  //
  this.id = function(value){ if( value ) id = value; else return id; };
  
  
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
  this.add_link = function(link){

    //
    //var link_have_new_pred_p = false;
    //var link_have_new_sub_p = false;
    //var link_have_new_obj_p = false;

    var pred_id = link.get_predicate_id();
    var sub_id = link.get_subject_id();
    var obj_id = link.get_object_id();

    //
    // First, add to the named bodies if appropriate.
    //

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

    //
    // Next, Add link to all predicate-first data structures. This
    // section will be heavily commented, while the next two analogous
    // sections will be crammed together.
    //

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
      //link_have_new_pred_p = true;
    }

    // Add the link to the predicate/subject table if it doesn't
    // exist. Every predicate subject combination can have at most one
    // object--we'll nuke anything in our way.
    if( ! predicate_subject_table[ pred_id ][ sub_id ] ){
      predicate_subject_table[ pred_id ][ sub_id ] = link;
    }

    // Add the link to the predicate/object table if it doesn't
    // exist. Every predicate object combination can have unlimited
    // subjects, so add them into the hash and array stored in the
    // table.
    //
    // Make sure we have a hash at the end of it.
    if( ! predicate_object_table[ pred_id ][ obj_id ] ){
      predicate_object_table[ pred_id ][ obj_id ] = {};
      predicate_object_table[ pred_id ][ obj_id ].subject =
	{ array: new Array,
	  hash: {} };
    }
    // 
    if( ! predicate_object_table[ pred_id ][ obj_id ].subject.hash[ sub_id ] ){
      predicate_object_table[ pred_id ][ obj_id ].subject.hash[ sub_id ] = link;
      predicate_object_table[ pred_id ][ obj_id ].subject.array.push(link);
    }

    //
    // Next, add to all subject-first data structures.
    //

    if( ! subjects.hash[ sub_id ] ){
      subjects.hash[ sub_id ] = true; 
      subjects.array.push(sub_id); 
      subject_predicate_table[ sub_id ] = {};
    }
    if( ! subject_predicate_table[ sub_id ][ pred_id ] ){
      subject_predicate_table[ sub_id ][ pred_id ] = link;
      //link_have_new_sub_p = true;
    }

    //
    // Next, add to all object-first data structures.
    //

    if( ! objects.hash[ obj_id ] ){
      objects.hash[ obj_id ] = true; 
      objects.array.push(obj_id); 
      object_predicate_table[ obj_id ] = {};
    }
    if( ! object_predicate_table[ obj_id ][ pred_id ] ){
      object_predicate_table[ obj_id ][ pred_id ] = {};
      object_predicate_table[ obj_id ][ pred_id ].subject = { array: new Array,
							      hash: {} };
      //link_have_new_obj_p = true;
    }
    if( ! object_predicate_table[ obj_id ][ pred_id ].subject.hash[ sub_id ] ){
      object_predicate_table[ obj_id ][ pred_id ].subject.hash[ sub_id ] = link;
      object_predicate_table[ obj_id ][ pred_id ].subject.array.push(link);
    }

    //
    // Finally, remove the subject and objects from all appropriate
    // 'is_a' tables (which were added during the add_node menthod).
    //    

    // Remove the link's subject and object from the singleton table.
    if( is_a_singleton_lookup[ sub_id ] ){
      delete is_a_singleton_lookup[ sub_id ]; }
    if( is_a_singleton_lookup[ obj_id ] ){
      delete is_a_singleton_lookup[ obj_id ]; }

    // TODO: this was stupid--it's easy to have a situation where
    // there was a link that had the same sub, and there was a link
    // that had the same obj, which is what this is really asking. And
    // frankly, what do I care if we readd an anonymous link?  Final
    // error check--this had better be unique.  if( !
    // link_have_new_pred_p && ! link_have_new_obj_p && !
    // link_have_new_sub_p ){ //alert("tried to add same link: " +
    // pred_id + ' ' + sub_id+ ' ' + obj_id); throw new Error("tried
    // to add same link: " + pred_id + ' ' + sub_id+ ' ' + obj_id);
    // }else{

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
  this.get_link = function(lid){
    var returnval = null;
    if( links.hash[ lid ] ){
      returnval = links.hash[ lid ]; }
    return returnval;
  };


  //
  this.get_link_by_pso = function(pred, sub_id, obj_id){

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
  this.get_links = function(){
    return links.array;
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


  // TODO: Could I speed this up by my moving some of the calculation
  // into the add_node and add_link methods?
  this.isRoot = function(nb_id, pred){

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
  // into the add_node and add_link methods?
  this.getRoots = function(pred){

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
	  //throw new Error("world issue in getRoots: " + nb_id);
	}
      }
    }

    return results;
  };


  // Clips to extant nodes.
//   // TODO: Could I speed this up by my moving some of the calculation
//   // into the add_node and add_link methods?
//   this.getRootNodes = function(pred){

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
  // into the add_node and add_link methods?
  this.get_leaves = function(pred){
    
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
  this.getSingletons = function(){

    var singleton_array = new Array;

    // Translate array into array extant bodies.
    for( var singleton_id in is_a_singleton_lookup ){
      if( extant_bodies.hash[ singleton_id ] ){
	singleton_array.push( extant_bodies.hash[ singleton_id ] );
      }else{
	throw new Error("world issue in getSingletons: " + singleton_id);
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

    var result = null;
    
    if( named_bodies.hash[ node_id ] &&
	predicate_subject_table[ pred ] &&
	predicate_subject_table[ pred ][ node_id ] ){
      
      var found_link = predicate_subject_table[ pred ][ node_id ];
      if( extant_bodies.hash[ found_link.get_object_id() ] ){
	result = extant_bodies.hash[ found_link.get_object_id() ];
      }else{
	throw new Error("world issues in get_parent: " +
			found_link.get_object_id());
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
	  predicate_object_table[ pred ] &&
	  predicate_object_table[ pred ][ nb_id ] &&
	  predicate_object_table[ pred ][ nb_id ].subject.array &&
	  predicate_object_table[ pred ][ nb_id ].subject.array.length > 0 ){
	
	var result_links =
	  predicate_object_table[ pred ][ nb_id ].subject.array;
	
	// Translate result links into extant bodies.
	for( var i = 0; i < result_links.length; i++ ){
	  
	  if( result_links[i] &&
	      result_links[i].get_subject_id() ){

	    // Missing child.
	    if( ! extant_bodies.hash[ result_links[i].get_subject_id() ] ){
	      results.push( result_links[i].get_subject_id() );
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
	  predicate_object_table[ pred ] &&
	  predicate_object_table[ pred ][ nb_id ] &&
	  predicate_object_table[ pred ][ nb_id ].subject.array &&
	  predicate_object_table[ pred ][ nb_id ].subject.array.length > 0 ){
	
	var result_links =
	  predicate_object_table[ pred ][ nb_id ].subject.array;
	
	// Translate result links into extant bodies.
	for( var i = 0; i < result_links.length; i++ ){
	  
	  if( result_links[i] &&
	      result_links[i].get_subject_id() &&
	      extant_bodies.hash[ result_links[i].get_subject_id() ] ){
	    results.push(extant_bodies.hash[ result_links[i].get_subject_id() ]);
	  }else{
	    throw new Error("world issue (for: " +
			    nb_id +
			    " , '" +
			    pred_id +
			    "') in getChildren: " +
			    result_links[i].get_subject_id() );
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
	  predicate_object_table[ pred ] &&
	  predicate_object_table[ pred ][ nb_id ] &&
	  predicate_object_table[ pred ][ nb_id ].subject.array &&
	  predicate_object_table[ pred ][ nb_id ].subject.array.length > 0 ){
	
	var result_links =
	  predicate_object_table[ pred ][ nb_id ].subject.array;
	
	// Translate result links into extant bodies.
	for( var i = 0; i < result_links.length; i++ ){
	  
	  if( result_links[i] &&
	      result_links[i].get_subject_id() &&
	      extant_bodies.hash[ result_links[i].get_subject_id() ] ){
	    results.push(extant_bodies.hash[ result_links[i].get_subject_id() ]);
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
	  predicate_object_table[ pred ] &&
	  predicate_object_table[ pred ][ nb_id ] &&
	  predicate_object_table[ pred ][ nb_id ].subject.array &&
	  predicate_object_table[ pred ][ nb_id ].subject.array.length > 0 ){
	
	var result_links =
	  predicate_object_table[ pred ][ nb_id ].subject.array;
	
	// Translate result links into bool.
	for( var i = 0; i < result_links.length; i++ ){
	  
	  if( result_links[i] &&
	      result_links[i].get_subject_id() ){
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
// 	  predicate_object_table[ pred ] &&
// 	  predicate_object_table[ pred ][ nid ] &&
// 	  predicate_object_table[ pred ][ nid ].subject.array &&
// 	  predicate_object_table[ pred ][ nid ].subject.array.length > 0 ){
	
// 	var result_links =
// 	  predicate_object_table[ pred ][ nid ].subject.array;
	
// 	// Translate result links into extant bodies.
// 	for( var i = 0; i < result_links.length; i++ ){
// 	  // Just our nodes, clip and ignore problems.
// 	  if( nodes.hash[ result_links[i].get_subject_id() ] ){
// 	    results.push(nodes.hash[ result_links[i].get_subject_id() ]);
// 	  }
// 	}
//       }
//     }
    
//     return results
//   };
};


//////////
//
//  OBDNode sub-object
//
//////////

org.bbop.model.node = function(new_id){
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
org.bbop.model.node.prototype.id = function(value){
  if(value) this._id = value; else return this._id; };


//
org.bbop.model.node.prototype.type = function(value){
  if(value) this._type = value; else return this._type; };


//
org.bbop.model.node.prototype.metadata = function(value){
  if(value) this._metadata = value; else return this._metadata; };


//
org.bbop.model.node.prototype.metatype = function(value){
  if(value) this._metatype = value; else return this._metatype; };


//
org.bbop.model.node.prototype.label = function(value){
  if(value) this._label = value; else return this._label; };


//
org.bbop.model.node.prototype.isAnonymous = function(value){
  if(value) this._is_anonymous = value; else return this._is_anonymous; };


//
org.bbop.model.node.prototype.isTransitive = function(value){
  if(value) this._is_transitive = value; else return this._is_transitive; };


//
org.bbop.model.node.prototype.setSource = function(about){
  this._source = new org.bbop.model.linkref(about); };


//
org.bbop.model.node.prototype.getSource = function(){
  var returnval = undefined;
  if( this._source ){
    returnval = this._source.about();
  }
  return returnval;
};


//
org.bbop.model.node.prototype.add_tagval = function(tagval){
  this._tagvals.push(tagval); };


//
org.bbop.model.node.prototype.addAlias = function(alias){
  this._aliases.push(alias); };


//
// org.bbop.model.node.prototype.getAlias = function(index){
//   var returnval = null;
  
//   if( index < this._aliases.length && index >= 0){
//     returnval = this._aliases[index];
//   }
  
//   return returnval;
// };


//
org.bbop.model.node.prototype.getAliases = function(){
  return this._aliases;
};


//
org.bbop.model.node.prototype.addDescription = function(desc){
  this._descriptions.push(desc); };


//
// org.bbop.model.node.prototype.getDescription = function(index){
//   var returnval = null;
  
//   if( index < this._descriptions.length && index >= 0){
//     returnval = this._descriptions[index];
//   }
  
//   return returnval;
// };


//
org.bbop.model.node.prototype.getDescriptions = function(){
  return this._descriptions.length;
};


//
org.bbop.model.node.prototype.add_xref = function(xref){
  this._xrefs.push(xref); };


//
// org.bbop.model.node.prototype.get_xref = function(index){
//   var returnval = null;
  
//   if( index < this._xrefs.length && index >= 0){
//     returnval = this._xrefs[index];
//   }
  
//   return returnval;
// };


//
org.bbop.model.node.prototype.get_xrefs = function(){
  return this._xrefs;
};



// Return an OBD_tagval by searching for the key in the val.
org.bbop.model.node.prototype.get_tagval = function(key){

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


//////////
//
//  Link sub-object
//
//////////
 
org.bbop.model.link = function(subject, predicate, object){
  this._id = undefined;
  this._type = 'link';
  this._combinator = undefined;
  this._subject = new org.bbop.model.linkref(subject);
  this._predicate = new org.bbop.model.linkref(predicate);
  this._object = new org.bbop.model.linkref(object);
};

org.bbop.model.link.prototype.id = function(value){
  if(value) this._id = value; else return this._id; };


org.bbop.model.node.prototype.type = function(value){
  if(value) this._type = value; else return this._type; };


org.bbop.model.link.prototype.combinator = function(value){
  if(value) this._combinator = value; else return this._combinator; };
  
org.bbop.model.link.prototype.get_subject_id = function(){
  return this._subject.about(); };

org.bbop.model.link.prototype.get_predicate_id = function(){
  return this._predicate.about(); };

org.bbop.model.link.prototype.get_object_id = function(){
  return this._object.about(); };

//////////
//
//  Linkref sub-object
//
//////////


org.bbop.model.linkref = function(about){
  this._about = about;
};


//
org.bbop.model.linkref.prototype.about = function(value){
  if(value) this._about = value; else return this._about; };


//////////
//
//  _tagval sub-object
//
//////////


org.bbop.model.tagval = function(tag, val){
  this._tag = tag;
  this._val = val;
};


//
org.bbop.model.tagval.prototype.val = function(val){
  if(val){ this._val = val;
  }else{ return this._val; }
};


//
org.bbop.model.tagval.prototype.tag = function(tag){
  if(tag){ this._tag = tag;
  }else{ return this._tag; }
};


//////////
//
//  Alias sub-object
//
//////////


org.bbop.model.alias = function(label){
  this._alias = new org.bbop.model.structured_info(label);
};


//////////
//
//  Description sub-object
//
//////////


org.bbop.model.description = function(label){
  this._description = new org.bbop.model.structured_info(label);
};


//////////
//
//  Structured_info sub-object
//
//////////


org.bbop.model.structured_info = function(label){
  this._label = label;
  this._id = undefined;
  this._scope = undefined;
  this._type = undefined;
  this._xrefs = new Array;
};


//
org.bbop.model.structured_info.prototype.id = function(value){
  if(value) this._id = value; else return this._id; };


//
org.bbop.model.structured_info.prototype.label = function(value){
  if(value) this._label = value; else return this._label; };


//
org.bbop.model.structured_info.prototype.scope = function(value){
  if(value) this._scope = value; else return this._scope; };


//
org.bbop.model.structured_info.prototype.type = function(value){
  if(value) this._type = value; else return this._type; };


//////////
//
//  _xref sub-object
//
//////////

org.bbop.model.xref = function(about){
  this._context = undefined;
  this._linkref = new org.bbop.model.linkref(about);
};


//
org.bbop.model.xref.prototype.context = function(value){
  if(value) this._context = value; else return this._context; };

//
org.bbop.model.xref.prototype.about = function(value){
  return this._linkref.about(value) };


//////////
//
//  Literal sub-object
//
//////////

org.bbop.model.literal= function(string){
  this._id = undefined;
  this._string = string;
};


//
org.bbop.model.literal.prototype.id = function(value){
  if(value) this._id = value; else return this._id; };
