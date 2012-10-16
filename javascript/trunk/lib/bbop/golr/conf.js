/* 
 * Package: conf.js
 * 
 * Generic BBOP manager for dealing with gross GOlr configuration
 * and management.
 * 
 * Contains <bbop.golr.conf_field>, <bbop.golr.conf_class>, and
 * <bbop.golr.conf>.
 * 
 * TODO: better document all of this. Essentially, this is all for
 * getting data out of a JSONized version of the YAML files used to
 * drive the OWLTools-Solr parts of GOlr.
 */

// Setup the internal requirements.
bbop.core.require('bbop', 'core');
bbop.core.namespace('bbop', 'golr');

/*
 * Namespace: bbop.golr.conf_field
 * 
 * Constructor: conf_field
 * 
 * Contructor for a GOlr search field.
 * 
 * Arguments:
 *  field_conf_struct - JSONized config
 * 
 * Returns:
 *  conf_field object
 */
bbop.golr.conf_field = function (field_conf_struct){
    this._is_a = 'bbop.golr.conf_field';

    // Get a good self-reference point.
    var anchor = this;

    // Per-manager logger.
    var logger = new bbop.logger(this._is_a);
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    // Capture search fields.
    this._field = field_conf_struct;

    /*
     * Function: display_name
     * 
     * The user-facing display name. Suitable for label or title
     * somewhere.
     * 
     * Returns:
     *  Display name string.
     */
    this.display_name = function(){
	return this._field['display_name'];
    };

    /*
     * Function: description
     * 
     * A longer description. Suitable for tooltips.
     * 
     * Returns:
     *  Description string.
     */
    this.description = function(){
	return this._field['description'];
    };

    /*
     * Function: id
     * 
     * The unique ID of this profile.
     * 
     * Returns:
     *  String.
     */
    this.id = function(){
	return this._field['id'];
    };

    /*
     * Function: searchable
     * 
     * Returns whether or not a string field has a shadow
     * "*_searchable" field defined that is suitable for dismax
     * searches. Defaults to false.
     * 
     * Returns:
     *  boolean
     */
    this.searchable = function(){
	var retval = false;
	if( this._field['searchable'] == 'true' ||
	    this._field['searchable'] == true ){
		retval = true;	
	    }
	return retval;
    };

    /*
     * Function: required
     * 
     * Returns whether or not this field is required. Defaults to
     * false.
     * 
     * Not of particular use.
     * 
     * Returns:
     *  Boolean.
     */
    this.required = function(){
	var retval = false;
	if( this._field['required'] == 'true' ||
	    this._field['required'] == true ){
		retval = true;	
	    }
	return retval;
    };

    /*
     * Function: is_multi
     * 
     * Using the "cardinality" entry, returns whether or not this
     * field is "single" (false) or "multi" (true). Defaults to false.
     * 
     * Returns:
     *  Boolean.
     */
    this.is_multi = function(){
	var retval = false;
	if( this._field['cardinality'] == 'multi' ){
	    retval = true;	
	}
	return retval;
    };

    /*
     * Function: is_fixed
     * 
     * Using the "property_type" entry, returns whether or not this
     * field is "dynamic" (false) or "fixed" (true). Defaults to false.
     * 
     * Not of particular use.
     * 
     * Returns:
     *  Boolean.
     */
    this.is_fixed = function(){
	var retval = false;
	if( this._field['property_type'] == 'fixed' ){
	    retval = true;	
	}
	return retval;
    };

    /*
     * Function: property
     * 
     * Returns the method of this field's generation in the loader.
     * 
     * Not of particular use.
     * 
     * Returns:
     *  String.
     */
    this.property = function(){
	var retval = '???';
	if( this._field['property'] ){
	    retval = this._field['property'];
	}
	return retval;
    };

    // TODO: ...
};

/*
 * Namespace: bbop.golr.conf_class
 *
 * Constructor: conf_class
 * 
 * Contructor for a GOlr search class.
 * 
 * Arguments:
 *  class_conf_struct - JSONized config
 * 
 * Returns:
 *  conf_class object
 */
bbop.golr.conf_class = function (class_conf_struct){
    this._is_a = 'bbop.golr.conf_class';

    // Get a good self-reference point.
    var anchor = this;

    // Per-manager logger.
    var logger = new bbop.logger(this._is_a);
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    // Capture class and the component fields into variables.
    this._class = class_conf_struct;
    // this._fields = {};
    // bbop.core.each(this._class['fields'],
    // 		   function(item, index){
    // 		       var sf = new bbop.golr.conf_field(item);
    // 		       anchor._fields[sf.id()] = sf;
    // 		  });

    /*
     * Function: display_name
     * 
     * The user-facing display name. Suitable for label or title
     * somewhere.
     * 
     * Returns:
     *  Display name string.
     */
    this.display_name = function(){
	return this._class['display_name'];
    };

    /*
     * Function: description
     * 
     * A longer description. Suitable for tooltips.
     * 
     * Returns:
     *  Description string.
     */
    this.description = function(){
	return this._class['description'];
    };

    /*
     * Function: weight
     * 
     * The relative weight of this search class.
     * 
     * Returns:
     *  Integer.
     */
    this.weight = function(){
    	return parseInt(this._class['weight']) || 0;
    };

    /*
     * Function: id
     * 
     * The unique ID of this profile.
     * 
     * Returns:
     *  String.
     */
    this.id = function(){
	return this._class['id'];
    };

    /*
     * Function: searchable_extension
     * 
     * This returns the searchable extension used for this
     * class. There is a typical default, but it might be change in
     * namespace collisions, so it's better to just use this.
     * 
     * Parameters:
     *  n/a
     * 
     * Returns:
     *  string
     */
    this.searchable_extension = function(){
	return this._class['searchable_extension'] || '_searchable';
    };

    /*
     * Function: get_field
     * 
     * Returns a search field by id string. Null otherwise.
     * 
     * Returns:
     *  <bbop.conf_field>
     */
    this.get_field = function(fid){
	retval = null;
	if( this._class.fields_hash &&
	    this._class.fields_hash[fid] ){
		retval = new bbop.golr.conf_field(this._class.fields_hash[fid]);
	    }
	return retval;
    };

    // Internal function to determine if the weight category that's
    // used by several functions is okay.
    this._munge_weight_category = function(weight_category){

	// Not defined or only the defined few.
	if( ! weight_category ){
	    throw new Error("Missing weight category");	
	}else if( weight_category != 'boost' &&
	    weight_category != 'result' &&
	    weight_category != 'filter' ){
	    throw new Error("Unknown weight category: " + weight_category);
	}

	return weight_category + '_weights';
    };

    /*
     * Function: get_weights
     * 
     * Get the various weights we need to run.
     * 
     * The weight category can be 'boost', 'result', or 'filter'.
     * 
     * Arguments:
     *  weight_category - string identifying the legal weight category
     * 
     * Returns:
     *  object of {field => weight, ...}
     */
    this.get_weights = function(weight_category){
	
	var rethash = {};

	// Only the defined few.
	weight_category = this._munge_weight_category(weight_category);

	// Collect the good bits.
	if( ! bbop.core.is_defined(this._class[weight_category]) ){
	    throw new Error("Missing weight category: " + weight_category);
	}else{
	    // Only work it if there is something there more than "".
	    if( this._class[weight_category] ){
		var dfab = this._class[weight_category];
		var fields = dfab.split(/\s+/);
		bbop.core.each(fields,
			       function(item, i){
				   var field_val = item.split(/\^/);
				   rethash[field_val[0]] =
				       parseFloat(field_val[1]);
			       });
	    }
	}

	return rethash;
    };

    /*
     * Function: field_order_by_weight
     * 
     * Returns an array of field ids ordered by weight.
     * 
     * The weight category can be 'boost', 'result', or 'filter'.
     * 
     * Arguments:
     * weight_category - string identifying the legal weight category
     * cutoff - *[optional]* if not defined, all listed fields in set returned
     * 
     * Returns:
     *  array like [field5, field4, ...]
     */
    this.field_order_by_weight = function(weight_category, cutoff){

    	retset = [];

	var weights = this.get_weights(weight_category);

	// Add the ones that meet threshold (if there is one) to the
	// set.
	bbop.core.each(weights,
		       function(key, val){
			   if( cutoff ){
			       if( val >= cutoff ){
				   retset.push(key);			       
			       }
			   }else{
			       retset.push(key);			       
			   }
		      });

	// Order the set.
	retset.sort(function(a, b){
			return weights[b] - weights[a];
		    });

    	return retset;
    };
};

/*
 * Namespace: bbop.golr.conf
 *
 * Constructor: conf
 * 
 * Contructor for the GOlr query manager.
 * Why don't we just take bbop.golr.golr_meta as read? We want to
 * leave the door open to having multiple GOlrs running in the same area.
 * 
 * Arguments:
 *  golr_conf_var - JSized GOlr config
 * 
 * Returns:
 *  golr conf object
 * 
 */
bbop.golr.conf = function (golr_conf_var){
    this._is_a = 'bbop.golr.conf';

    // Get a good self-reference point.
    var anchor = this;

    // Per-manager logger.
    var logger = new bbop.logger(this._is_a);
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    // Lightly check incoming arguments.
    // There could be a hash of pinned filters argument.
    if( ! golr_conf_var || typeof golr_conf_var != 'object' ){
	ll('ERROR: no proper golr conf var argument');
    }
    
    // Settle in the conf.
    this._golr_conf = golr_conf_var;

    // Process the conf classes into one spot.
    this._classes = {};
    bbop.core.each(anchor._golr_conf,
		  function(key, val){
		      var new_asp = new bbop.golr.conf_class(val);
		      anchor._classes[new_asp.id()] = new_asp;
		  });

    /*
     * Function: get_class
     * 
     * Returns a class info object by id string. Null otherwise.
     * 
     * Arguments:
     *  fid - TODO
     * 
     * Returns:
     *  bbop.conf_class.
     */
    this.get_class = function(fid){
	retval = null;
	if( this._classes &&
	    this._classes[fid] ){
		retval = this._classes[fid];
	    }
	return retval;
    };

    /*
     * Function: get_classes
     * 
     * Returns an array of all search classes.
     * 
     * Returns:
     *  Array of <bbop.golr.conf_class> (unordered).
     */
    this.get_classes = function(){
	ret = [];
	bbop.core.each(anchor._classes,
		       function(key, val){
			   ret.push(val);
		       });
	return ret;
    };
};
