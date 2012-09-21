/* 
 * Package: registry.js
 * 
 * Namespace: bbop.registry
 * 
 * BBOP generic lightweight listener/callback registry system.
 */

bbop.core.require('bbop', 'core');
bbop.core.namespace('bbop', 'registry');

/*
 * Constructor: registry
 * 
 * Contructor for BBOP registry. Takes a list of event categories as
 * strings.
 * 
 * Arguments:
 *  evt_list - a list of strings that identify the events to be used
 * 
 * Returns:
 *  bbop registry object
 */
bbop.registry = function(evt_list){
    this._is_a = 'bbop.registry';

    var registry_anchor = this;

    // Handle the registration of call functions to get activated
    // after certain events.
    this.callback_registry = {};
    bbop.core.each(evt_list, function(item, i){
		       registry_anchor.callback_registry[item] = {};
		   });
    
    /*
     * Function: register
     *
     * Add the specified function from the registry, with an optional
     * relative priority against other callback functions.
     *
     * The in_priority value is relative to others in the category,
     * with a higher priority...getting priority.
     * 
     * Parameters: 
     *  category - string; ont of the pre-defined categories
     *  function_id - string; a unique string to identify a function
     *  in_function - function
     *  in_priority - *[optional]* number
     *
     * Returns: 
     *  n/a
     * 
     * See also:
     *  <apply>
     */
    this.register = function(category, function_id, in_function, in_priority){

	// Only these categories.
	if( typeof(registry_anchor.callback_registry[category]) == 'undefined'){
	    throw new Error('cannot register, unknown category');
	}

	// The default priority is 0.
	var priority = 0;
	if( in_priority ){ priority = in_priority; }

	registry_anchor.callback_registry[category][function_id] =
	    {
		runner: in_function,
		priority: priority
	    };
    };

    /*
     * Function: unregister
     *
     * Remove the specified function from the registry. Must specify a
     * legitimate category and the function id of the function in it.
     *
     * Parameters: 
     *  category - string
     *  function_id - string
     *
     * Returns: 
     *  n/a
     */
    this.unregister = function(category, function_id){
	if( registry_anchor.callback_registry[category] &&
	    registry_anchor.callback_registry[category][function_id] ){
		delete registry_anchor.callback_registry[category][function_id];
            }
    };
    
    /*
     * Function: get_callbacks
     *
     * Generic getter for callback functions, returns by priority.
     *
     * Parameters: 
     *  category - string
     *
     * Returns: 
     *  an ordered (by priority) list of function_id strings
     */
    this.get_callbacks = function(category){

	var cb_id_list =
	    bbop.core.get_keys(registry_anchor.callback_registry[category]);
	// Sort callback list according to priority.
	var ptype_registry_anchor = this;
	cb_id_list.sort(
	    function(a, b){  
		var pkg_a =
		    ptype_registry_anchor.callback_registry[category][a];
		var pkg_b =
		    ptype_registry_anchor.callback_registry[category][b];
		return pkg_b['priority'] - pkg_a['priority'];
	    });
	
	// Collect the actual stored functions by priority.
	var cb_fun_list = [];
	for( var cbi = 0; cbi < cb_id_list.length; cbi++ ){
	    var cb_id = cb_id_list[cbi];
	    var to_run =
		registry_anchor.callback_registry[category][cb_id]['runner'];
	    cb_fun_list.push(to_run);
	    // ll('callback: ' + category + ', ' + cb_id + ', ' +
	    //    this.callback_registry[category][cb_id]['priority']);
	}
	
	return cb_fun_list;
    };

    /*
     * Function: apply_callbacks
     *
     * Generic runner for prioritized callbacks with various arguments
     * and an optional change in context..
     *
     * Parameters: 
     *  category - string
     *  arg_list - a list of arguments to pass to the function in the category
     *  context - *[optional]* the context to apply the arguments in
     *
     * Returns: 
     *  n/a
     */
    this.apply_callbacks = function(category, arg_list, context){

	// Run all against registered functions.
	var callbacks = registry_anchor.get_callbacks(category);
	for( var ci = 0; ci < callbacks.length; ci++ ){
	    var run_fun = callbacks[ci];
	    //run_fun(arg_list);
	    run_fun.apply(context, arg_list);
	}
    };
};
