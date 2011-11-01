////////////
////
//// bbop.registry
////
//// BBOP generic lightweight listener/callback registry system.
////
//// Taken name spaces:
////    bbop.registry
////
//////////

bbop.core.require('bbop', 'core');
bbop.core.namespace('bbop', 'registry');

// Takes a list of event categories as strings.
bbop.registry = function(evt_list){
    this._is_a = 'bbop.registry';

    var anchor = this;

    // Handle the registration of call functions to get activated
    // after certain events.
    this.callback_registry = {};
    bbop.core.each(evt_list, function(item, i){
		       anchor.callback_registry[item] = {};
		   });
    
    // Remove the specified function from the registry, with an
    // optional relative priority against other callback functions.
    this.register = function(category, function_id, in_function, in_priority){

	// Only these categories.
	if( typeof(anchor.callback_registry[category]) == 'undefined'){
	    throw new Error('cannot register, unknown category');
	}

	// The default priority is 0.
	var priority = 0;
	if( in_priority ){ priority = in_priority; }

	anchor.callback_registry[category][function_id] =
	    {
		runner: in_function,
		priority: priority
	    };
    };

    // Remove the specified function from the registry.
    this.unregister = function(category, function_id){
	if( anchor.callback_registry[category] &&
	    anchor.callback_registry[category][function_id] ){
		delete anchor.callback_registry[category][function_id];
            }
    };
    
    // Generic getter for callback functions, returns by priority.
    this.get_callbacks = function(category){

	var cb_id_list =
	    bbop.core.get_keys(anchor.callback_registry[category]);
	// Sort callback list according to priority.
	var ptype_anchor = this;
	cb_id_list.sort(function(a, b){  
			    var pkg_a =
				ptype_anchor.callback_registry[category][a];
			    var pkg_b =
				ptype_anchor.callback_registry[category][b];
			    return pkg_b['priority'] - pkg_a['priority'];
			});
	
	// Collect the actual stored functions by priority.
	var cb_fun_list = [];
	for( var cbi = 0; cbi < cb_id_list.length; cbi++ ){
	    var cb_id = cb_id_list[cbi];
	    var to_run = anchor.callback_registry[category][cb_id]['runner'];
	    cb_fun_list.push(to_run);
	    // ll('callback: ' + category + ', ' + cb_id + ', ' +
	    //    this.callback_registry[category][cb_id]['priority']);
	}
	
	return cb_fun_list;
    };

    // Generic runner for prioritized callbacks with arg.
    this.apply_callbacks = function(category, arg_list, context){

	// Run all against registered functions.
	var callbacks = anchor.get_callbacks(category);
	for( var ci = 0; ci < callbacks.length; ci++ ){
	    var run_fun = callbacks[ci];
	    //run_fun(arg_list);
	    run_fun.apply(context, arg_list);
	}
    };
};
