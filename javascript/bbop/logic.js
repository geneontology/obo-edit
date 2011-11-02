////////////
////
//// bbop.logic
////
//// BBOP object to try and take some of the pain out of managing
//// the boolean logic that seems to show up periodically. Right now
//// mostly aimed at dealing with Solr/GOlr.
////
//// Taken name spaces:
////    bbop.logic
////
//// Anatomy of a core data bundle.
//// 
////   data_bundle => {op: arg}
////   op => '__AND__', '__OR__', '__NOT__'
////   arg => <string>, array, data_bundle
////   array => [array_item*]
////   array_item => <string>, data
//// 
//// Ex:
//// {and: [{or: ...}, {or: ...}, {and: ...} ]}
//// var filters = {'and': []};
////
//// TODO: parens between levels
////
//////////

bbop.core.require('bbop', 'core');
bbop.core.require('bbop', 'logger');
bbop.core.namespace('bbop', 'logic');

// NOTE: during processing, binary operators with a single argument
// cease to exist as they will never make it to output.
bbop.logic = function(default_conjunction){
    this._is_a = 'bbop.logic';

    // Add logging.
    var logger = new bbop.logger();
    //logger.DEBUG = true;
    logger.DEBUG = false;
    function ll(str){ logger.kvetch(str); };

    var logic_anchor = this;

    // // Handling conjunctions.
    // this._and = '__AND__';
    // this._or = '__OR__';
    // this._not = '__NOT__';
    // function _is_token(possible_token){
    // 	var retval = false;
    // 	if( possible_token == this._and ||
    // 	    possible_token == this._or ||
    // 	    possible_token == this._not ){
    // 	   retval = true; 
    // 	}
    // 	return retval;
    // }
    // // Cinvert the internal
    // function _usable

    // // Set the internal default conjunction. Default to "and".
    // if( ! default_conjunction ){
    // 	default_conjunction = this._and;
    // }else if( default_conjunction == this._or ){
    // 	default_conjunction = this._or;
    // }else{
    // 	default_conjunction = this._and;
    // }
    if( ! default_conjunction ){
    	default_conjunction = 'and';
    }
    this.default_conjunction = default_conjunction;

    // Set initial state.
    // ie: this._bundle = {'__AND__': []};
    //this._bundle = {};
    //this._bundle[this.default_conjunction] = [];
    var _empty = function(){
	logic_anchor._bundle = {};
	logic_anchor._bundle[logic_anchor.default_conjunction] = [];
    };
    _empty();

    // 
    //this.and = function(){
    //this.or = function(){
    //this.not = function(){
    this.add = function(foo){

	// Add things a little differently if it looks like a bit of
	// logic.
	if(  bbop.core.what_is(foo) == 'bbop.logic' ){
	    this._bundle[this.default_conjunction].push(foo._bundle);
	}else{
	    this._bundle[this.default_conjunction].push(foo);
	}
    };

    // Negate the current stored logic.
    // TODO/BUG: I think this might cause an unreleasable circular
    // reference.
    this.negate = function(){
	var nega = {};
	nega['not'] = this._bundle;
	this._bundle = nega;
    };
    
    // Walk the data structure...
    this._read_walk = function(data_bundle, lvl){
	
	ll("LRW: with: " + bbop.core.dump(data_bundle));

	// If level is not defined, we just started and we're on level
	// one, the first level.
	var l_enc = '(';
	var r_enc = ')';
	if( typeof(lvl) == 'undefined' ){
	    lvl = 1;
	    l_enc = '';
	    r_enc = '';
	}	

	var read = '';
	
	// The task of walking is broken into the terminal case (a
	// string) or things that we need to operate on (arrays or
	// sub-data_bundles).
	if( bbop.core.what_is(data_bundle) == 'string' ){
	    ll("LRW: trigger string");
	    read = data_bundle;
	}else{
	    ll("LRW: trigger non-string");

	    // Always single op.
	    var op = bbop.core.get_keys(data_bundle)[0];
	    var arg = data_bundle[op];

	    // We can treat the single data_bundle/string case like a
	    // degenerate array case.
	    if( ! bbop.core.is_array(arg) ){
		arg = [arg];
	    }

	    // Recure through the array and join the results with the
	    // current op.
	    //ll('L: arg: ' + bbop.core.what_is(arg));
	    var stack = [];
	    bbop.core.each(arg, function(item, i){
			       stack.push(logic_anchor._read_walk(item, lvl+1));
			   });

	    // Slightly different things depending on if it's a unary
	    // or binary op.
	    if( op == 'not' ){
		// TODO: I believe that it should no be possible
		// (i.e. policy by code) to have a 'not' with more
		// that a single argument.
		read = op + ' ' + stack.join('');
	    }else{
		read = l_enc + stack.join(' ' + op + ' ') + r_enc;
	    }
	}

	
	ll("LRW: returns: " + read);
	return read;
    };

    // Dump the current data out to a URL.
    this.to_string = function(){
	return logic_anchor._read_walk(logic_anchor._bundle);
    };

    // TODO: Dump the current data out to a URL.
    this.url = function(){
	return logic_anchor._read_walk(logic_anchor._bundle);
    };

    // Empty/reset self.
    // Staggered declaration so I can use it above during initialization.
    this.empty = _empty;

    // TODO
    // Parse an incoming string into the internal data 
    this.parse = function(){
	return null;
    };

};