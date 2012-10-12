////////////
////
//// bbop.parser.newick
////
//// Purpose: Newick tree parser for the strange PANTHER dialect.
//// 
//// NOTE: Should not/will not work on other dialects, modifications
//// (mostly simplifications) should do it.
////
//// Taken name spaces:
////    bbop.parse.newick.*
////
//////////


// Module and namespace checking.
bbop.core.require('bbop', 'core');
bbop.core.namespace('bbop', 'parse', 'newick');


///
///  ...
///

bbop.core.DEBUG = true;;
function ll (str){
    bbop.core.kvetch(str);
}


// List of possible tokens types.
bbop.parse.newick.token_type = {};
bbop.parse.newick.token_type.STRING = 0;
bbop.parse.newick.token_type.LEFT = 1;
bbop.parse.newick.token_type.RIGHT = 2;
bbop.parse.newick.token_type.SEMI = 3;
bbop.parse.newick.token_type.COLON = 4;

// Create a token of type, with possible value of string.
bbop.parse.newick.token = function(type, string){

    this._type = type;
    this._string = null;

    if( type == bbop.parse.newick.token_type.STRING ){
	this._string = string;
    }else if( type == bbop.parse.newick.token_type.LEFT ){
	this._string = '(';
    }else if( type == bbop.parse.newick.token_type.RIGHT ){
	this._string = ')';
    }else if( type == bbop.parse.newick.token_type.SEMI ){
	this._string = ';';
    }else if( type == bbop.parse.newick.token_type.COLON ){
	this._string = ':';
    }

    this.copy = function(){
	return true;	
    };

};
// 
bbop.parse.newick.token.prototype.type = function(){ return this._type; };
bbop.parse.newick.token.prototype.string = function(){ return this._string; };
// 
bbop.parse.newick.token.prototype.eq = function(in_token){
    var retval = false;
    if( in_token.type() == this.type() &&
	in_token.string() == this.string() ){
	retval = true;
    }
    return retval;
};



//
bbop.parse.newick.tokenizer = function(in_str){

    this._stream = new Array();



//    }

    //
    function apply_join(in_array, join_item){
	var acc = new Array();
	for( var i = 0; i < in_array.length -1; i++ ){
	    acc.push();
	}
	return acc;	
    }

    // 
    function apply_split(in_token_array, split_char){
	var acc = new Array();
	for( var i = 0; i < in_token_array.length; i++ ){
	    var current_token = in_str_array[i];
	    // If it's a string cut it up, otherwise, pass it through.
	    if( current_token.type == this.STRING ){
		var str = current_token.string;
		var splot = str.split(split_char);
		for( var j = 0; j < splot.length; j++ ){
		    var new_str = splot[j];
		    
		}
		//acc.push(current_token);
	    }else{
		acc.push(current_token);
	    }
	}
	return acc;
    }

    //
    var initial_token = make_token(this.STRING, in_str);
    this._stream = apply_split([initial_token], '(');

};

// From: http://en.wikipedia.org/wiki/Newick_format
// Tree --> Subtree ";" | Branch ";"
// Subtree --> Leaf | Internal
// Leaf --> Name
// Internal --> "(" BranchSet ")" Name
// BranchSet --> Branch | BranchSet "," Branch
// Branch --> Subtree Length
// Name --> empty | string
// Length --> empty | ":" number
bbop.parse.newick.parser = function(){

    function rule_check(){
	// ...
    }

    function _tokenize(in_str){
	
	ll('current(' + in_str.length + '): ' + in_str);
	
	var retval = null;

	if( typeof(in_str) == 'string' && in_str.length > 0 ){

	    var first = in_str[0];
	    var last = in_str[in_str.length -1];

	    if(  first == '(' && last == ';' ){
		ll("TERMINAL NODE");		
		// Walk-back to paren.
		
		
		for( var wbi = in_str.length -1; wbi > 0; wbi-- ){
		    if( in_str[wbi] == ')' ){
			break;
		    }
		}

	    }else{
		ll("SYNTAX ERROR?");
	    }
	}else{
	    ll("NON-START");
	}
    };
    this.tokenize = _tokenize;

};

// // Prototype getter/setters.
// bbop.model.node.prototype.id = function(value){
//     if(value) this._id = value; return this._id; };
