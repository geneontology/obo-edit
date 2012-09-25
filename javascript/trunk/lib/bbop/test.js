/* 
 * Package: test.js
 * 
 * Namespace: bbop.test
 * 
 * A trivial testing framework for JS. See test.tests.js for usage.
 * 
 *  Note: this cannot depend on core.js (it tests that), so some stuff
 *  may be dupped. On the other hand, we can test ourselves--see
 *  test.js.tests.
 */

// Module and namespace checking.
if ( typeof bbop == "undefined" ){ bbop = {}; }

/*
 * Constructor: test
 * 
 * Contructor for the BBOP JS unit test system.
 * 
 * Arguments:
 *  n/a
 * 
 * Returns:
 *  BBOP test suite object
 */
bbop.test = function(){

    ///
    /// Accounting and reporting.
    ///

    var test_number = 1;
    var tests_passed = 0;
    var tests_failed = 0;
    function _incr_tests(){ test_number = test_number + 1; }
    function _incr_passed(){ tests_passed = tests_passed + 1; }
    function _incr_failed(){ tests_failed = tests_failed + 1; }
    function _incr_failed(){ tests_failed = tests_failed + 1; }
    function _complete(bool, msg){
	if( bool ){
	    if( msg ){
		print('Test ' + test_number + ' passed: ' + msg + '.');
	    }else{
		print('Test ' + test_number + ' passed.');
	    }
	    _incr_passed();
	}else{
	    if( msg ){
		print('FAIL: Test ' + test_number + ' failed: ' + msg + '.');
	    }else{
		print('FAIL: Test ' + test_number + ' failed.');
	    }
	    _incr_failed();
	}
	test_number++;	
    }

    /*
     * Function: report
     *
     * Print a report about what happened during the tests.
     *
     * Parameters: 
     *  n/a
     *
     * Returns: 
     *  n/a; but prints the report as a side-effect
     */
    this.report = function(){
	if( tests_passed + 1 == test_number ){
	    print('* All tests passed.');
	}else{
	    print('* Tests passed: ' + tests_passed);
	    print('* Tests failed: ' + tests_failed);
	}
    };

    ///
    /// Internal helper functions--different kinds of comparisions.
    ///

    //
    function _same_array(one, two){
	var retval = true;
	if( one.length != two.length ){
	    retval = false;
	}else{
	    for( var i = 0; i < one.length; i++ ){
		if( one[i] != two[i] ){
		    retval = false;
		    break;
		}
	    }
	}
	return retval;
    }

    // Looking at array as sets of...something.
    function _same_set(set1, set2){
	var h1 = {};
	var h2 = {};
	for( var h1i = 0; h1i < set1.length; h1i++ ){ h1[set1[h1i]] = 1; }
	for( var h2i = 0; h2i < set2.length; h2i++ ){ h2[set2[h2i]] = 1; }
	return _same_hash(h1, h2);
    }

    // NOTE/WARNING: This is a very shallow comparison function.
    function _same_hash(hash1, hash2){

	var same_p = true;
	
	// See if the all of the keys in hash1 are defined in hash2
	// and that they have the same ==.
	for( var k1 in hash1 ){
	    if( typeof hash2[k1] === 'undefined' ||
		hash1[k1] !== hash2[k1] ){
		same_p = false;
		break;
	    }
	}

	// If there is still no problem...
	if( same_p ){

	    // Reverse of above.
	    for( var k2 in hash2 ){
		if( typeof hash1[k2] === 'undefined' ||
		    hash2[k2] !== hash1[k2] ){
		    same_p = false;
		    break;
		}
	    }
	}
	
	return same_p;
    }

    // TODO: This could probably be done better.
    function _link_comp(str1, str2){

	// Decompose links and arguments.
	var tmp1 = str1.split('?');
	var head1 = '';
	var args1 = [];
	if( ! tmp1[1] ){ // nothing before '?'
	    args1 = tmp1[0].split('&');
	}else{ // normal structure
	    head1 = tmp1[0];
	    args1 = tmp1[1].split('&');
	}
	var sorted_args1 = args1.sort();

	var tmp2 = str2.split('?');
	var head2 = '';
	var args2 = [];
	if( ! tmp2[1] ){ // nothing before '?'
	    args2 = tmp2[0].split('&');
	}else{ // normal structure
	    head2 = tmp2[0];
	    args2 = tmp2[1].split('&');
	}
	var sorted_args2 = args2.sort();

	// var tmp2 = str2.split('?');
	// var head2 = tmp2[0];
	// var args2 = tmp2[1].split('&');
	// var sorted_args2 = args2.sort();

	// Compare heads and arguments.
	var retval = false;
	if( head1 == head2 &&
	    _same_array(sorted_args1, sorted_args2) ){
	    retval = true;
	}
	return retval;
    }

    // Walk through the list and see if it's there.
    // If compareator is not defined, just to atom comparison.
    function _in_list(in_item, list, comparator){

	var retval = false;
	for(var li = 0; li < list.length; li++ ){
	    var list_item = list[li];

	    if( comparator ){
		var comp_op = comparator(in_item, list_item);
		if( comp_op && comp_op == true ){
		    retval = true;
		}
	    }else{
		if( in_item == list_item ){
		    retval = true;
		}
	    }
	}

	return retval;
    }

    // Basically asking if you can make the target string from the
    // base string with the add_str added into it somewhere. Strange,
    // but another way of looking at URL creation in some cases.
    function _is_string_embedded(target_str, base_str, add_str){

	// Walk through all of ways of splitting base_str and add
	// add_str in there to see if we get the target_str.
	var retval = false;
	for(var si = 0; si <= base_str.length; si++ ){
	    
	    var car = base_str.substr(0, si);
	    var cdr = base_str.substr(si, base_str.length);
	    //print(car + "|" + add_str + "|" + cdr);
	    if( car + add_str + cdr == target_str){
		retval = true;
		break;
	    }
	}
	return retval;
    }

    ///
    /// End-user comparisions and assertions.
    ///

    /*
     * Function: is_same_atom
     *
     * Test whether two atoms are the same.
     *
     * Parameters: 
     *  question - the atom to test
     *  answer - the expected atom
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    function _is_simple_same(question, answer, msg){
	_complete(question == answer, msg);
    };
    this.is_same_atom = _is_simple_same;

    /*
     * Function: is_different_atom
     *
     * A negative version of <is_same_atom>.
     *
     * Parameters: 
     *  question - the atom to test
     *  answer - the unexpected atom
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_different_atom = function(question, answer, msg){
	_complete(question != answer, msg);
    };

    /*
     * Function: is_defined
     *
     * Test whether a value is defined.
     *
     * Parameters: 
     *  thing - the value to test for being defined
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_defined = function(thing, msg){
	if( thing ){
	    _complete(true, msg);
	}else{
	    _complete(false, msg);
	}
    };

    /*
     * Function: is_not_defined
     *
     * A negative version of <is_defined>.
     *
     * Parameters: 
     *  thing - the value to test for being undefined
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_not_defined = function(thing, msg){
	if( thing ){
	    _complete(false, msg);
	}else{
	    _complete(true, msg);
	}
    };

    /*
     * Function: is_true
     *
     * Test whether a value is true.
     *
     * Parameters: 
     *  bool - the variable to test
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_true = function(bool, msg){
	if( bool == true ){
	    _complete(true, msg);
	}else{
	    _complete(false, msg);
	}
    };

    /*
     * Function: is_false
     *
     * A negative version of <is_true>.
     *
     * Parameters: 
     *  bool - the variable to test
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_false = function(bool, msg){
	if( bool == false ){
	    _complete(true, msg);
	}else{
	    _complete(false, msg);
	}
    };

    /*
     * Function: is_x_greater_than_y
     *
     * Test whether one value is greate than another. Uses the
     * standard ">" operator.
     *
     * Parameters: 
     *  x_thing - the expected greater value
     *  y_thing - the expected lesser value
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_x_greater_than_y = function(x_thing, y_thing, msg){
	if( x_thing > y_thing ){
	    _complete(true, msg);
	}else{
	    _complete(false, msg);
	}
    };

    /*
     * Function: is_same_url
     *
     * Test whether two links are functionally equivalent.
     *
     * Parameters: 
     *  link1 - url
     *  link2 - url
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_same_url = function(link1, link2, msg){
	_complete(_link_comp(link1, link2), msg);
    };    

    /*
     * Function: is_different_url
     *
     * A negative version of <is_same_url>.
     *
     * Parameters: 
     *  link1 - url
     *  link2 - url
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_different_url = function(link1, link2, msg){
	_complete(! _link_comp(link1, link2), msg);
    };    

    // /*
    //  * Function: is_same_url_by_assembly
    //  *
    //  * Test whether two URLs are functionally equivalent.
    //  *
    //  * Parameters: 
    //  *  link - url
    //  *  base - string
    //  *  psuedo_assembly - hash 
    //  *  msg - *[optional]* informational message about test
    //  *
    //  * Returns: 
    //  *  n/a
    //  */
    // function _psuedo_assmble(assembly){
    // 	var retval = '';
    // 	for( var k2 in hash2 ){
    // 	return retval;
    // }
    // this.is_same_url_by_assembly = function(link, base,
    // 					    psuedo_assembly, msg){
    // 	_complete(_link_comp(link,
    // 			     base + bbop.core.get_assemble(assembly)),
    // 		  msg);
    // };    

    /*
     * Function: is_same_set
     *
     * Test whether two sets (as atomic arrays) are the same.
     *
     * Parameters: 
     *  set1 - set (as array)
     *  set2 - set (as array)
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_same_set = function(set1, set2, msg){
	_complete(_same_set(set1, set2), msg);
    };

    /*
     * Function: is_different_set
     *
     * A negative version of <is_same_set>.
     *
     * Parameters: 
     *  set1 - set (as array)
     *  set2 - set (as array)
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_different_set = function(set1, set2, msg){
	_complete(! _same_set(set1, set2), msg);
    };

    /*
     * Function: is_same_hash
     *
     * Test whether two simple atomic hashes are the same.
     *
     * Parameters: 
     *  hash1 - hash
     *  hash2 - hash
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_same_hash = function(hash1, hash2, msg){
	_complete(_same_hash(hash1, hash2), msg);
    };

    /*
     * Function: is_different_hash
     *
     * A negative version of <is_same_hash>.
     *
     * Parameters: 
     *  hash1 - hash
     *  hash2 - hash
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_different_hash = function(hash1, hash2, msg){
	_complete(! _same_hash(hash1, hash2), msg);
    };

    /*
     * Function: is_in_list
     *
     * Test whether an item is in a list (array).
     *
     * Parameters: 
     *  item - the value to test
     *  list - the array to test in
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_in_list = function(item, list, msg){
	_complete(_in_list(item, list), msg);
    };

    /*
     * Function: is_not_in_list
     *
     * A negative version of <is_in_list>.
     *
     * Parameters: 
     *  item - the value to test
     *  list - the array to test in
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_not_in_list = function(item, list, msg){
	_complete(! _in_list(item, list), msg);
    };

    /*
     * Function: is_in_list_diy
     *
     * A DIY version of is_in_list. In this case, you can pass your
     * own comparison function to check the item against the list.
     *
     * Parameters: 
     *  item - the value to test
     *  list - the array to test in
     *  comp - the comparison function; like: function(in_item, list_item){...}
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_in_list_diy = function(item, list, comp, msg){
	_complete(_in_list(item, list, comp), msg);
    };

    /*
     * Function: is_not_in_list_diy
     *
     * A negative version of <is_in_list_diy>.
     *
     * Parameters: 
     *  item - the value to test
     *  list - the array to test in
     *  comp - the comparison function; like: function(in_item, list_item){...}
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_not_in_list_diy = function(item, list, comp, msg){
	_complete(! _in_list(item, list, comp), msg);
    };

    /*
     * Function: is_string_embedded
     *
     * Test whether a target string (target_str) can be made by
     * embedding a string (added_str) into a base string (base_str).
     * 
     * Useful in certain cases when checking URLs.
     *
     * Parameters: 
     *  target_str - the value to test
     *  base_str - the expected value
     *  added_str - the expected value
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_string_embedded = function(target_str, base_str, added_str, msg){
	_complete(_is_string_embedded(target_str, base_str, added_str), msg);
    };

    /*
     * Function: is_string_not_embedded
     *
     * A negative version of <is_string_embedded>.
     *
     * Parameters: 
     *  target_str - the value to test
     *  base_str - the expected value
     *  added_str - the expected value
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.is_string_not_embedded =
	function(target_str, base_str, added_str, msg){
	    _complete(! _is_string_embedded(target_str, base_str, added_str),
		      msg);
	};

    /*
     * Function: pass
     *
     * Always return test as true--useful when testing using control
     * structures and the like.
     *
     * Parameters: 
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.pass = function(msg){
	_complete(true, msg);
    };

    /*
     * Function: fail
     *
     * Always return test as false--useful when testing using control
     * structures and the like.
     *
     * Parameters: 
     *  msg - *[optional]* informational message about test
     *
     * Returns: 
     *  n/a
     */
    this.fail = function(msg){
	_complete(false, msg);
    };
};
