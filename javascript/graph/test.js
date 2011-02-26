////
//// A trivial testing framework for JS. See test.tests.js for usage.
////
//// Notes: cannot depend on core.js
////


// Module and namespace checking.
if ( typeof bbop == "undefined" ){ bbop = {}; }

// 
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

    //
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

    // NOTE/WARNING: This is a very shallow comparison function.
    function _same_hash(hash1, hash2){

	var same_p = true;
	
	// See if the all of the keys in hash1 are defined in hash2
	// and that they have the same ==.
	for( var k1 in hash1 ){
	    if( ! hash2[k1] ||
		hash1[k1] != hash2[k1] ){
		same_p = false;
		break;
	    }
	}

	// If there is still no problem...
	if( same_p ){

	    // Reverse of above.
	    for( var k2 in hash2 ){
		if( ! hash1[k2] ||
		    hash2[k2] != hash1[k2] ){
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
	var head1 = tmp1[0];
	var args1 = tmp1[1].split('&');
	var sorted_args1 = args1.sort();
	var tmp2 = str2.split('?');
	var head2 = tmp2[0];
	var args2 = tmp2[1].split('&');
	var sorted_args2 = args2.sort();

	// Compare heads and arguments.
	var retval = false;
	if( head1 == head2 &&
	    _same_array(sorted_args1, sorted_args2) ){
	    retval = true;
	}
	return retval;
    }

    ///
    /// End-user comparisions and asserions.
    ///

    //
    function _is_simple_same(question, answer, msg){
	_complete(question == answer, msg);
    };
    this.is_same_atom = _is_simple_same;

    //
    this.is_different_atom = function(question, answer, msg){
	_complete(question != answer, msg);
    };

    //
    this.is_defined = function(thing, msg){
	if( thing ){
	    _complete(true, msg);
	}else{
	    _complete(false, msg);
	}
    };

    //
    this.is_true = function(bool, msg){
	if( bool == true ){
	    _complete(true, msg);
	}else{
	    _complete(false, msg);
	}
    };

    // Try and see if two links are functionally equivalent.
    this.is_same_url = function(link1, link2, msg){
	_complete(_link_comp(link1, link2), msg);
    };    

    // Try and see if two links are functionally different.
    this.is_different_url = function(link1, link2, msg){
	_complete(! _link_comp(link1, link2), msg);
    };    

    //
    this.is_same_hash = function(hash1, hash2, msg){
	_complete(_same_hash(hash1, hash2), msg);
    };
    //
    this.is_different_hash = function(hash1, hash2, msg){
	_complete(! _same_hash(hash1, hash2), msg);
    };
};
