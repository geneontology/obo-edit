//////////
//
// org.bbop.go
//
// Purpose: Some GO specific things that we need.
//
//////////


// Module and namespace checking.
if ( typeof org == "undefined" ){ org = {}; }
if ( typeof org.bbop == "undefined" ){ org.bbop = {}; }
if ( typeof org.bbop.go == "undefined" ){ org.bbop.go = {}; }


//
org.bbop.go = function(){

  // Returns one or zero depending on whether or not the incoming
  // string is a experimental ev code.
  this.exp_evcode_p = function(string){

    var experimental_evcodes =
    {
      'IDA' : true,
      'IMP' : true,
      'IGI' : true,
      'IEP' : true,
      'IPI' : true
    };
    
    var p = false;
    var res = experimental_evcodes[string] || false;
    if( res ){
      p = true;
    }
    return p;
  };
};
