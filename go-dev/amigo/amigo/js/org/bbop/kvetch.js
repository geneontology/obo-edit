//////////
//
// org.bbop.kvetch
//
// Purpose: Simplify debugs, warnings, etc...
//
// DEPRICATED: Everything has been moved (and improved) into org.bbop.amigo.
//
//////////


// Module and namespace checking.
if ( typeof org == "undefined" ){ org = {}; }
if ( typeof org.bbop == "undefined" ){ org.bbop = {}; }
//if ( typeof org.bbop.kvetch == "undefined" ){ org.bbop.kvetch = {}; }


//
org.bbop.kvetch = function(){

  var DEBUG = true;
  //var DEBUG = false;

  // Browser whining support.
  var sayer = alert; // Default to alert if we cant find any other
		     // output methods.

  // Check: Opera, FF, and Safari.
  if( typeof(opera) != 'undefined' &&
      typeof(opera.postError) != 'undefined' ){
    sayer = opera.postError;
  }else if( typeof(dump) != 'undefined' ){
    // From developer.mozilla.org:
    // To see the dump output you have to enable it by setting the
    // preference browser.dom.window.dump.enabled to true. You can set
    // the preference in about:config or in a user.js file. Note: this
    // preference is not listed in about:config by default, you may
    // need to create it (right-click the content area -> New ->
    // Boolean).
    sayer = dump;
  }else if( typeof(window) != 'undefined' &&
	    typeof(window.console) != 'undefined' ){
    // From developer.apple.com:
    //Safari's "Debug" menu allows you to turn on the logging of
    //JavaScript errors. To display the debug menu in Mac OS X, open a
    //Terminal window and type: "defaults write com.apple.Safari
    //IncludeDebugMenu 1"

    sayer = window.console.log;
  } 

  // Say what needs to be said.
  this.say = function(string){  

    if( DEBUG ){
      sayer(string + "\n");
    }
  };
};
