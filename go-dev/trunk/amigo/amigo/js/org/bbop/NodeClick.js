////////////
////
//// org.bbop.NodeClick
////
//// Purpose:
////
//////////


// Module and namespace checking.
if ( typeof org == "undefined" ){ org = {}; }
if ( typeof org.bbop == "undefined" ){ org.bbop = {}; }
if ( typeof org.bbop.NodeClick == "undefined" ){ org.bbop.NodeClick = {}; }


//
org.bbop.NodeClick = function(callback_function){

  var g_nodes = document.getElementsByTagName('g');
  for( var i = 0; i < g_nodes.length; i++ ){
    
    var g = g_nodes[i];

    // Use node this to hunt down top level nodes in the graph.
    // TODO: This is very nasty--we're actually looking at all of the
    // nodes and then just examining the ones that are g. Can't think
    // of another way though...
    var g_class = g.getAttribute('class') ;
    if( g_class == 'node' ){

      $(g).addEvent('click',
		    function(e) {
		      e = new Event(e).stop();  
		      callback_function(e);
		    });
    }
  }
};
