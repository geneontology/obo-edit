////////////
////
//// org.bbop.Viewer
////
//// Purpose:
////
//////////


// Module and namespace checking.
if ( typeof org == "undefined" ){ org = {}; }
if ( typeof org.bbop == "undefined" ){ org.bbop = {}; }
if ( typeof org.bbop.Viewer == "undefined" ){ org.bbop.Viewer = {}; }


//
org.bbop.Viewer = function(rg_graph_id,
			   matrix_group_id){//,
  
  var rg_graph = document.getElementById(rg_graph_id);
  var transform_group = document.getElementById(matrix_group_id);

  // Dragging vars.
  var dragging = false;
  var dragStartX = -1;
  var dragStartY = -1;
  
  // Get the height and width of the SVG.
  //var svg_w_string = rg_graph.getAttribute("width");
  //var svg_h_string = rg_graph.getAttribute("height");
  //var svg_width=parseFloat(svg_w_string.substring(0,svg_w_string.length -2));
  //var svg_height=parseFloat(svg_h_string.substring(0,svg_h_string.length -2));

  var affine = new org.bbop.AffineSVG();
  //kvetch.say('transform_group: ' + transform_group.getAttribute("transform"));
  //kvetch.say('affine: ' + affine.toString());
  affine.set_default(transform_group.getAttribute("transform"));
  transform_group.setAttribute("transform", affine.toString());

  var scaling_factor = 1.5;

  // Add mousewheel events.
  Element.Events.extend({
      'wheelup': { type: Element.Events.mousewheel.type,
       map: function(event){
	  event = new Event(event);
	  if (event.wheel >= 0) this.fireEvent('wheelup', event)
	}
      },
 
      'wheeldown': { type: Element.Events.mousewheel.type,
       map: function(event){
	  event = new Event(event);
	  if (event.wheel <= 0) this.fireEvent('wheeldown', event)
	}
      }
    });
  

  // Probably a better implementation for when WebKit catches up.
  //     if( dir == 'in' ){
  //      rg_graph.currentScale = rg_graph.currentScale * 1.5;
  //     }else if( dir == 'out' ){
  //      rg_graph.currentScale = rg_graph.currentScale / 1.5;
  //     }
  function zoom(dir){
    //
    //kvetch.say(affine.toString());
    if( dir == 'in' || dir == 'out' ){
      affine.zoom( { direction: dir, factor: scaling_factor } );
      transform_group.setAttribute("transform", affine.toString());
    }
    //kvetch.say(affine.toString());

    // TODO: change cookie to reflect affine string.
    
  }


  // Tie the new zoom and mousewheel together.
  // 
  $(rg_graph_id).addEvents({
      'wheelup': function(e) {
	e = new Event(e).stop();
	zoom('in');
      },
	  
      'wheeldown': function(e) {
	e = new Event(e).stop();  
	zoom('out');
      }
    });


  // Add event handlers to this div.
  // TODO: Replace this with MooTools version.
  rg_graph.onmousedown = function(event){
    
    // Handle IE stooooopidity.
    var e = event || window.event;
    
    dragging = true;
    
    dragStartX = e.clientX;
    dragStartY = e.clientY;
    
    // Prevent default browser response to event. (TODO: understand this.)
    return false;
  };
  
  
  // Add event handlers to this div.
  // BUG/TODO: Isn't there a proper way to do event registration? Observer?
  rg_graph.onmouseup = function(event){

    dragging = false;
    //affine.toAlert();
  };
  

  /**
   * Since we're tying this to a global object, it traps our TileFrame
   * instance in a closure that we can reference in the future without
   * fear of holy retribution.
   */
  // TODO: Replace this with MooTools version.
  rg_graph.onmousemove = function(event){
    
    if( dragging == true ){
      
      // Handle IE stooooopidity.
      var e = event || window.event;
    
      var dX = e.clientX - dragStartX;
      var dY = e.clientY - dragStartY;
      
      // Update for next firing.
      dragStartX = e.clientX;
      dragStartY = e.clientY;
      
      // Wow! That's it!
      //svg.currentTranslate.x = rg_graph.currentTranslate.x + dX
      //svg.currentTranslate.y = rg_graph.currentTranslate.y + dY
      affine.move( { x: dX, y: dY } );
      transform_group.setAttribute("transform", affine.toString());
    }
  };


  // Set mootools keystrokes.
  $(rg_graph_id).onkeydown = function(event){

    //event is now the Event class.
    // BUG: there is something wrong with mootools character
    // converter.
    // Stop event.
    //var e = new Event(event).stop();
    var e = new Event(event);
    var key = e.key; // May be interesting like 'delete', etc.
  
    if( e.control ){

      //
      if( key == 'a' ||
	  key == 'z' ||
	  key == 's' ||
	  key == 'd' ||
	  key == 'f' ||
	  key == 'g' ||
	  key == 'e' ||
	  key == 'v'){
	e.stop();
      }

      //
      if( key == 'a' ){
	// In.
	zoom('in');
      }else if( key == 'z' ){
	// Out.
	zoom('out');
      }else if( key == 's' ){
	// Left.
	affine.move( { x: -50, y: 0 } );
	transform_group.setAttribute("transform", affine.toString());
      }else if( key == 'g' ){
	// Right.
	affine.move( { x: 50, y: 0 } );
	transform_group.setAttribute("transform", affine.toString());
      }else if( key == 'd' ){
	// Up.
	affine.move( { x: 0, y: -50 } );
	transform_group.setAttribute("transform", affine.toString());
      }else if( key == 'f' ){
	// Down.
	affine.move( { x: 0, y: 50 } );
	transform_group.setAttribute("transform", affine.toString());
      }else if( key == 'x' ){
	// Reset.
	affine.reset_to_default();
	transform_group.setAttribute("transform", affine.toString());
      }else if( key == 'e' ){
	// Report.
	kvetch.say(affine.toString());
	//affine.toAlert();
      }else if( key == 'v' ){
	// Report.
	//kvetch.say(affine.toString());
	affine.toAlert();
      }

    }else{

      // Otherwise: ignore the input.

    }
  };
};
