////////////
////
//// org.bbop.Crosshair
////
//// Purpose: Add a vanishing point crosshair to the overlay.
////
//// TODO/BUG: The vanishing point seems to be variable, to the
//// crosshair is going to have to be pretty smart...
////
//////////


// Module and namespace checking.
if ( typeof org == "undefined" ){ org = {}; }
if ( typeof org.bbop == "undefined" ){ org.bbop = {}; }
if ( typeof org.bbop.Crosshair == "undefined" ){ org.bbop.Crosshair = {}; }


//
org.bbop.Crosshair = function(overlay_context_id, x_center, y_center){
  
  var overlay = document.getElementById(overlay_context_id);
  var svg_lib = new org.bbop.SVG();
  
  // Create a background bounding box for the control panels.
  overlay.appendChild(svg_lib.makeCrosshair(358, 208, 100,
					    'black', '.10'));
};
