//////////
//
// org.bbop.AffineSVG
//
// Purpose: 
//
//////////


// Module and namespace checking.
if ( typeof(org) == "undefined" ){ org = {}; }
if ( typeof(org.bbop) == "undefined" ){ org.bbop = {}; }
if ( typeof(org.bbop.AffineSVG) == "undefined" ){ org.bbop.AffineSVG = {}; }
var kvetch = new org.bbop.kvetch();
//kvetch.say('org.bbop.AffineSVG.js');


//
org.bbop.AffineSVG = function(){
  
  var affine_array = new Array(6);
  affine_array[0] = 1.0; // scale x
  affine_array[1] = 0.0; // //unused
  affine_array[2] = 0.0; // //unused
  affine_array[3] = 1.0; // scale y
  affine_array[4] = 0.0; // transform x
  affine_array[5] = 0.0; // transform y
  
  var default_affine_array = new Array(6);
  default_affine_array[0] = 1.0; // scale x
  default_affine_array[1] = 0.0; // //unused
  default_affine_array[2] = 0.0; // //unused
  default_affine_array[3] = 1.0; // scale y
  default_affine_array[4] = 0.0; // transform x
  default_affine_array[5] = 0.0; // transform y
  
  // Used to provide an initial transform for the layer.
  this.set_default = function(t_string){
    
    //kvetch.say('affine.set_default(1): ' + t_string)
    //kvetch.say('affine.set_default(2): ' + new_affine_array + ', ' +
    //	       new_affine_array.length);

    var vector_string = t_string.substring(7, t_string.length -1);
    var new_affine_array = new Array();
    // NOTE/WARNING: There are apparently massive incompatibilies with
    // how different JSs do regexp and split. There will be no safety
    // there, so find workarounds.
    //new_affine_array = vector_string.split(/\,\s*/);
    //new_affine_array = vector_string.split(' ');
    //new_affine_array = vector_string.split(',');
    new_affine_array = vector_string.split(', ');
    
    default_affine_array[0] = new_affine_array[0];
    default_affine_array[1] = new_affine_array[1];
    default_affine_array[2] = new_affine_array[2];
    default_affine_array[3] = new_affine_array[3];
    default_affine_array[4] = new_affine_array[4];
    default_affine_array[5] = new_affine_array[5];
    
    //
    this.reset_to_default();
  };


  // Move will be dealing entirely in integers, so we can protect
  // accordingly.
  this.move = function(args){

    var moveX = parseInt(args.x);
    var moveY = parseInt(args.y);
    
    var pX = parseInt(affine_array[4]);
    var pY = parseInt(affine_array[5]);

    affine_array[4] = pX + moveX;
    affine_array[5] = pY + moveY;
  };


  //
  this.zoom = function(args){

    var direction = args.direction;
    var factor = args.factor;

    var tx = affine_array[4];
    var ty = affine_array[5];
    var sx = affine_array[0];
    var sy = affine_array[3];

    // Use Mootools' additional properties?
    // var svg_width = ...;
    // var svg_height = ...;
    // BUG: Huh? Why do I need to skew these numbers?
    var w_width = window.outerWidth / 2.75;
    var w_height = window.outerHeight / 3.25;
    //var w_width = window.outerWidth / 2.0;
    //var w_height = window.outerHeight / 2.5;

    //alert("w_width = " + w_width + "; w_height = " + w_height);

    if( direction == 'in' ){
      affine_array[0] = sx * factor;
      affine_array[3] = sy * factor;
      affine_array[4] = ((tx - w_width) * factor) + w_width;
      affine_array[5] = ((ty - w_height)* factor) + w_height;
    }else{
      affine_array[0] = sx / factor;
      affine_array[3] = sy / factor;
      affine_array[4] = ((tx - w_width) / factor) + w_width;
      affine_array[5] = ((ty - w_height) / factor) + w_height;
    }
  };


  //
  this.toAlert = function(){
    var output = new Array;
    output.push('matrix(');
    //output.push(affine_array.join(", "));
    output.push(affine_array.join(" "));
    output.push(')');
    alert( output.join('') );
  };


  //
  this.toString = function(){
    var output = new Array;
    output.push('matrix(');
    //output.push(affine_array.join(", "));
    output.push(affine_array.join(", "));
    output.push(')');
    return output.join('');
  };


  //
  this.reset_to_default = function(){
    affine_array[0] = default_affine_array[0];
    affine_array[1] = default_affine_array[1];
    affine_array[2] = default_affine_array[2];
    affine_array[3] = default_affine_array[3];
    affine_array[4] = default_affine_array[4];
    affine_array[5] = default_affine_array[5];
  };
};
