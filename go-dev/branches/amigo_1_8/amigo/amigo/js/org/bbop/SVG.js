//////////
//
// org.bbop.SVG
//
// Purpose: Simplify generation of SVG constructs.
//
//////////


// Module and namespace checking.
if ( typeof org == "undefined" ){ org = {}; }
if ( typeof org.bbop == "undefined" ){ org.bbop = {}; }
if ( typeof org.bbop.SVG == "undefined" ){ org.bbop.SVG = {}; }
var kvetch = new org.bbop.kvetch();
//kvetch.say('org.bbop.SVG.js');


// Takes the div to tooltip-ify as an argument.
org.bbop.SVG = function(div_id){

  //
  //this.tt_div = document.getElementById(div_id);

  //
  var svgNS = 'http://www.w3.org/2000/svg';
  var xlinkNS = 'http://www.w3.org/1999/xlink';


  //
  this.makeMiniHighlightSymbol = function(x, y){

    var use_tag = document.createElementNS(svgNS, 'use');
    use_tag.setAttributeNS(null, 'x', x);
    use_tag.setAttributeNS(null, 'y', y);
    use_tag.setAttributeNS(xlinkNS, 'href', '#miniHighlightSymbol');
    var color = 'black';
    use_tag.setAttributeNS(null, 'fill', color);
    use_tag.setAttributeNS(null, 'stroke', color);
    use_tag.setAttributeNS(null, 'stroke-fill', color);
    use_tag.setAttributeNS(null, 'color', color);
    use_tag.setAttributeNS(null, 'cursor', 'pointer');

    return use_tag;
  };


  // 'suftag' can be 'iss' or 'exp' or not an arg.
  this.makeMiniPieSymbol = function(x, y, suftag){

    if( ! suftag ){
      suftag = '';
    }else{
      suftag = '_' + suftag;
    }
    var use_tag = document.createElementNS(svgNS, 'use');
    use_tag.setAttributeNS(null, 'x', x);
    use_tag.setAttributeNS(null, 'y', y);
    use_tag.setAttributeNS(null, 'fill', '#777777');
    use_tag.setAttributeNS(xlinkNS, 'href', '#mini_pie' + suftag);
    use_tag.setAttributeNS(null, 'cursor', 'pointer');

    return use_tag;
  };

  //
  this.makeHighlightSymbol = function(x, y, color){

    var use_tag = document.createElementNS(svgNS, 'use');
    use_tag.setAttributeNS(null, 'x', x);
    use_tag.setAttributeNS(null, 'y', y);
    use_tag.setAttributeNS(null, 'fill', color);
    // TODO/BUG: This should not be hard-wired--need a communication
    // channel with SVG.pm.
    use_tag.setAttributeNS(xlinkNS, 'href', '#highlightSymbol');
    use_tag.setAttributeNS(null, 'cursor', 'pointer');
    //use_tag.addEventListener("click",this,false);

    return use_tag;
  };


  //
  this.makePieSymbol = function(x, y, color, spc_name){

    var use_tag = document.createElementNS(svgNS, 'use');
    use_tag.setAttributeNS(null, 'x', x);
    use_tag.setAttributeNS(null, 'y', y);
    use_tag.setAttributeNS(null, 'fill', color);
    use_tag.setAttributeNS(xlinkNS, 'href', '#' + spc_name + '_pie');
    use_tag.setAttributeNS(null, 'cursor', 'pointer');

    return use_tag;
  };


  //
  this.makePieSymbolExp = function(x, y, color, spc_name){

    var use_tag = document.createElementNS(svgNS, 'use');
    use_tag.setAttributeNS(null, 'x', x);
    use_tag.setAttributeNS(null, 'y', y);
    use_tag.setAttributeNS(null, 'fill', color);
    use_tag.setAttributeNS(xlinkNS, 'href', '#' + spc_name + '_pie_exp');
    use_tag.setAttributeNS(null, 'cursor', 'pointer');

    return use_tag;
  };


  //
  this.makePieSymbolISS = function(x, y, color, spc_name){

    var use_tag = document.createElementNS(svgNS, 'use');
    use_tag.setAttributeNS(null, 'x', x);
    use_tag.setAttributeNS(null, 'y', y);
    use_tag.setAttributeNS(null, 'fill', color);
    use_tag.setAttributeNS(xlinkNS, 'href', '#' + spc_name + '_pie_iss');
    use_tag.setAttributeNS(null, 'cursor', 'pointer');

    return use_tag;
  };


  //
  this.makeRect = function(x, y, width, height, color, fill_color){

    var rect = document.createElementNS(svgNS, 'rect');
    rect.setAttributeNS(null, 'fill', fill_color);
    //rect.setAttributeNS(null, 'fill-opacity', '.85');
    rect.setAttributeNS(null, 'fill-opacity', '.67');
    rect.setAttributeNS(null, 'stroke', color);
    rect.setAttributeNS(null, 'stroke-width', '2');
    rect.setAttributeNS(null, 'x', x);
    rect.setAttributeNS(null, 'y', y);
    rect.setAttributeNS(null, 'width', width);
    rect.setAttributeNS(null, 'height', height);
    return rect;
  };


  //
  this.makeSolidRect = function(x, y, width, height, color, fill_color){

    var rect = document.createElementNS(svgNS, 'rect');
    rect.setAttributeNS(null, 'fill', fill_color);
    //rect.setAttributeNS(null, 'fill-opacity', '.');
    rect.setAttributeNS(null, 'stroke', color);
    rect.setAttributeNS(null, 'stroke-width', '1');
    rect.setAttributeNS(null, 'x', x);
    rect.setAttributeNS(null, 'y', y);
    rect.setAttributeNS(null, 'width', width);
    rect.setAttributeNS(null, 'height', height);
    return rect;
  };


  //
  function path_command(command, arg_x, arg_y){
    return  command + ' ' + arg_x + ' ' + arg_y + ' ';
  }
  this.makeCrosshair = function(x, y, width, color, alpha){

    var new_x = x - (width/2);
    var new_y = y - (width/2);

    var cross = document.createElementNS(svgNS, 'path');
    cross.setAttributeNS(null, 'd',
			 path_command('M', new_x, new_y) +
			 path_command('L', new_x + width, new_y + width) +
			 path_command('M', new_x + width, new_y) +
			 path_command('L', new_x, new_y + width)
			 );
    //cross.setAttributeNS(null, 'fill', fill_color);
    //cross.setAttributeNS(null, 'fill-opacity', '.85');
    cross.setAttributeNS(null, 'stroke', color);
    cross.setAttributeNS(null, 'stroke-opacity', alpha);
    cross.setAttributeNS(null, 'stroke-width', '2');
    return cross;
  };


  // BUG/TODO: make a better call--use hash for optional args.
  //var unique_id = 0;
  this.makeText = function(x, y, string, size, weight){

    if( typeof(size) == 'undefined'){ size = '15px'; }
    if( typeof(weight) == 'undefined'){ weight = 'normal'; }

    // Create envelope.
    var text = document.createElementNS(svgNS, 'text');
    text.setAttributeNS(null, "x", x);
    text.setAttributeNS(null, "y", y);
    text.setAttributeNS(null, "font-weight", weight);
    text.setAttributeNS(null, "font-size", size);
    //text.setAttributeNS(null, "id", "unique_text_id_" + unique_id);
    //unique_id = unique_id + 1;

    // Actual text.
    var textNode = document.createTextNode(string);
    text.appendChild(textNode);

    return text;
  };

  // Wrap an object in a link...
  this.makeLink = function(thing, url){

//     // Create anchor.
//     var anchor = document.createElementNS(xlinkNS, "a");

//     anchor.setAttributeNS("http://www.w3.org/2000/xlink",
// 			  "xlink:href",
// 			  url);
//     anchor.appendChild(thing);
//     //thing.appendChild(anchor);
    
    //var text_elem = document.getElementById("unique_text_id_8");
    var text_elem = thing;
    var a_elem = document.createElement("a");
    //var text_node = document.createTextNode("link");
    a_elem.setAttributeNS("http://www.w3.org/1999/xlink",
			  //"http://www.w3.org/1999/xlink/namespace",
			  //"http://www.w3.org/2000/xlink/namespace",
			  //xlinkNS,
			  "xlink:href",
			  url);
    //a_elem.appendChild(text_node);
    //text_elem.appendChild(a_elem);
    a_elem.appendChild(text_elem);
    return a_elem;
    
    // return anchor;
  };

  // BUG/TODO: make a better call--use hash for optional args.
  this.makeItemText = function(x, y, major_string, minor_string){

    //if( typeof(size) == 'undefined'){ size = '15px'; }
    //if( typeof(weight) == 'undefined'){ weight = 'normal'; }

    // Create envelopes.
    var major_text = document.createElementNS(svgNS, 'text');
    major_text.setAttributeNS(null, "x", x);
    major_text.setAttributeNS(null, "y", y);
    major_text.setAttributeNS(null, "font-size", '15px');
    major_text.setAttributeNS(null, "font-weight", 'normal');
    var minor_text = document.createElementNS(svgNS, 'text');
    minor_text.setAttributeNS(null, "x", x + 10);
    minor_text.setAttributeNS(null, "y", y);
    minor_text.setAttributeNS(null, "font-size", '10px');
    minor_text.setAttributeNS(null, "font-weight", 'normal');

    // Actual text.
    var majorTextNode = document.createTextNode(major_string);
    major_text.appendChild(majorTextNode);
    var minorTextNode = document.createTextNode(minor_string);
    minor_text.appendChild(minorTextNode);

    // Make an opaque bounding box for the text...
    var length = major_string.length;

    var rect = document.createElementNS(svgNS, 'rect');
    rect.setAttributeNS(null, 'fill', 'white');
    rect.setAttributeNS(null, 'fill-opacity', '.5');
    rect.setAttributeNS(null, 'stroke', 'blue');
    rect.setAttributeNS(null, 'stroke-width', '1');
    rect.setAttributeNS(null, 'x', x);
    rect.setAttributeNS(null, 'y', y);
    rect.setAttributeNS(null, 'width', (10 * length) + 'px');
    rect.setAttributeNS(null, 'height', 20 + 'px');
    rect.appendChild(majorTextNode);
    rect.appendChild(minor_text);

    //return text;
    return rect;
  };

};


//   //
//   this.makeStarSymbol = function(x, y){

//     var use_tag = document.createElementNS(svgNS, 'use');
//     use_tag.setAttributeNS(null, 'x', x);
//     use_tag.setAttributeNS(null, 'y', y);
//     // TODO/BUG: This should not be hard-wired--need a communication
//     // channel with SVG.pm.
//     use_tag.setAttributeNS(xlinkNS, 'href', '#starSymbol');
//     use_tag.setAttributeNS(null, 'cursor', 'pointer');
//     //use_tag.addEventListener("click",this,false);

//     return use_tag;
//   };


//   //
//   this.makeStarShiftedSymbol = function(x, y){

//     var use_tag = document.createElementNS(svgNS, 'use');
//     use_tag.setAttributeNS(null, 'x', x);
//     use_tag.setAttributeNS(null, 'y', y);
//     // TODO/BUG: This should not be hard-wired--need a communication
//     // channel with SVG.pm.
//     use_tag.setAttributeNS(xlinkNS, 'href', '#starShiftedSymbol');
//     use_tag.setAttributeNS(null, 'cursor', 'pointer');
//     //use_tag.addEventListener("click",this,false);

//     return use_tag;
//   };


//   //
//   this.makeTargetSymbolGreen = function(x, y){

//     var use_tag = document.createElementNS(svgNS, 'use');
//     use_tag.setAttributeNS(null, 'x', x);
//     use_tag.setAttributeNS(null, 'y', y);
//     // TODO/BUG: This should not be hard-wired--need a communication
//     // channel with SVG.pm.
//     use_tag.setAttributeNS(xlinkNS, 'href', '#targetSymbolGreen');
//     use_tag.setAttributeNS(null, 'cursor', 'pointer');
//     //use_tag.addEventListener("click",this,false);

//     return use_tag;
//   };


//   //
//   this.makeTargetSymbolBlue = function(x, y){

//     var use_tag = document.createElementNS(svgNS, 'use');
//     use_tag.setAttributeNS(null, 'x', x);
//     use_tag.setAttributeNS(null, 'y', y);
//     // TODO/BUG: This should not be hard-wired--need a communication
//     // channel with SVG.pm.
//     use_tag.setAttributeNS(xlinkNS, 'href', '#targetSymbolBlue');
//     use_tag.setAttributeNS(null, 'cursor', 'pointer');
//     //use_tag.addEventListener("click",this,false);

//     return use_tag;
//   };


//   //
//   this.makeTargetSymbolYellow = function(x, y){

//     var use_tag = document.createElementNS(svgNS, 'use');
//     use_tag.setAttributeNS(null, 'x', x);
//     use_tag.setAttributeNS(null, 'y', y);
//     // TODO/BUG: This should not be hard-wired--need a communication
//     // channel with SVG.pm.
//     use_tag.setAttributeNS(xlinkNS, 'href', '#targetSymbolYellow');
//     use_tag.setAttributeNS(null, 'cursor', 'pointer');
//     //use_tag.addEventListener("click",this,false);

//     return use_tag;
//   };
//   //
//   this.makeWedgeSymbol = function(x, y, color, species){

//     var use_tag = document.createElementNS(svgNS, 'use');
//     use_tag.setAttributeNS(xlinkNS, 'href', '#' + species + '_wedge');
//     use_tag.setAttributeNS(null, 'x', x);
//     use_tag.setAttributeNS(null, 'y', y);
//     use_tag.setAttributeNS(null, 'fill', color);
//     use_tag.setAttributeNS(null, 'stroke', color);
//     use_tag.setAttributeNS(null, 'stroke-fill', color);
//     use_tag.setAttributeNS(null, 'color', color);
//     use_tag.setAttributeNS(null, 'cursor', 'pointer');

//     return use_tag;
//   };


//   //
//   this.makeTargetSymbol = function(x, y, color, species){

//     var use_tag = document.createElementNS(svgNS, 'use');
//     use_tag.setAttributeNS(xlinkNS, 'href', '#' + species + '_target');
//     use_tag.setAttributeNS(null, 'x', x);
//     use_tag.setAttributeNS(null, 'y', y);
//     use_tag.setAttributeNS(null, 'fill', color);
//     use_tag.setAttributeNS(null, 'stroke', color);
//     use_tag.setAttributeNS(null, 'stroke-fill', color);
//     use_tag.setAttributeNS(null, 'color', color);
//     use_tag.setAttributeNS(null, 'cursor', 'pointer');

//     return use_tag;
//   };


//   //
//   this.makeMiniCircles = function(x, y){

//     var use_tag = document.createElementNS(svgNS, 'use');
//     use_tag.setAttributeNS(null, 'x', x);
//     use_tag.setAttributeNS(null, 'y', y);
//     use_tag.setAttributeNS(xlinkNS, 'href', '#miniCircles');
//     var color = 'black';
//     use_tag.setAttributeNS(null, 'fill', color);
//     use_tag.setAttributeNS(null, 'stroke', color);
//     use_tag.setAttributeNS(null, 'stroke-fill', color);
//     use_tag.setAttributeNS(null, 'color', color);
//     use_tag.setAttributeNS(null, 'cursor', 'pointer');

//     return use_tag;
//   };

//   //
//   this.makeMiniEllipses = function(x, y){

//     var use_tag = document.createElementNS(svgNS, 'use');
//     use_tag.setAttributeNS(null, 'x', x);
//     use_tag.setAttributeNS(null, 'y', y);
//     use_tag.setAttributeNS(xlinkNS, 'href', '#miniEllipses');
//     var color = 'black';
//     use_tag.setAttributeNS(null, 'fill', color);
//     use_tag.setAttributeNS(null, 'stroke', color);
//     use_tag.setAttributeNS(null, 'stroke-fill', color);
//     use_tag.setAttributeNS(null, 'color', color);
//     use_tag.setAttributeNS(null, 'cursor', 'pointer');

//     return use_tag;
//   };


