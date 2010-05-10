////////////
////
//// org.bbop.NodeDetails
////
//// Purpose: Provide an automatic way of having tooltips pop-up when over
////          terms in the ontology.
////
//// Description of (necessary) amigo_terms".
////
//// {"<GO ACC>": {
////    "name": "<GO NAME>",
////    "count": <INTEGER>,              // Optional. 
////    "children": {                    // Optional.
////       "<GO ACC>": {
////          name: <STRING>,
////          open_p:[1|0],              // Is the child open? Optional.
////       },
////    },                     
////    "gene_products": {               // Optional.
////       "<UNIQUE GP ID>": {
////          "symbol": "<STRING>",
////          "species": "<HUMAN READABLE STRING>",
////          "color": "<HEX STRING>",
////          "has_exp_p":      [1|0],       // Optional.
////          "has_good_iss_p": [1|0],       // Optional.
////          "has_odd_iss_p":  [1|0],       // Optional.
////          "has_bad_iss_p":  [1|0],       // Optional.
////       },
////    },
//// ...
//// }
////
//////////


// Module and namespace checking.
if ( typeof org == "undefined" ){ org = {}; }
if ( typeof org.bbop == "undefined" ){ org.bbop = {}; }
if ( typeof org.bbop.NodeDetails == "undefined" ){ org.bbop.NodeDetails = {}; }


//
org.bbop.NodeDetails = function(details_context_id){

  var offset_from_mouse = 5;
  var in_x_offset = offset_from_mouse + 15;
  var in_y_offset = offset_from_mouse + 20;
  //var step_offset = 22;
  var step_offset = 16;

  var details_context = document.getElementById(details_context_id);

  var svg_lib = new org.bbop.SVG();

  var g_nodes = document.getElementsByTagName('g');
  for( var i = 0; i < g_nodes.length; i++ ){
    
    var g = g_nodes[i];

    // Use node this to hunt down top level nodes in the graph.
    // TODO: This is very nasty--we're actually looking at all of the
    // nodes and then just examining the ones that are g. Can't think
    // of another way though...
    var g_class = g.getAttribute('class') ;
    if( g_class == 'node' ){
      $(g).addEvent('mouseenter',
		    function(e) {

		      e = new Event(e).stop();  

		      // Make sure that we have the top "g"
		      // node. We'll go at most two steps up and then
		      // quit.
		      var g_node = e.target;
		      if( g_node.tagName != 'g' ){
			g_node = g_node.parentNode;
		      }
		      if( g_node.tagName != 'g' ){
			g_node = g_node.parentNode;
		      }

		      // Walk throught the "g" node's children until
		      // we find a text node.
		      var kids = g_node.childNodes;
		      var text_node = null;
		      for( var i = 0; i < kids.length; i++){
			if( kids[i].tagName == 'text' ){
			  text_node = kids[i];
			  i = kids.length;
			}
		      }
		      // If we got it.
		      if( text_node ){

			var x = e.client.x;
			var y = e.client.y;

			// Get the text contents.
			var go_id = text_node.textContent;
			var go_name = amigo_terms[go_id]["name"];

			// Dynamic box sizing...
			var calc_width = 235;
			// Unneeded now that we're "text wrapping".
			//var go_name_width = 10 * go_name.length;
			//if( calc_width < go_name_width ){
			//calc_width = go_name_width;
			//}

			// We will wait for the height attribute.
 			//rect.setAttributeNS(null, 'height', '425');
			var rect = svg_lib.makeRect(x + offset_from_mouse,
						    y + offset_from_mouse,
						    calc_width, 0,
						    'black', 'white');
 			details_context.appendChild(rect);

			var nx = x + in_x_offset;
			var ny = y + in_y_offset;

			// Add a GO id.
 			details_context.appendChild(svg_lib.makeText(nx, ny, go_id, '15px', 'bold'));

 			// Add a GO name.
 			//details_context.appendChild(svg_lib.makeText(nx, ny + step_offset, go_name, '15px', 'bold'));
			var wrapped_name_segments = _name_wrap(go_name);
 			//details_context.appendChild(svg_lib.makeText(nx, ny + step_offset, go_name, '15px', 'bold'));
			var segments = 1;
			for( {};
			     segments <= wrapped_name_segments.length;
			     segments++ ){
			  details_context.appendChild(svg_lib.makeText(nx, ny + (step_offset * (segments)), wrapped_name_segments[segments -1], '15px', 'bold'));
			}

			///
			/// Add additional/optional term information.
			///

			// used to set/space things as we go
			var step = 2 + (segments -2); 
			
			// Check for additional term info for children.
			if( amigo_terms[go_id]["children"] ){

			  var children = amigo_terms[go_id]["children"];
			  var total_count = 0;
			  var open_count = 0;
			  for( var gid in children ){

			    // 
			    total_count++;
			    if( children[gid]['open_p'] == 1 ){
				open_count++;
			    }
			  }

			  // Figure which and where to display.
			  var x = nx;
			  var y = ny + (step_offset * step) + 5;
			  if( total_count == 0 ){
			    details_context.appendChild(svg_lib.makeText(x, y, 'leaf node', '12px'));
			  }else if( total_count == 1 ){
			    details_context.appendChild(svg_lib.makeText(x, y, total_count + ' child (' + open_count + ' open)', '12px'));
			  }else{
			    details_context.appendChild(svg_lib.makeText(x, y, total_count + ' children (' + open_count + ' open)', '12px'));
			  }
			  step++;
			}
			
			// Check for additional term info (count).
			if( typeof amigo_terms[go_id]["count"] != "undefined"){

			  var count = amigo_terms[go_id]["count"];

			  var x = nx;
			  var y = ny + (step_offset * step) + 5;
			  if( count == 1 ){
			    details_context.appendChild(svg_lib.makeText(x, y, '1 association', '12px'));
			  }else{
			    details_context.appendChild(svg_lib.makeText(x, y, count + ' associations', '12px'));
			  }
			  step++;
			}

			///
			/// Check for additional GP info and then do a
			/// hash lookup and display the results in the
			/// "details" area.
			///
			//if( typeof amigo_terms[go_id]['gene_products'] != "undefined" ){
			if( amigo_terms[go_id]["gene_products"] ){

			  //step = 2; // Layout tweak for adding GPs
			  var gps = amigo_terms[go_id]["gene_products"];

			  ///
			  /// Create an alphabetically ordered array,
			  /// then iterate over that.
			  ///

			  //
			  var order_array = new Array();
			  for( var gp_id in gps ){
			    order_array.push(gp_id);
			  }

			  //
			  if( amigo_species_order ){

			    // Give values to species for GP sorting.
			    var ext_hash = {};
			    for( var gp_id in gps ){
			      ext_hash[gps[gp_id].species] = 1;
			    }
			    var amigo_species_order_hash = {};
			    for( var spi = 0;
				 spi < amigo_species_order.length;
				 spi++ ){			      
			      if( ext_hash[amigo_species_order[spi]] ){
				amigo_species_order_hash[amigo_species_order[spi]] = spi;
			      }
			    }
			    
			    // Order according to life order form RG.
			    order_array.sort(
					     function(a, b){
					       var spec_a = amigo_species_order_hash[gps[a].species];
					       var spec_b = amigo_species_order_hash[gps[b].species];
					       if( spec_a <
						   spec_b ){
						 return -1;
					       }else if ( spec_a >
							  spec_b ){
						 return 1;
					       }
					       return 0;
					     });

			  }else{

			    // Alphabetical ordering.
			    order_array.sort(
					     function(a, b){
					       if( gps[a].species <
						   gps[b].species ){
						 return -1;
					       }else if (gps[a].species >
							 gps[b].species){
						 return 1;
					       }
					       return 0;
					     });
			  }

			  //
			  for( var i = 0; i < order_array.length; i++ ){

			    var gp_id = order_array[i];
			    var gp = gps[gp_id];
			    var spec = gp.species;
			    var sym = gp.symbol;
			    var color = gp.color;
			    var direct_p = gp.direct_p;
			    var has_exp_p = gp.has_exp_p;
			    var has_good_iss_p = gp.has_good_iss_p;
			    // //var iss_only_p = gp.iss_only_p;
			    // //var exp_only_p = gp.exp_only_p;
			    //var has_odd_iss_p = gp.has_odd_iss_p;
			    //var has_bad_iss_p = gp.has_bad_iss_p;
			    
			    // Conditional text on boolean values.
			    var conditional_text = '';
			    var mini_buf = [];
			    if( has_exp_p && has_exp_p != 0 ){
			      mini_buf.push('E');}
			    if( has_good_iss_p && has_good_iss_p != 0 ){
			      mini_buf.push('I');}
			    //if( has_good_iss_p && has_good_iss_p != 0 ){
			    //  mini_buf.push('i');}
			    var mini_text = mini_buf.join('');
			    if( mini_text.length > 0 ){
			      mini_text = ' [' + mini_text + ']';
			      conditional_text = mini_text;
			    }
			    //if( has_good_iss_p == 1){
			    //  conditional_text += ' i'; }

			    var x = nx;
			    var y = ny + (step_offset * step) + 5;
			    
			    // Add a colored BG.
			    var outline =
			      document.createElementNS(svgNS, 'rect');
			    outline.setAttributeNS( null, 'x', x - 5);
			    outline.setAttributeNS( null, 'y', y - 12);
			    //outline.setAttributeNS( null, 'width',  180 );
			    outline.setAttributeNS( null, 'width',  215 );
			    outline.setAttributeNS( null, 'height', 16 );
			    outline.setAttributeNS(null, 'fill', color);
			    outline.setAttributeNS(null, 'fill-opacity', '.85');
			    outline.setAttributeNS(null, 'stroke', 'black');
			    outline.setAttributeNS(null, 'stroke-width', '.5');
			    outline.setAttributeNS( null, 'stroke', 'black' );
			    details_context.appendChild(outline);
			    
			    //
			    //details_context.appendChild(svg_lib.makeText(x, y, sym + ' (' + spec + ')' + conditional_text, '12px'));
			    if( direct_p && direct_p > 0 ){
				details_context.appendChild(svg_lib.makeText(x, y, sym + ' (' + spec + ')' + conditional_text, '12px', 'bold'));
			    }else{
				details_context.appendChild(svg_lib.makeText(x, y, sym + ' (' + spec + ')' + conditional_text, '12px'));
			    }

			    step++;
			  }
			}

			//
			rect.setAttributeNS(null, 'height',
					    in_y_offset + (step_offset * step));
		      }
		    });
      $(g).addEvent('mouseleave',
		    function(e) {

		      e = new Event(e).stop();  

		      // Remove everything in the details area.  There
		      // should only be one anyways, so this is a bit
		      // verbose.
		      var old_details = details_context.childNodes;
		      while( 0 < old_details.length ){
			details_context.removeChild(old_details[0]);
		      }		      
		    });
    }
  }


    // See AmiGO::GraphViz::_name_wrap for discussion of algorithm.
    function _accumulated_length(foo){ return foo.join(' ').length; }
    function _name_wrap(in_name, in_wnum){

	//dump("IN!\n");

	var name = '';
	if( in_name ){
	    name = in_name;
	}
	var wnum = 20;
	if( in_wnum ){
	    wnum = in_wnum;
	}

	//
	var wsre = new RegExp("\\s+"); 
	var phrases = name.split(wsre);

	final_phrases = new Array;
	while( phrases.length > 0 ){

	    tmp_phrase_reg = new Array;
	    while( phrases.length > 0 ){

		var phrase = phrases.shift();
		var total_length =
		    phrase.length + _accumulated_length(tmp_phrase_reg);

		if( total_length > wnum && tmp_phrase_reg.length > 0 ){
		    phrases.unshift(phrase);
		    break;
		}else if( total_length > wnum ){

		    var first_part = phrase.substr(0, wnum);
		    tmp_phrase_reg.push(first_part);

		    var second_part = phrase.substr(wnum + 1);
		    phrases.unshift(second_part);
		}else{
		    tmp_phrase_reg.push(phrase);
		}

	    }

	    final_phrases.push(tmp_phrase_reg.join(' '));
	    tmp_phrase_reg = new Array;
	}

	return final_phrases;
    }
};
