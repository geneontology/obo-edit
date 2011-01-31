//////////
//
// RG Browser
//
//////////


var kvetch = new org.bbop.kvetch();
//kvetch.say('RefGenome.js');
//var go = new org.bbop.go();


////////////////////////////
//
// Initialization.
//
////////////////////////////


//
function RefGenomeInit(//svg_id,
		       //matrix_id,
		       underlay_context_1_id,
		       underlay_context_2_id,
		       underlay_context_3_id,
		       overlay_context_id,
		       control_context_id){//,
  //details_context_id){  

  // TODO: Make sure that this is not going to run on IE. First things
  // first: a cascade to figure out exactly what this environment is
  // capable of...and give an error message if it looks bad.
  //var can_do_svg = true;
  //if( ! can_do_svg ){
  // alert("SVG failure--abort!");
  //}
  //var proceed_p = false;
  //var test_svg = document.getElementById(svg_id);
  //if( test_svg ){
  //  var test_svg_string = test_svg.getAttribute("width");
  //  if( test_svg_string ){
  //   proceed_p = true;
  //  }
  //}

  //if( proceed_p ){
  
  // TODO: External variables like this should come through something
  // like: AmiGO.External.varname. Gotta keep track of these better.
  //var node_detailer = new NodeDetails(details_context_id);

  //
  //var viewer = new Viewer(svg_id, matrix_id);

  // NOTE: 'homolset_symbol' in an incoming global coming from the perl side.
  var control_panel = new ControlPanel(control_context_id,
				       homolset_symbol,
				       homolset_link);
  
  //
  var hiliter = new HiLiter(underlay_context_3_id,
			    underlay_context_2_id,
			    underlay_context_1_id,
			    control_panel.addition_id());
  var spec_hash = amigo_species;

  // Create an alphabetically ordered array, then iterate over that.
  //  var order_array = amigo_species_order;
//   var order_array = new Array();
//   for( var spec_id in spec_hash ){
//     order_array.push(spec_id);
//   }
//   order_array.sort(
// 		   function(a, b){
// 		     if( spec_hash[a].name < spec_hash[b].name ){
// 		       return -1;
// 		     }else if (spec_hash[a].name > spec_hash[b].name){
// 		       return 1;
// 		     }
// 		     return 0;
// 		   });
  for( var i = 0; i < amigo_species_order.length; i++ ){
    var prop = amigo_species_order[i];
    hiliter.add(spec_hash[prop].name,
		// Pascale prefers w/o the counts...
		//spec_hash[prop].name,
		spec_hash[prop].name +
		 ' (' + spec_hash[prop].direct_count +
		 '/' + spec_hash[prop].indirect_count + ')',
		spec_hash[prop].color,
		spec_hash[prop].direct_terms,
		spec_hash[prop].indirect_terms);
  }
}


ControlPanel = function(control_context_id, title, link){

  var control_context = document.getElementById(control_context_id);

  var svg_lib = new org.bbop.SVG();

  //var c_width = 160;
  var c_width = 250;
  //var c_height = 460;
  var c_height = 545;
  var x_inset = 5;
  var y_inset = 5;

  var text_spacing = 12;

  // Create a background bounding box for the control panels.
  control_context.appendChild(svg_lib.makeRect(x_inset, y_inset,
					       c_width, c_height,
					       'black', 'white'));

  // Add CP marker.
  control_context.appendChild(svg_lib.makeText(77, 20, 'Control Panel',
					       '12px', 'normal'));

  // Add a title to the page.
  control_context.appendChild(svg_lib.makeText(15, 40, title,
					       '15px', 'bold'));

  //
  control_context.appendChild(svg_lib.makeText(15, 150,
					       'Direct annotation controls:',
					       '10px', 'bold'));

  // Additional information blurb.
  {
    //var t = 378;
    var t = 490;
    control_context.appendChild(svg_lib.makeText(15, t + text_spacing * 0,
						 'Coverage display:',
						 '10px', 'bold'));
    control_context.appendChild(svg_lib.makeText(15, t + text_spacing * 1,
						 'Toggles the annotation coverage of the',
						 '10px', 'normal'));
    control_context.appendChild(svg_lib.makeText(15, t + text_spacing * 2,
						 'species selected above.',
						 '10px', 'normal'));
  }

  // Pie info key...
  {
    var text_spacing = 19;
    var t = 113;
    //var control_box = document.getElementById(component_id);
    control_context.appendChild(svg_lib.makeText(15, 
						 t + text_spacing * 0,
						 'Direct evidence pie key:',
						 '10px', 
						 'bold'));
    control_context.appendChild(svg_lib.makeMiniPieSymbol(20,
							  t+text_spacing *1,
							  'exp'));
    control_context.appendChild(svg_lib.makeText(35,
						 t+text_spacing *1,
						 'Experimental',
						 '10px',
						 'normal'));
    control_context.appendChild(svg_lib.makeMiniPieSymbol(120,
							  t+text_spacing *1,
							  'iss'));
    control_context.appendChild(svg_lib.makeText(135,
						 t+text_spacing *1,
						 'ISS',
						 '10px', 'normal'));
    control_context.appendChild(svg_lib.makeMiniPieSymbol(168,
							  t+text_spacing *1));
    control_context.appendChild(svg_lib.makeText(183,
						 t+text_spacing *1,
						 'Other',
						 '10px', 'normal'));
  }

  // Add an instruction blurb.
  {
    var text_spacing = 12;
    //var t = 492;
    var t = 60;
    control_context.appendChild(svg_lib.makeText(15, t + text_spacing * 0,
						 'Graph controls:',
						 '10px', 'bold'));
    control_context.appendChild(svg_lib.makeText(15, t + text_spacing * 1,
						 'Mouse "wheel" zooms. Pan by clicking',
						 '10px', 'normal'));
    control_context.appendChild(svg_lib.makeText(15, t + text_spacing * 2,
						 'and dragging the mouse. Control-a',
						 '10px', 'normal'));
    control_context.appendChild(svg_lib.makeText(15, t + text_spacing * 3,
						 'and control-z zoom on the keyboard.',
						 '10px', 'normal'));
  }

  // BUG: doesn't work!
  // Add a link to homolset_annotation to the page.
  //control_context.appendChild(svg_lib.makeText(140, 25, 'foo', '10px', 'bold'), link);
  //control_context.appendChild(svg_lib.makeLink(svg_lib.makeText(140, 25, '(return)', '10px', 'normal'), link));

  // Add a title to the page.
  var gNode =
  document.createElementNS('http://www.w3.org/2000/svg', "g");
  gNode.setAttributeNS(null, "id", 15);
  gNode.setAttributeNS(null, "y", 30);
  gNode.setAttributeNS(null, "font-size", "15px");
  gNode.setAttributeNS(null, "font-weight", "bold");
  control_context.appendChild(gNode);


  //
  this.addition_node = function(){
    return gNode;
  };


  //
  this.addition_id = function(){
    return gNode.id;
  };


  //
  this.add_component = function(incoming_g){
    control_context.appendChild(incoming_g);
  };

};


//HiLiter = function(control_context_id, graphic_context_id){
HiLiter = function(underlay_context_top_id,
		   underlay_context_middle_id,
		   underlay_context_bottom_id,
		   component_id){

  // Get control center.
  // BUG: document should not be used here.
  //var control_element = document.getElementById(control_context_id);
  // As ACCs should be unique, we won't worry about clobbering...
  var text_node_hash = {};
  var text_nodes = document.getElementsByTagName('text');
  for( var k = 0; k < text_nodes.length; k++ ){
    text_node_hash[text_nodes[k].textContent] = text_nodes[k];
  }

  var graphic_element_top =
     document.getElementById(underlay_context_top_id);
  var graphic_element_middle =
     document.getElementById(underlay_context_middle_id);
  var graphic_element_bottom =
     document.getElementById(underlay_context_bottom_id);

  var svg_lib = new org.bbop.SVG();

  // Our little checkboxes.
  var species_cb = {};
  var current_index = 0;

  // Radio buttons for direct and indirect.
  var labeltextStyles = {//"font-family":"Arial,Helvetica",
			 "fill":"black",
			 "font-size":15};
  var labelDistance = 12;
  var labelYOffset = 5.5;

  // Internal toggle variables that will dictate how underlays are
  // rendered.
  var showCoverageP = false;


  //
  function showCoverage(){

    // Then uncheck all of the checkboxes.
    //var bulk_species = {};
    for (var species_unit in species_cb){
      if( species_cb[species_unit].checkbox.checkedStatus ){
	//bulk_species[species_unit] = species_cb;
	hiliteCoverage(species_unit);
      }
    }
    //hiliteCoverage(bulk_species);
  }

  //
  function wipeCoverage(){

    // Then uncheck all of the checkboxes.
    for (var species_unit in species_cb){
      if( species_cb[species_unit].checkbox.checkedStatus ){
	unhiliteCoverage(species_unit);
      }
    }
  }


  // All these widgets as a logical unit. For checking and unchecking
  // all species.
  {
    var t = 165;

    // Toggle box for coverage.
    var cbToggleAll =
    new checkBox("toggleAll",
		 component_id,
		 20, t,
		 "checkBoxRect",
		 "checkBoxFill",
		 false,
		 "All",
		 labeltextStyles,
		 labelDistance,
		 labelYOffset,
		 undefined,
		 // ...
		 function(e){

		   if( this.checked ||
		       this.checkedStatus ){
		     // Check all unchecked boxes; should send events.
		     for (var species_unit in species_cb){
		       if( ! species_cb[species_unit].checkbox.checkedStatus ){
			 species_cb[species_unit].checkbox.check(true);
		       }
		     }
		   }else{

		     // Uncheck all checked boxes; should send events.
		     for (var species_unit in species_cb){
		       if( species_cb[species_unit].checkbox.checkedStatus ){
			 species_cb[species_unit].checkbox.uncheck(true);
		       }
		     }
		   }
		 });

    cbToggleAll.timerMs = 0; // rapid state changing
  }

  // All these widgets as a logical unit.
  {
    //var t = 420;
    var t = 490;
    var widget_spacing = 20;

    // Toggle box for coverage.
    var cbCoverage =
    new checkBox("coverage",
		 component_id,
		 20, t + widget_spacing * 2,
		 "checkBoxRect",
		 "checkBoxFill",
		 false,
		 "Coverage",
		 labeltextStyles,
		 labelDistance,
		 labelYOffset,
		 undefined,
		 // ...
		 function(e){

		   if( this.checked ||
		       this.checkedStatus ){
		     showCoverage();
		     showCoverageP = true;
		   }else{
		     wipeCoverage();
		     showCoverageP = false;
		   }
		 });

    cbCoverage.timerMs = 0; // rapid state changing

    //
    var control_box = document.getElementById(component_id);
    control_box.appendChild(svg_lib.makeMiniHighlightSymbol(120,t+widget_spacing *2));
  }


  //
  this.add = function(id, label, color, direct_terms, indirect_terms){

    var svgNS = 'http://www.w3.org/2000/svg';
    var xlinkNS = 'http://www.w3.org/1999/xlink';
    var x = 20;
    //var y = (current_index * 25) + 115;
    //var y = (current_index * 25) + 145;
    var y = (current_index * 25) + 190;

    // Add a colored BG.
    var control_element = document.getElementById(component_id);
    var outline = document.createElementNS(svgNS, 'rect');
    outline.setAttributeNS( null, 'x', x - 10 );
    outline.setAttributeNS( null, 'y', y - 10 );
    //outline.setAttributeNS( null, 'width',  100 + 50 );
    outline.setAttributeNS( null, 'width',  190 + 50 );
    outline.setAttributeNS( null, 'height', 15 + 5 );
    outline.setAttributeNS(null, 'fill', color);
    outline.setAttributeNS(null, 'fill-opacity', '.85');
    outline.setAttributeNS(null, 'stroke', 'black');
    outline.setAttributeNS(null, 'stroke-width', '1');
    outline.setAttributeNS( null, 'stroke', 'black' );
    control_element.appendChild(outline);
    
    // Add the checkbox.
    var gt = document.createElementNS(svgNS, 'g');
    var gm = document.createElementNS(svgNS, 'g');
    var gb = document.createElementNS(svgNS, 'g');
    graphic_element_top.appendChild(gt);
    graphic_element_middle.appendChild(gm);
    graphic_element_bottom.appendChild(gb);

    species_cb[id] = { checkbox: new checkBox(id,           // ID.
					   component_id, // Parent node.
					   x, y,
					   "checkBoxRect",  //SVG def for box.
					   "checkBoxCross", //SVG def for chk.
					   false,           // Checked.
					   label,           // Label?
					   0,  //labeltextStyles,
					   12, //labelDistance,
					   5,  //labelYOffset,
					   undefined,
					   function(e){
					     if( this.checked ||
						 this.checkedStatus ){
					       if( showCoverageP ){
						 hiliteCoverage(e);
					       }
					       hiliteDirect(e);
					     }else{
					       unhiliteDirect(e);	
					       unhiliteCoverage(e);
					     }
					   }),
		    id: id,
		    color: color,
		    direct_terms: direct_terms,
		    indirect_terms: indirect_terms,
		    element_top: gt,
		    element_middle: gm,
		    element_bottom: gb
    };

    species_cb[id].checkbox.timerMs = 0; // rapid state changing
    current_index++;
  };


  // TODO/BUG: this is the main bottleneck...fix!
  // NOTE: this is only being used for ACCs now... =)
  function getAccTextNode(acc){

    // Drill down and find everything that has this string in it.
    var result_node = undefined;
    if( text_node_hash[acc] ){
      result_node = text_node_hash[acc];
    }

    return result_node;
  }


  // This is (unfortunately) searching for text in text tags.
  // TODO: move to a real graph system.
  function hiliteCoverage(e){

    // Get our hash.
    var hash = species_cb[e];
    if( ! hash ){
      alert('Failed to find species on hilite:' + e);            
    }else{
      
      //
      for( var acc in hash["indirect_terms"] ){

	var result_node = getAccTextNode(acc);
	if ( result_node ){
	  var x = result_node.getAttribute('x') ;
	  var y = result_node.getAttribute('y') ;	
	  hash.element_bottom.appendChild(svg_lib.makeHighlightSymbol(x, y, hash.color));
	}
      }
    }
  }


  // Remove all of our coverage hilites.
  function unhiliteCoverage(e){

    var hash = species_cb[e];
    if( ! hash ){
      alert('Failed to find species on unhilite:' + e);            
    }else{
      // Remove the old highlighted children .
      var old_hilites_bottom = hash.element_bottom.childNodes;
      while( 0 < old_hilites_bottom.length ){
	hash.element_bottom.removeChild(old_hilites_bottom[0]);
      }
    }
  }


  // Add function.
  function hiliteDirect(e){

    // Get our hash.
    var hash = species_cb[e];
    if( ! hash ){
      alert('Failed to find species on hilite:' + e);            
    }else{

      // Drill down and find everything that has this string in t.
      //for (var prop in hash){
      for( var acc in hash["direct_terms"] ){

	var result_node = getAccTextNode(acc);
	
	// For all the nodes that have a string in them, draw a circle
	// behind them.
	// BUG: See note above.
	//for( var l = 0; l < result_nodes.length; l++ ){
	if ( result_node ){

	  // Get the location.
	  var x = result_node.getAttribute('x');
	  var y = result_node.getAttribute('y');

	  // Drill through the terms list looking for
	  // has exp nodes.
	  var show_exp_pie = false;
	  var show_iss_pie = false;
	  var show_pie = false;
	  for( var gp_id in amigo_terms[acc].gene_products ){

	    // Choose the pie type depending on the properties.
	    var cgp = amigo_terms[acc].gene_products[gp_id];
	    if( e == cgp.species ){
	      if( cgp['has_exp_p'] && cgp.direct_p &&
		  cgp['has_exp_p'] > 0 && cgp.direct_p > 0 ){
		show_exp_pie = true;
	      }else if( cgp['has_good_iss_p'] && cgp.direct_p &&
			cgp['has_good_iss_p'] > 0 && cgp.direct_p > 0 ){
		show_iss_pie = true;
	      }else{
		show_pie = true;
	      }
	    }
	  }

	  // Make sure that I just draw one--if not, there will be
	  // enough layers to drown-out ev circles.
	  if( show_exp_pie ){
	    hash.element_top.appendChild(svg_lib.makePieSymbolExp(x, y,
								  hash.color,
								  e));
	  }else if( show_iss_pie ){
	    hash.element_top.appendChild(svg_lib.makePieSymbolISS(x, y,
								  hash.color,
								  e));
	  }else{
	    hash.element_top.appendChild(svg_lib.makePieSymbol(x, y,
							       hash.color,
							       e));
	  }
	}
      }
    }
  }


  // Remove all of our direct hilites.
  function unhiliteDirect(e){

    var hash = species_cb[e];
    if( ! hash ){
      alert('Failed to find species on unhilite:' + e);            
    }else{
      // Remove the old highlighted children.
      var old_hilites_top = hash.element_top.childNodes;
      while( 0 < old_hilites_top.length ){
	hash.element_top.removeChild(old_hilites_top[0]);
      }
    }
  }

};
