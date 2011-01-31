//////////
////
//// General Browser
////
//// TODO/BUG: to make the dynamic box bounding work well, there needs
//// to be a mono-spaced font in use more widley. Or clipping/wrapping
//// magic...
////
//////////


var kvetch = new org.bbop.kvetch();

// Carto's combobox wants this.
var myMapApp = new mapApp();


// Styles for the buttons.
//var styleButtonText = {"font-family":"monospace",
var styleButtonText = {"font-family":"Arial,Helvetica",
		       "fill":"black",
		       "font-size":15};
//var styleButton = {"fill":"lightsteelblue"};
var styleButton = {"fill":"lightgray"};
var styleButtonShadeLight = {"fill":"white"};
var styleButtonShadeDark = {"fill":"black"};

// Styles for the combo box.
var comboBoxCellHeight = 20;
var comboBoxTextpadding = 5;
//var comboBoxtextStyles = {"font-family":"Arial,Helvetica",
var comboBoxtextStyles = {"font-family":"monospace",
			  "font-size":15,
			  "fill":"black"};
var comboBoxStyles = {"stroke":"black",
		      "stroke-width":1,
		      "fill":"white"};
var comboBoxScrollbarStyles = {"stroke":"black",
			       "stroke-width":1,
			       "fill":"whitesmoke"};
var comboBoxSmallrectStyles = {"stroke":"black",
			       "stroke-width":1,
			       "fill":"lightgray"};
var comboBoxHighlightStyles = {"fill":"dimgray",
			       "fill-opacity":0.25};
var comboBoxTriangleStyles = {"fill":"dimgray"};


//
function BrowserInit(control_context_id, host_string){

  var svg_lib = new org.bbop.SVG();

  // Capture the control context.
  var control_context = document.getElementById(control_context_id);

  return function (e){

    var go_id = 'null';
    var go_name = 'null';

    ///
    /// Find out GO id and, from there, number of children.
    ///

    var go_node = e.target;
    if( go_node.tagName != 'g' ){
      go_node = go_node.parentNode;
    }
    if( go_node.tagName != 'g' ){
      go_node = go_node.parentNode;
    }

    // Walk throught the "g" node's children until
    // we find a text node.
    var kids = go_node.childNodes;
    var text_node = null;
    for( var i = 0; i < kids.length; i++){
      if( kids[i].tagName == 'text' ){
	text_node = kids[i];
	i = kids.length;
      }
    }
    // If we got it.
    if( text_node ){

      // Get the text contents and use thim to index AmiGO terms.
      go_id = text_node.textContent;
      go_name = amigo_terms[go_id]["name"];
    }

    ///
    /// Clean everything from context.
    ///

    var old_kids = control_context.childNodes;
    while( 0 < old_kids.length ){
      control_context.removeChild(old_kids[0]);
    }
    
    ///
    /// Assemble table.
    ///

    var step_space = 30;
    var c_width = 190;
    var c_height = 355;
    var x_inset = 5;
    var y_inset = 5;

    // Create a background bounding box for the control panels.
    var number_of_children = 0;
    for (var noise_kid_acc in amigo_terms[go_id]["children"]){
      number_of_children++;
    }
    if( number_of_children == 0 ){
      c_height = c_height - 210;
    }
    control_context.appendChild(svg_lib.makeRect(x_inset, y_inset,
						 c_width, c_height,
						 'grey', 'white'));


    // Add a title to the control.
    control_context.appendChild(svg_lib.makeText(18, 33, go_id,
						 '15px', 'bold'));
    
    // Add a name to the control.
    control_context.appendChild(svg_lib.makeText(18, 57, go_name,
						 '15px', 'bold'));

    // Make a reset button to add to the mix.
    ResetHandler = function(){
      this.buttonPressed = function(grpid, event, text){
	window.location = host_string + '?action=reset';
      };
    };
    var reset_handler = new ResetHandler();
    var resetButton = new button("ResetButton", control_context_id,
				 reset_handler,
				 "rect", "R", undefined,
				 15, 70, 20, 20,
				 styleButtonText, styleButton,
				 styleButtonShadeLight,
				 styleButtonShadeDark,
				 1);
    resetButton.timerMs = 0;
    control_context.appendChild(svg_lib.makeText(42, 85,
						 'reset',
						 '15px', ''));

    // Make a clean/seeding button to add to the mix.
    CleanHandler = function(){
      this.buttonPressed = function(grpid, event, text){
	window.location = host_string + '?action=seed&acc=' + go_id;
      };
    };
    var clean_handler = new CleanHandler();
    var cleanButton = new button("CleanButton", control_context_id,
				 clean_handler,
				 "rect", "C", undefined,
				 95, 70, 20, 20,
				 styleButtonText, styleButton,
				 styleButtonShadeLight,
				 styleButtonShadeDark,
				 1);
    cleanButton.timerMs = 0;
    control_context.appendChild(svg_lib.makeText(120, 85,
						 'clean',
						 '15px', ''));

    // Make a collapse button to add to the mix.
    CollapseHandler = function(){
      this.buttonPressed = function(grpid, event, text){
	window.location = host_string + '?action=close&acc=' + go_id;
      };
    };
    var collapse_handler = new CollapseHandler();
    var collapseButton = new button("CollapseButton", control_context_id,
				    collapse_handler,
				    "rect", "-", undefined,
				    15, 112, 20, 20,
				    styleButtonText, styleButton,
				    styleButtonShadeLight,
				    styleButtonShadeDark,
				    1);
    collapseButton.timerMs = 0;
    control_context.appendChild(svg_lib.makeText(42, 127,
						 'close',
						 '15px', ''));

    // Add a close pane button to the control.
    ClosePaneHandler = function(){
      this.buttonPressed = function(grpid, event, text){
	var old_kids = control_context.childNodes;

	// When we close the control, we nuke all children and then
	// add the reset button.
	while( 0 < old_kids.length ){
	  control_context.removeChild(old_kids[0]);
	}
      };
    };
    var close_pane_handler = new ClosePaneHandler();
    var closePaneButton = new button("closePaneButton", control_context_id,
				     close_pane_handler,
				     "rect", "X", undefined,
				     170, 13, 15, 15,
				     styleButtonText, styleButton,
				     styleButtonShadeLight,styleButtonShadeDark,
				     1);
    closePaneButton.timerMs = 0;

    // Add an image button to the control.
    ImageHandler = function(){
      this.buttonPressed = function(grpid, event, text){
	window.location = host_string + '?output=png';	
      };
    };
    var image_handler = new ImageHandler();
    var imageButton = new button("ImageButton", control_context_id,
				 image_handler,
				 "rect", "I", undefined,
				 130, 13, 15, 15,
				 styleButtonText, styleButton,
				 styleButtonShadeLight, styleButtonShadeDark,
				 1);
    imageButton.timerMs = 0;

    // Add a bookmark button to the control.
    BookmarkHandler = function(){
      this.buttonPressed = function(grpid, event, text){
	window.location = host_string + '?output=bookmark';	
      };
    };
    var bookmark_handler = new BookmarkHandler();
    var bookmarkButton = new button("BookmarkButton", control_context_id,
				    bookmark_handler,
				    "rect", "B", undefined,
				    150, 13, 15, 15,
				    styleButtonText, styleButton,
				    styleButtonShadeLight, styleButtonShadeDark,
				    1);
    bookmarkButton.timerMs = 0;

    // Do the next bit only if we found children above...
    if( number_of_children != 0 ){

      // Add a button (and label) to open all children.
      AllHandler = function(){
	this.buttonPressed = function(grpid, event, text){
	  window.location = host_string + '?action=open&acc=' + go_id;
	};
      };
      var all_handler = new AllHandler();
      var allButton = new button("allButton", control_context_id,
				 all_handler,
				 "rect", "+", undefined,
				 95, 112, 20, 20,
				 styleButtonText, styleButton,
				 styleButtonShadeLight, styleButtonShadeDark,
				 1);
      control_context.appendChild(svg_lib.makeText(120, 127,
						   'open',
						   '15px', ''));
      allButton.timerMs = 0;
      
      // Get all of the infor on the node children (will be used in
      // the construction of the combobox).
      var term_kid_nodes = new Array();
      var term_kid_nodes_hash = {};
      var term_kids_max_length = 0;
      for (var noise_kid_acc in amigo_terms[go_id]["children"]){
	
	var kid_name =  amigo_terms[go_id]["children"][noise_kid_acc]['name'];	
	term_kid_nodes.push({key:kid_name, value:false});
	term_kid_nodes_hash[kid_name] = noise_kid_acc;
	if( term_kids_max_length < kid_name.length ){
	  term_kids_max_length = kid_name.length;
	}
      }

      //
      ComboHandler = function(){
	this.getComboboxVals = function(grpid, strings, indexes){
	  // There should only be one, so we'll just operate on the first...
	  if( strings && strings.length > 0){
	    //alert('?action=add&acc=' + term_kid_nodes_hash[strings[0]]);
	    window.location =
	    host_string + '?action=add&acc='+term_kid_nodes_hash[strings[0]];
	  }
	};
      };
      var combo_handler = new ComboHandler();
      var comboLeBox = new combobox("ChildComboBox",
				    control_context_id,
				    term_kid_nodes,
				    //170,15,150,
				    10 * term_kids_max_length,15,150,
				    comboBoxCellHeight,
				    comboBoxTextpadding,
				    10,false,0,
				    comboBoxtextStyles,
				    comboBoxStyles,
				    comboBoxScrollbarStyles,
				    comboBoxSmallrectStyles,
				    comboBoxHighlightStyles,
				    comboBoxTriangleStyles,
				    combo_handler);    
      comboLeBox.timerMs = 0;
    }
  };
}
