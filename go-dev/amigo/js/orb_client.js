//
// BUG: Track down that ENVO problem (category and group have no
// effect).
//

// Core objects.
var orb = new ORBServer({trackerInformation: writeTrackerInformation,
      //      items: writeItems
});
var form = new ORBForm({submit: writeResults});



function Initialize(){

  // 
  attachToggle('option_toggle', 'option_toggle_image',
	       '/amigo/images/orb_nav_open.png',
	       '/amigo/images/orb_nav_closed.png',
	       'option_menu', 'option_area');
  attachToggle('mode_toggle', 'mode_toggle_image',
	       '/amigo/images/orb_nav_open.png',
	       '/amigo/images/orb_nav_closed.png',
  	       'mode_menu');
  attachToggle('danger_toggle', 'danger_toggle_image',
	       '/amigo/images/orb_nav_open.png',
	       '/amigo/images/orb_nav_closed.png',
  	       'danger_menu');

  // Ready the retrieve button.
  var retrieve = document.getElementById("retrieve_button");
  retrieve.onclick = function(){
    var default_string = "Get all proposed terms";
    var foo = prompt("Enter the username of the proposed terms you wish to retrieve. Leave untouched if you wish to retrieve all of them.",
		     default_string);
    // Check whether or not we'll get everybody's. Remember: 'cancel'
    // will return null so we need a default value in the box.
    if( foo ){

      var filter = '';
      if( foo != default_string ){
	filter = foo; }      
      
      //
      //orb.getItems('TEST', 'true', filter);

      var link = document.getElementById('link');
      link.innerHTML =
	'<p><a href="' +
	'http://toy.lbl.gov:9002/cgi-bin/amigo2/orb.cgi?request=items&ontology_id=TEST&detailed_information=true&format=obo&username=' + filter +
	'><em>link</em></a> to personal ontology</p>';
    }
  };

  // Display tracker status (category and artifact_group) and items.
  //writeTrackers();
  form.set('ontology_id', 'TEST');
  orb.getTrackerInformation('TEST');
  //orb.getItems('TEST');
  //writeReport();


  // Prepare summary, definition, and details for callback.
  writeSummary();
  writeDefinition();
  writeDetails();

  writeOptionals();

  // Write initial report.
  writeReport();  

  writeModes();

  // Ready full mode.
  //var full = document.getElementById("full_update");
  //full.onclick = getFullItems;

  // Ready detailed mode.
  //var detailed = document.getElementById("detailed_update");
  //detailed.onclick = getDetailedItems;
}


// TODO: search for hidden so we can have multiple classes.
function attachToggle(toggle_id, toggle_image_id, open_image, closed_image){

  var toggle_element = document.getElementById(toggle_id);
  var attach_args = arguments;

  toggle_element.onclick = function(){

    var toggle_image_element = document.getElementById(toggle_image_id);
    var img_src = toggle_image_element.src;

    var new_class = '';
    var open_image_regexp = new RegExp(open_image + '$');
    if( img_src.match(open_image_regexp) ){
      new_class = 'hidden';
      toggle_image_element.src = closed_image;
    }else{
      toggle_image_element.src = open_image;
    }
 
    for( var i = 4; i < attach_args.length; i++ ){
       var element = document.getElementById(attach_args[i])
      element.setAttribute("class", new_class);
      element.setAttribute("className", new_class);
    }
  };
}


/////
//
// Writers.
//
/////


// Display input errors.
function writeReport(){

  var review = form.review();

  var all_clear = true;
  //var html = new Array;
  //html.push('<table class="list">');
  for( var i = 0; i < review.length; i++ ){

    // Skip agent, ontology id, and username (it is GO).
    if( review[i].name != 'agent' && review[i].name != 'ontology_id' &&
	review[i].name != 'username' ){

      // Grab image and td elements from html page.
      var change_lbl = document.getElementById('label_' + review[i].name);
      var change_img = document.getElementById('image_' + review[i].name);

      if( review[i].required && ! review[i].ok ){
	all_clear = false;
	change_lbl.setAttribute("class", "list_item_bad");
	change_lbl.setAttribute("className", "list_item_bad");
	change_img.src = "/amigo/images/orb_led_red.png";
      }else if( ! review[i].required && ! review[i].ok ){
	change_lbl.setAttribute("class", "list_item_neutral");
	change_lbl.setAttribute("className", "list_item_neutral");
	change_img.src = "/amigo/images/orb_led_grey.png";
      }else{
	change_lbl.setAttribute("class", "list_item_good");
	change_lbl.setAttribute("className", "list_item_good");
	change_img.src = "/amigo/images/orb_led_green.png";
      }
    }
  }

  //
  var html = new Array;
  if( all_clear ){
    html.push('<input type="button" name="submit_button" id="submit_button" ');
    html.push(' value="Submit" class="happy_button"/>');
  }else{
    html.push('<input type="button" name="submit_button" id="submit_button" ');
    html.push(' value="Submit" disabled="disabled" />');
  }
  var buttons = document.getElementById('submitter');
  buttons.innerHTML = html.join('');

  // Add listener to submit button.
  document.generated_form.submit_button.onclick = function(){
    form.submit();
  };
}


//
function writeResults( result_struct ){

  //
  var ont_struct = form.get('ontology_id');
  var known_trackers = orb.getTrackers();
  var url = '';
  var ont_id = '';
  for( var i = 0; i < known_trackers.length; i++ ){
    if( known_trackers[i].ontology_id == ont_struct.name ){
      url = known_trackers[i].url;
      i = known_trackers.length; // Short-circut
    }
  }

  //
  var html = new Array;
  if( result_struct.success ){

    // Make a nice message.
    html.push('<p class="success">');    
    html.push('<b>Success ID: <a href="');
    html.push(url + '&func=detail&aid=' + result_struct.item_id);
    html.push('">');
    html.push(result_struct.item_id);
    html.push('</a></b></p>');

  }else{
    html.push('<p class="error">');    
    html.push('<b>');
    html.push('Error: ' + result_struct.message);
    html.push('</b></p>');
  }

  var results = document.getElementById('results');
  results.innerHTML = html.join('');
}


// Write status to div.
function writeTrackerInformation( cat_stats, art_stats ){

  var html = new Array;
  html.push('<table>');
  for( var i = 0; i < cat_stats.length; i++ ){
    html.push('<tr><td>');
    html.push('<input type="radio" name="category_id" value="');
    html.push(cat_stats[i].value + '" />');
    html.push('</td><td><b>');
    html.push(cat_stats[i].name);
    html.push('</b></td></tr>');
  }
  html.push('</table>');

  var categories = document.getElementById('categories');
  categories.innerHTML = html.join('');

  // Add listeners to category_id radio.
  for( var i = 0; i < document.generated_form.category_id.length; i++ ){
    document.generated_form.category_id[i].onclick = function(){
      form.set('category_id', this.value);
      writeReport();
    }
  }
  
  // Display artifact groups.
  html = new Array;
  html.push('<table>');
  for( var i = 0; i < art_stats.length; i++ ){
    html.push('<tr><td>');
    html.push('<input type="radio" name="artifact_group_id" value="');
    html.push(art_stats[i].value + '" />');
    html.push('</td><td><b>');
    html.push(art_stats[i].name);
    html.push('</b></td></tr>');
  }
  html.push('</table>');

  var artifacts = document.getElementById('artifacts');
  artifacts.innerHTML = html.join('');
  
  // Add listeners to artifact_group radio.
  for( var i = 0; i < document.generated_form.artifact_group_id.length; i++ ){
    document.generated_form.artifact_group_id[i].onclick = function(){
      form.set('artifact_group_id', this.value);
      writeReport();
    };
  }
}


//
function writeItems( item_stats ){

  var d = new Date();
  var day = d.getDate();
  if( day < 10 ){ day = '0' + day; }
  var month = d.getMonth();
  if( month < 10 ){ month = '0' + month; }
  var year = d.getFullYear();
  if( year < 10 ){ year = '0' + year; }
  var hour = d.getHours();
  if( hour < 10 ){ hour = '0' + hour; }
  var minute = d.getMinutes();
  if( minute < 10 ){ minute = '0' + minute; }

  var queue = new Array;

  // Header.
  queue.push('format-version: 1.2');
  queue.push('date: ' + 
	     day + ':' +
	     month + ':' +
	     year + ':' +
	     hour + ':' +
	     minute);
  queue.push('auto-generated-by: AmiGO ORB Client 0.1b');
  queue.push('\n');

  // Stanzas.
  for( var i = 0; i < item_stats.length; i++ ){

    var item = item_stats[i];
    //queue.push(item['priority']);
    //queue.push('<br />');
    //queue.push(item['resolution_status']);
    //queue.push('<br />');
    //queue.push(item['status']);
    //queue.push('<br />');
    //queue.push(item['assigned_to']);
    //queue.push('<br />');
    //queue.push('resolution:' + item['resolution']);
    //queue.push('modtype:' + item['modtype']);
    //queue.push('agent:' + item['agent']);
    //queue.push('submitted_by:' + item['submitted_by']);
    queue.push('[Term]');
    queue.push('id: ' + item['submitted_by'] + ':' + item['item_id']);
    queue.push('name: ' + item['summary']);
    queue.push('comment: ' + item['details']);
    queue.push('creation_date: ' + item['open_date']);
    queue.push('definition: "' + item['definition'] +
	       '" [orb:' + item['submitted_by'] + ']');
    queue.push('attribution: ' + item['attribution']);
    queue.push('\n');
  }

  //var file = document.getElementById('obo_file');
  document.open("text/unknown");
  document.write(queue.join('\n'));
		 //file.innerHTML = queue.join('');
  document.close();
}


// //
// function writeItems( item_stats ){

//   // Need to get ontology information to form the URLs.
//   var ont_struct = form.get('ontology_id');
//   var known_trackers = orb.getTrackers();
//   var url = '';
//   for( var i = 0; i < known_trackers.length; i++ ){
//     if( known_trackers[i].ontology_id == ont_struct.name ){
//       url = known_trackers[i].url;
//     }
//   }

//   // Determine which table we will write to.
//   var queue = null;
//   if( item_stats &&
//       item_stats[0] &&
//       item_stats[0].resolution ){
//     queue = document.getElementById('detailed_queue');
//   }else{
//     queue = document.getElementById('full_queue');
//   }

//   //
//   var html = new Array;
//   html.push('<table>');
//   for( var i = 0; i < item_stats.length; i++ ){
//     html.push('<tr>');
//     if( url ){
//       html.push('<td><a href="');
//       html.push(url + '&func=detail&aid=' + item_stats[i].ontology_id);
//       html.push('"><b>');
//       html.push(item_stats[i].ontology_id);
//       html.push('</b></a>: </td><td>');
//     }else{
//       html.push('<td><b>');      
//       html.push(item_stats[i].ontology_id);
//       html.push('</b>: </td><td>');
//     }
//     html.push(item_stats[i].summary);
//     html.push(' (');
//     html.push(item_stats[i].open_date);
//     html.push(')');
//     html.push('</td>');
//     html.push('</tr>');
//   }
//   html.push('</table>');

//   var queue = document.getElementById('queue');
//   queue.innerHTML = html.join('');
// }


//
function getFullItems( event ){

  if( form.get('ontology_id').name ){

    orb.getItems(form.get('ontology_id').name, 'false');
		 
  }else{
    var queue = document.getElementById('full_queue');
    queue.innerHTML = '<b>select ontology in modification mode first!</b>';
  }
}
 
 
//
function getDetailedItems( event ){

  if( form.get('ontology_id').name &&
      form.get('username').name ){

    orb.getItems(form.get('ontology_id').name, 'true',
		 form.get('username').name);
		 
  }else{
    var queue = document.getElementById('detailed_queue');
    queue.innerHTML = '<b>select ontology in modification mode and username first!</b>';
  }
}  


// Display trackers.
function writeTrackers(){

  var known_trackers = orb.getTrackers();

  var html = new Array;
  html.push('<table>');
  for( var i = 0; i < known_trackers.length; i++ ){
    html.push('<tr><td>');
    html.push('<input type="radio" name="ontology_id" value="' +
	      known_trackers[i].ontology_id + '"/>');
    html.push('</td><td><b>');
    html.push(known_trackers[i].ontology_id);
    html.push('</b>: ');
    html.push(known_trackers[i].description);
    html.push('</td></tr>');
  }
  html.push('</table>');

  var trackers = document.getElementById('trackers');
  trackers.innerHTML = html.join('');

  // Add listeners to trackers radio.
  for(var i = 0; i < document.generated_form.ontology_id.length; i++ ){
    document.generated_form.ontology_id[i].onclick = function(){
      form.set('ontology_id', this.value);
      orb.getTrackerInformation(this.value);
      //orb.getItems(this.value, 'false');
      writeReport();
    };
  }
}


// 
function writeSummary(){
  document.generated_form.summary.onkeyup = function(){
    form.set('summary', this.value);
    writeReport();
  };
}


// Definition.
function writeDefinition(){
  document.generated_form.definition.onkeyup = function(){
    form.set('definition', this.value);
    writeReport();
  };
}


//
function writeDetails(){
  document.generated_form.details.onkeyup = function(){
    form.set('details', this.value);
    writeReport();
  };
}


// Add listeners to all non-dynamic optional items.
function writeOptionals(){

  // Modtype.
  for(var i = 0; i < document.generated_form.modtype.length; i++ ){
    document.generated_form.modtype[i].onclick = function(){
      form.set('modtype', this.value);
      writeReport();
    };
  }

  // Username.
  //document.generated_form.username.onkeyup = function(){
  //  form.set('username', this.value);
  //  writeReport();
  //};

  // Login.
  document.generated_form.login.onkeyup = function(){
    form.set('login', this.value);
    writeReport();
  };

  // Password.
  document.generated_form.password.onkeyup = function(){
    form.set('password', this.value);
    writeReport();
  };

  // Attribution.
  document.generated_form.attribution.onkeyup = function(){
    form.set('attribution', this.value);
    writeReport();
  };
}


// Add listeners to mode/view items.
function writeModes(){

  var areas = ['modification_mode_area',
	       'full_queue_mode_area',
	       'detailed_queue_mode_area'];
  //var areas = ['propose_mode_area',
  //	       'queue_mode_area',
  //	       'proposal_mode_area'];

  // Mode.
  for(var i = 0; i < document.generated_form.mode.length; i++ ){
    document.generated_form.mode[i].onclick = function(){

      // For every one, either turn on or off.
      for( var j = 0; j < areas.length; j++ ){
	var element = document.getElementById(areas[j])
	if( this.value + '_mode_area' == areas[j] ){

	  element.setAttribute("class", '');
	  element.setAttribute("className", '');

	}else{

	  element.setAttribute("class", 'hidden');
	  element.setAttribute("className", 'hidden');

	}
      }
    };
  }
}
