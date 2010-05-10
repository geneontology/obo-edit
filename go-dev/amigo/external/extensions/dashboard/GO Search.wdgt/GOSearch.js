////
//// ...
////


// Global hard-wired variables.
var global_amigo_base = "http://amigo.berkeleybop.org/cgi-bin/amigo/";
//var global_amigo_gp_details = "gp-details.cgi?gp=";
//var global_amigo_term_details = "term-details.cgi?term=";

// Global Apple widgets.
var global_return_button = null;
var global_info_button = null;

org.bbop.amigo.DEBUG = true;
var core = new org.bbop.amigo.core();

//
function kvetch(msg){
    //jQuery('#kvetch').empty();
    //jQuery('#kvetch').html(msg);
    //core.kvetch(msg);
    var k = document.getElementById('kvetch');
    k.innerHTML = msg;
}


//
function GOSearchWidgetInit(){
    
    //kvetch(''); // console spacing

    // Create Apple widgets and attach their callbacks.
    try{
	global_return_button =
            new AppleGlassButton(document.getElementById("returnButton"),
    				 "Return",
    				 hideBack);
	global_info_button =
            new AppleInfoButton(document.getElementById("infoButton"),
    				document.getElementById("front"),
    				"black",
    				"black",
    				showBack);

	//kvetch('done adding apple widgets...');
    }catch(err){
	//kvetch('failed to add apple widgets...');
    }
    
    
    // TODO: Attach callbacks to form.
    function callback(event){
	//kvetch('event callback start...');
	//Event.stop(event);
	//var element = Event.element(event);
	formAction();
    }
    // TODO:
    //Event.observe('completion-form', 'change', callback);
    jQuery('#completion-form').keyup(callback);
    
    //kvetch('done adding events...');
}


//
function formAction(){
    
    //kvetch('in formAction...');
    
    // Squeeze data out of the form.
    var form = jQuery('#query-field');
    var query = form.val();    
    //kvetch('in formAction: query: ' + query);
    
    if( query && query.length >= 3 ){
	
	//kvetch('in formAction: if...');
	
	var type = 'gene_product';
	if( $('#type-field-term').attr('checked') ){
	    type = 'term';
	}
	
 	//kvetch('in formAction: type: ' + type);

	var all_inputs = 'format=opensearch' +
	    "&type=" + type +
	    "&query=" + encodeURIComponent(query);
	
	// Ready ajax and results actions.
	var url = global_amigo_base + 'completion' + '?' + all_inputs;
	//kvetch("in formAction: url: " + url);

	jQuery.ajax({
	    type: "GET",
	    url: url,
	    dataType: 'json',
 	    success: displayResults,
 	    error: function (result, status, error) {
 		//alert('Failed server request: ' + status);
 	    }
	});

	//kvetch("in formAction: after: url: " + url);
    }else{
	//kvetch("in formAction: query too short");
    }
}


//
function displayResults(json_data, status){

    //kvetch('ajax callback start...');

    // TODO: make this safer...
    //var results = org.bbop.amigo.json.parse(res_text);
    var results = json_data;
    var full_names = results[1];
    var symbols = results[2];
    var amigo_urls = results[3];
    
    //kvetch('data length...' + json_data.length + ':' + symbols.length);

    var mbuf = new Array();
    for( var i = 0; i < full_names.length; i++ ){
	
        var name = full_names[i];
        var sym = symbols[i];
        var aurl = amigo_urls[i];

	//
	var mysty = '';
	if( i % 2 == 0 ){ mysty = ' even_row'; }
	
	//
        mbuf.push('<div class="row ' +
    		  mysty + '">' + '<a title="' +
    		  sym + ' "href="#" onclick="amigo_link(' + "'" +
    		  aurl + "'" + ');">' +
    		  name + '</a>' + '</div>');
    }
    var new_res_str = mbuf.join('');
    
    // Apparently, dashboard doesn;t like these functions for DOM
    // manipulation.
    //jQuery('#results').empty();
    //jQuery('#results').html(new_res_str);
    var r = document.getElementById('results');
    r.innerHTML = new_res_str;
}


//
function amigo_link(item_url){
    if( widget && widget.openURL ){
	widget.openURL(item_url);
    }
}


//
function showBack(){

  var front = document.getElementById("front");
  var back = document.getElementById("back");
  if (window.widget){
    widget.prepareForTransition("ToBack");
  }
  front.style.display="none";
  back.style.display="block";
  if( window.widget ){
    setTimeout('widget.performTransition();', 0);
  }
}


//
function hideBack(){

  var front = document.getElementById("front");
  var back = document.getElementById("back");
  if( window.widget ){
    widget.prepareForTransition("ToFront");
  }
  back.style.display="none";
  front.style.display="block";
  if( window.widget ){
    setTimeout ('widget.performTransition();', 0);
  }  
}


// //
// function doit(url){
//     xmlRequest = new XMLHttpRequest();
//     xmlRequest.setRequestHeader("Cache-Control", "no-cache");
//     xmlRequest.onreadystatechange = processRequestChange;
//     xmlRequest.open("GET",url,true);
//     xmlRequest.send(null);
// }


// //
// function processRequestChange() {   
//     if (null == xmlRequest.readyState) return;
//     if (xmlRequest.readyState == 4) {
// 	if (xmlRequest.status == 200) {      
//             displayResults(xmlRequest.responseText);
//             parseRSS();
// 	} else {
//             if (null == xmlRequest.status) return;
//             displayMsg("noconnection");
// 	}
//     }
// }

