////
//// This could all be cleaned a bit with some lambda functions.
////

var nativeJSON =
  Components.classes["@mozilla.org/dom/json;1"].createInstance(Components.interfaces.nsIJSON);


//
var httpRequest;
var httpRequestCallbackFunction; 
var curr_url = "http://localhost/cgi-bin/amigo/completion?type=general&query=";


//
window.addEventListener("load", 
			function(){
			  var menu =
			    document.getElementById("contentAreaContextMenu");
			  menu.addEventListener("popupshowing",
						function(){
						  var menuitem = document.getElementById("amigofox_submenu");
						  if(menuitem)
						    menuitem.hidden = !gContextMenu.isTextSelected;
						},
						false);
			},
			false);


// Remove whitespace from the front and back of the highlighted
// section.
String.prototype.chomp = function(){
  return(this.replace(/^\s+/,'').replace(/\s+$/,''));
};


// Hack a line down to something reasonable, add ellipses.
String.prototype.truncate = function(){

  var len = 30;
  var str = this;
  if( this.length > len ){
    str = str.substr(0, len) + "...";
  }
  return str;
};


//



//
function jumpToAmiGO(url){
  try{
    var newTab = gBrowser.addTab(url);
    gBrowser.selectedTab = newTab;
  }catch(e){
    alert(e);
  }
}


//
function scanAmiGO(){

  blankMenu();
  
  try{

    //addMessage("Scanning...");
    var json = nativeJSON.decode(httpRequest.responseText);

    var echo = json[0];
    var comp = json[1];
    var desc = json[2];
    var qurl = json[3];

    //alert(json);
    
    if( comp && comp.length == 0 ){
      addMenuMessage("No such entity in AmiGO...");
    }else{
      for( var i = 0; i < comp.length; i++ ){
	addItem(comp[i].truncate(), qurl[i]);
      }
    }

  }catch(e){
    addMenuMessage("Error: No such entity or bad query.");
  }
}


//
function addItem(text, link){

  try{

    var big_menu = document.getElementById("amigofox_submenu");	
    var menu = big_menu.getElementsByTagName("menupopup");
    var menuitems = menu[0].getElementsByTagName("menuitem");
		
    var tempItem = document.createElement("menuitem");
    tempItem.setAttribute("label", text);
    tempItem.setAttribute("oncommand", "jumpToAmiGO('" + link + "');");
    menu[0].appendChild(tempItem);

  }catch(e){
    alert("Error: addItem");
  }
}


// //
function addMenuMessage(str){

  try{		

    var big_menu = document.getElementById("amigofox_submenu");
    var menu = big_menu.getElementsByTagName("menupopup");
    var menuitems = menu[0].getElementsByTagName("menuitem");
    
    // The adding.
    if( str && str.length > 0 ){
      var tempItem = document.createElement("menuitem");
      tempItem.setAttribute("label", str);
      menu[0].appendChild(tempItem);
    }

  }catch(e){
    alert("Error: addMenuMessage");
  }
}


//
function blankMenu(){

  try{		

    var big_menu = document.getElementById("amigofox_submenu");
    var menu = big_menu.getElementsByTagName("menupopup");
    var menuitems = menu[0].getElementsByTagName("menuitem");
    
    // The blanking.
    var num_items = menuitems.length -1;
    for( var i = 0; i <= num_items; i++ ){
      menu[0].removeChild(menu[0].childNodes.item(0));
    }

  }catch(e){
    alert("Error: blankMenu");
  }    
}


//
function getSelection(){
  var focusedWindow = document.commandDispatcher.focusedWindow;
  var selection = focusedWindow.getSelection();
  return String(selection).chomp();
}


//
function _httpExecuteCallback(){
  if (httpRequestCallbackFunction != null) {
    if (httpRequest.readyState == 4) {
      if (httpRequest.status == 200) {
	httpRequestCallbackFunction();
	httpRequestCallbackFunction = null;
      }
    }
  }
}


//
function _httpGet(url, callbackFunction){
  httpRequest = false;
  httpRequestCallbackFunction = callbackFunction;
  httpRequest = new XMLHttpRequest();
  httpRequest.onreadystatechange = _httpExecuteCallback;
  httpRequest.open('GET', url, true);
  httpRequest.send(null);
}


//
function lookup() {

  var str = getSelection();

  // TODO: actually go and get some term information for the GO ids.
  // This will mean that we need to add something similar to the
  // completion server--probably just a constraint on lucene would be
  // sufficient for our purposes...
  var go_id_re = /^GO\:[0-9]{7}$/;
  var go_key_re = /^[0-9]{7}$/;
  if( go_id_re.test(str) ){

    blankMenu();
    addItem(str, "http://localhost/cgi-bin/amigo/term_details?term=" + str);

  }else if( go_key_re.test(str) ){

    blankMenu();
    str = "GO:" + str;
    addItem(str, "http://localhost/cgi-bin/amigo/term_details?term=" + str);

  }else{
    _httpGet(curr_url + str, scanAmiGO);
  }
}
