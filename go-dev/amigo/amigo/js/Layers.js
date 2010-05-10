////
//// Experiments with OpenLayers
////
//// This is a static experiment.
////
//// See: http://openlayers.org/dev/examples/
////
//// TODO: mouseover details in separate window
////


// TODO: this should be dynamically created for each graph.
var graph_terms =
  {
    'GO:0008150': {lon: -101.866, lat: 80.746},
    'GO:0003674': {lon: -46.229,  lat: 80.746},
    'GO:0005575': {lon: 9.034,    lat: 80.746},

    'GO:0065007': {lon: -44.379, lat: 53.933},
    'GO:0050896': {lon: 106.018, lat: 53.933},

    'GO:0050789': {lon: -125.280, lat: 25.580},
    'GO:0032501': {lon: -33.439,  lat: 25.580},
    'GO:0065008': {lon: 27.275,   lat: 25.580},
    'GO:0009605': {lon: 157.640,  lat: 25.580},

    'GO:0048519': {lon: -144.696, lat: -5.547},
    'GO:0051239': {lon: -34.825,  lat: -5.547},
    'GO:0050817': {lon: 13.406,   lat: -5.547},
    'GO:0048583': {lon: 62.562,   lat: -5.547},
    'GO:0050878': {lon: 118.962,  lat: -5.547},

    'GO:0051241': {lon: -104.400, lat: -39.294},
    'GO:0050818': {lon: -42.761,  lat: -39.294},
    'GO:0032101': {lon: 91.687,   lat: -39.294},
    'GO:0007599': {lon: 140.997,  lat: -39.294},

    'GO:0050819': {lon: -53.009, lat: -75.815},
    'GO:0007596': {lon: 53.009,  lat: -75.815},
  };


// Keep things that we've requested and seen.
var term_cache = {};


//
function LayersInit (){

  // Map constructor, use 'map' div.
  var map = new OpenLayers.Map('map');

  // Main ontology layer/resource.
  var options = {numZoomLevels: 6};
  var wms =
    new OpenLayers.Layer.Image('Gene Ontology',
			       maploc,
			       new OpenLayers.Bounds(-180,-88.759,180,88.759),
			       //new OpenLayers.Size(1160,576),
			       new OpenLayers.Size(580,288),
			       options);
  map.addLayer(wms);

  //   // Add a WMS data layer (world map).
  //   var wms = new OpenLayers.Layer.WMS( "OpenLayers WMS",
  // 				      "http://labs.metacarta.com/wms/vmap0",
  // 				      {layers: 'basic'} );
  //    var jpl_wms = new OpenLayers.Layer.WMS( "Heat Map",
  //              "http://t1.hypercube.telascience.org/cgi-bin/landsat7",
  //            {layers: "landsat7"}, options);
  //map.addLayers([wms, jpl_wms]);
    
  map.addControl(new OpenLayers.Control.LayerSwitcher());
    
  // Add an overview map.
  var wms_ov =
    new OpenLayers.Layer.Image('Gene Ontology',
			       maploc,
			       new OpenLayers.Bounds(-180,-88.759,180,88.759),
			       //new OpenLayers.Size(1160,576),
			       new OpenLayers.Size(580,288),
			       //new OpenLayers.Size(290,144),
			       options);
  var overview_options = {
  layers: [wms_ov],
  //numZoomLevels: 1
  };
  var overview = new OpenLayers.Control.OverviewMap(overview_options);
  //overview.size.w = 160; // 4 * 40
  //overview.size.h = 120; // 3 * 40
  map.addControl(overview);
  
  map.addControl(new OpenLayers.Control.Permalink('permalink'));

  map.zoomToMaxExtent();

  map.events.register('click',
		      map,
		      function (e) {
			
			var x_pos = e.xy.x;
			var y_pos = e.xy.y;

			//
			var pixel = new OpenLayers.Pixel(x_pos, y_pos);
			var pixll = map.getLonLatFromPixel(pixel);
			var llcoords = "";
			var llcoords = pixll.lon + ', ' + pixll.lat;
			OpenLayers.Util.getElement('resLoc').innerHTML =
			  "Clicked: (" + x_pos + ", " + y_pos + ") [" +
			  llcoords + "]";
		      });

  // Add more generated layers.
  PathLayer(map, ['GO:0008150', 'GO:0065007', 'GO:0065008', 
		  'GO:0050878', 'GO:0007599']);
  HiliteLayer(map, ['GO:0005575', 'GO:0051241', 'GO:0007596']);
  ActionLayer(map);
  InfoLayer(map,[{id:'GO:0050789', text:"<b>Hey!</b><br />Nice to meet y'all!"},
		 {id: 'GO:0032501', text: 'Salutations!'},
		 {id: 'GO:0050896', text: 'Hello, World!'},
		 {id: 'GO:0007596', text: "I'm hiding!"}]);
}


///
/// Everything down here should be created dynamically using the API
/// for bridging the graph static render (with meta-info) and client
/// JS. For now, let's hard-code the example.
///

// Add special info balloon markers.
function InfoLayer (map, term_markers){

  //
  var info_markers = new OpenLayers.Layer.Markers("Additional GO info");
  info_markers.setVisibility(false);
  map.addLayer(info_markers);

  // Defined the actually marker adding function.
  function addMarker(ll, popupClass, popupContentHTML, closeBox, overflow) {

    var feature = new OpenLayers.Feature(info_markers, ll); 
    feature.closeBox = closeBox;
    feature.popupClass = popupClass;
    feature.data.popupContentHTML = popupContentHTML;
    feature.data.overflow = (overflow) ? "auto" : "hidden";
                    
    var marker = feature.createMarker();

    var markerClick = function (evt) {
      if (this.popup == null) {
	this.popup = this.createPopup(this.closeBox);
	map.addPopup(this.popup);
	this.popup.show();
      } else {
	this.popup.toggle();
      }
      currentPopup = this.popup;
      OpenLayers.Event.stop(evt);
    };
    marker.events.register("mousedown", feature, markerClick);
    
    info_markers.addMarker(marker);
  }

  //
  for( var ammi = 0; ammi < term_markers.length; ammi++ ) {

    //
    var term_id = term_markers[ammi].id;
    var text = term_markers[ammi].text;
    var term_lon = graph_terms[term_id].lon;
    var term_lat = graph_terms[term_id].lat;
   
    addMarker(new OpenLayers.LonLat(term_lon,term_lat),
	      OpenLayers.Popup.FramedCloud,
	      text,
	      true);
  }
}


// Add responsive layer to the base.
function ActionLayer (map){

  ///
  ///
  ///

  //
  function in_bounds_p (e, object){

    var retval = false;

    var mp = e.object.events.getMousePosition(e);
    var x_pos = mp.x;
    var y_pos = mp.y;
    var pixel = new OpenLayers.Pixel(x_pos, y_pos);
    var pixll = map.getLonLatFromPixel(pixel);
    var point =
      new OpenLayers.Geometry.Point(pixll.lon,
				    pixll.lat);
    if( object.containsPoint(point) ){
      retval = true;
    }

    return retval;
  }


  //
  function waitHTML() {
    OpenLayers.Util.getElement('htmlLoc').innerHTML =
      '<p>Getting term information...</p>';
  }
  function waitHTML2() {
    OpenLayers.Util.getElement('htmlLoc2').innerHTML =
      '<p>Getting navigation information...</p>';
  }
  //   function setHTML(response) {
  //     OpenLayers.Util.getElement('htmlLoc').innerHTML =
  //       response.responseText;
  //   }
  function setHTML(response_object) {
    //    alert(foo);
    OpenLayers.Util.getElement('htmlLoc').innerHTML =
      //'<h3>Info</h3>' +
      '<p><b>'+ response_object.acc +
      '</b><br />'+ response_object.name +
      '</p>'; 
  }
  function setHTML2(response_object) {

    var minibuf = [];

    //
    //minibuf.push('<h3>Info</h3>');
    //minibuf.push('<p><b>'+ response_object.acc);
    //minibuf.push('</b><br />'+ response_object.name);
    //minibuf.push('</p>');

    //
    if( response_object.children ){
      minibuf.push('<b>Navigation from ');
      minibuf.push(response_object.acc);
      minibuf.push('</b> <p>');
      minibuf.push('<a href="">open all</a>');
      minibuf.push('<br />');
      minibuf.push('<a href="">close all</a>');
      minibuf.push('</p> <p>');
      minibuf.push('Children:<br />');
      for( var kid in response_object.children ){
	var kobj = response_object.children[kid];
	minibuf.push('<a href="">');
	minibuf.push(kobj.acc);
	minibuf.push('</a>');
	minibuf.push('<br />');
      }
      minibuf.push('</p>');
    }else{
      minibuf.push('<h4>No children</h4>');
    }

    OpenLayers.Util.getElement('htmlLoc2').innerHTML = minibuf.join('');
  }
  

  ///
  ///
  ///

  //
  var style_blue =
    OpenLayers.Util.extend({}, OpenLayers.Feature.Vector.style['default']);
  //style_blue.strokeColor = "blue"; 
  //style_blue.fillColor = "blue"; 
  style_blue.fillOpacity = 0.0; 
  style_blue.strokeOpacity = 0.0; 

  var vectorLayer = new OpenLayers.Layer.Vector("[GO action layer]");
  var features = [];
            
  for( var term_id in graph_terms ){

    //
    var term_lon = graph_terms[term_id].lon;
    var term_lat = graph_terms[term_id].lat;
    
    //
    var origin = new OpenLayers.Geometry.Point(term_lon, term_lat);
    var polygon =
      new OpenLayers.Geometry.Polygon.createRegularPolygon(origin, 15, 4);
    // WARNING:
    polygon.term_id = term_id; // Huh!? Closures didn't work, but this did?
    vectorLayer.events.register("dblclick",
				polygon,
				function (e) {
				  if( in_bounds_p(e, this) ){

				    waitHTML2();

				    // Check cache.
				    if( term_cache[this.term_id] ){

				      setHTML2(term_cache[this.term_id]);

				    }else{

				      var url = servloc + this.term_id;
				      OpenLayers.loadURL(url, '', this,
							 function (response) {
							   var djson = new OpenLayers.Format.JSON();
							   var foo = djson.read(response.responseText);
							   term_cache[this.term_id] = foo;
							   setHTML2(term_cache[this.term_id]);
							 });
				    }
				    OpenLayers.Event.stop(e);
				  }
				});

    // TODO/BUG: this is a little laggy--why?
    vectorLayer.events.register("mouseover",
				polygon,
				function (e) {
				  if( in_bounds_p(e, this) ){

				    waitHTML();

				    // Check cache.
				    if( term_cache[this.term_id] ){

				      setHTML(term_cache[this.term_id]);

				    }else{

				      var url = servloc + this.term_id;
				      OpenLayers.loadURL(url, '', this,
							 function (response) {
							   var djson = new OpenLayers.Format.JSON();
							   var foo = djson.read(response.responseText);
							   term_cache[this.term_id] = foo;
							   setHTML(foo);
							 });
				      OpenLayers.Event.stop(e);
				    }
				  }
				});

    features.push(new OpenLayers.Feature.Vector(polygon, null, style_blue));
  }

  //
  vectorLayer.addFeatures(features);
  map.addLayer(vectorLayer);
}


// Add responsive layer to the base.
function HiliteLayer (map, terms_to_hilite){

  //
  var style =
    OpenLayers.Util.extend({}, OpenLayers.Feature.Vector.style['default']);
  style.strokeColor = "green"; 
  style.fillColor = "green"; 
  //style.fillOpacity = 0.0; 
  //style.strokeOpacity = 0.0; 

  var vectorLayer = new OpenLayers.Layer.Vector("GO hilite layer");
  vectorLayer.setVisibility(false);

  var features = [];
  for( var tthi = 0; tthi < terms_to_hilite.length; tthi++ ) {

    //
    var term_id = terms_to_hilite[tthi];
    var term_lon = graph_terms[term_id].lon;
    var term_lat = graph_terms[term_id].lat;
    
    //
    var origin = new OpenLayers.Geometry.Point(term_lon, term_lat);
    var polygon =
      new OpenLayers.Geometry.Polygon.createRegularPolygon(origin, 20, 20);
    features.push(new OpenLayers.Feature.Vector(polygon, null, style));
  }

  //
  vectorLayer.addFeatures(features);
  map.addLayer(vectorLayer);
}


// Add a happy path.
function PathLayer (map, term_path){

  var vectorLayer = new OpenLayers.Layer.Vector("Interesting path");
  vectorLayer.setVisibility(false);

  var style = {
  strokeColor: "#ff0000",
  strokeWidth: 15,
  strokeOpacity: .33,
  //strokeDashstyle: "dashdot",
  pointRadius: 6,
  pointerEvents: "visiblePainted"
  };

  //
  var pointList = [];
  var newPoint = point;
  for( var ti = 0; ti < term_path.length; ti++ ) {

    var term_id = term_path[ti];
    var term_lon = graph_terms[term_id].lon;
    var term_lat = graph_terms[term_id].lat;

    var point = new OpenLayers.Geometry.Point(term_lon, term_lat);
    pointList.push(point);
  }

  var lineFeature =
    new OpenLayers.Feature.Vector(
				  new OpenLayers.Geometry.LineString(pointList),
				  null,
				  style);

  //
  vectorLayer.addFeatures([lineFeature]);
  map.addLayer(vectorLayer);
}
