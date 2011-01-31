////
//// NOTE: uses the jQuery library.
////
//// BUG: Heat map always seems to get added the number of navigation
//// actions since the start.
////


// Add responsive layer to the base randomly hilite terms.
function RandomTermHiliteLayer (){

    //
    var def = OpenLayers.Feature.Vector.style['default'];
    var style = OpenLayers.Util.extend({}, def);
    style.strokeColor = "blue"; 
    style.fillColor = "blue"; 
    //style.fillOpacity = 0.0; 
    //style.strokeOpacity = 0.0; 

    var vectorLayer =
	new OpenLayers.Layer.Vector("Random Term Hilite (plugin)");
    vectorLayer.setVisibility(false);

    var features = new Array();
    var all_term_accs = GONavi.term_list();
    for( var a = 0; a < all_term_accs.length; a++ ){

	var acc = all_term_accs[a];
	var term_data = GONavi.term_data(acc);

	//
	if( Math.random() > 0.5 ){

	    var term_lon = term_data.position.lon;
	    var term_lat = term_data.position.lat;
	    
	    //
	    var origin = new OpenLayers.Geometry.Point(term_lon, term_lat);
	    var halo_size = GONavi.map_halo_size();
	    var polygon =
		new OpenLayers.Geometry.Polygon.createRegularPolygon(origin,
								     halo_size,
								     25);
	    features.push(new OpenLayers.Feature.Vector(polygon, null, style));
	}
    }

    //
    //dump("_term_features_: " + features.length + "\n");
    vectorLayer.addFeatures(features);
    GONavi.register_layer(vectorLayer);
}


// Add a happy path.
function RandomPathHiliteLayer (){

    var def = OpenLayers.Feature.Vector.style['default'];
    var style = OpenLayers.Util.extend({}, def);
    style.strokeColor = "green"; 
    style.strokeWidth = 7.5; 
    style.strokeOpacity = 0.9; 

    var pathLayer = new OpenLayers.Layer.Vector("Random Path Hilite (plugin)");
    pathLayer.setVisibility(false);
    
    var features = new Array();
    var all_edge_accs = GONavi.edge_list();
     for( var a = 0; a < all_edge_accs.length; a++ ){

 	 // For half of the paths...
 	 if( Math.random() > 0.5 ){

 	    var acc = all_edge_accs[a];
 	    var edge_data = GONavi.edge_data(acc);

	     //
 	     var points = new Array();
 	     var waypoints = edge_data['waypoints'];
 	     for( var w = 0; w < waypoints.length; w++ ){
 		 var wp = waypoints[w];
  		 var point = new OpenLayers.Geometry.Point(wp.lon, wp.lat);
		 points.push(point);
 	     }

	     features.push(
		 new OpenLayers.Feature.Vector(
		     new OpenLayers.Geometry.LineString(points),
		     null,
		     style)
	     );	
 	 }
     }
    
    //
    pathLayer.addFeatures(features);
    GONavi.register_layer(pathLayer);
}


// TODO: Needs doing...broken after inline upgrade...
function BookInit (){
    jQuery('#bookmark').mouseover(
	function(event) {

	    var all = GONavi.current_lonlat();
	    var alon = all.lon;
	    var alat = all.lat;
	    var az = GONavi.current_zoom();
	    var apf = GONavi.previous_focus();
	    var atl = GONavi.term_list().join(' ');
	    if( alon &&
		alat &&
		az &&
		apf &&
		atl ){

		GONavi.core.kvetch('BookInit: lon: ' + alon);
		GONavi.core.kvetch('BookInit: lat: ' + alat);
		GONavi.core.kvetch('BookInit: zoom: ' + az);
		GONavi.core.kvetch('BookInit: focus: ' + apf);
		GONavi.core.kvetch('BookInit: terms: ' + atl);

		GONavi.core.kvetch('BookInit: old href: ' +
				   jQuery('#bookmark').attr('href'));

		var new_link = GONavi.core.link.layers_graph(
		    {
			lon: alon,
			lat: alat,
			zoom: az,
			focus: apf,
			terms: GONavi.term_list()
		    }
		);
		GONavi.core.kvetch('BookInit: new link: ' + new_link);

		jQuery('#bookmark').attr('href', new_link);

	    }else{
		GONavi.core.kvetch('BookInit: no bookmark yet...');
	    }
	});
    //   GONavi.register_control(new OpenLayers.Control.Permalink('bookmark',
    // 							     'foo'));
}


//
function PlugInit(){

    var widgets = new org.bbop.amigo.ui.widgets();

    var feature2acc = {};

    //
    jQuery('#annotation_heat_plugin').unbind('click');

    // Generate layer on link click, just one time.
    var not_fired = 0;
    jQuery('#annotation_heat_plugin').click(
	function(event) {

	    GONavi.core.kvetch('PlugInit: click event fired: ' + not_fired);

	    // Prevent actual link and fire just once.
	    event.preventDefault();
	    //jQuery('#annotation_heat_plugin').unbind('click');
	    if( not_fired == 0 ){
	    	not_fired = 1;

		// Generate service URL.
		var my_terms = GONavi.term_list();
		var mt_cache = new Array();
		for( var mt = 0; mt < my_terms.length; mt++ ){
		    mt_cache.push(my_terms[mt]);
		}
		var my_term_string = mt_cache.join(' ');
		
		var service_string = 'aserve_exp?';
		var request_string = service_string +
		    'mode=term&full=true&term=' +
		    encodeURIComponent(my_term_string);

		//
		GONavi.core.kvetch("PlugInit: request_string: "+request_string);
		widgets.start_wait('Requesting data...');
		jQuery.ajax({
		    type: 'GET',
		    url: request_string,
		    dataType: 'json',
		    success: function(data, textStatus) {
			
			GONavi.core.kvetch('PlugInit: ajax success: '
					   + not_fired);

			GONavi.core.kvetch("_textStatus: " + textStatus);
			if( data && data.results ){
			    GONavi.core.kvetch("_data.type: " + data.type);
			    create_layer_with_results(data.results,
						      feature2acc);
			}
			widgets.finish_wait();
		    },
		    error: function(results, status, error){

			GONavi.core.kvetch('PlugInit: ajax failure: '
					   + not_fired);
			widgets.finish_wait();
		    }
		});
	    }
	});
}


//
function create_layer_with_results(result_obj, feature2acc){

    GONavi.core.kvetch('PlugInit:create_layer_with_results: starting...');

    //var all_my_popups = new Array();
    var vectorLayer =
	new OpenLayers.Layer.Vector("Annotation Heat Map (plugin)");
    vectorLayer.setVisibility(false);

    // Get the high and low direct values.
    var highest = 0;
    var lowest = 9999999;
    for( var term_id in result_obj ){
	if( highest < result_obj[term_id].direct_count ){
	    highest = result_obj[term_id].direct_count;
	}
	if( lowest > result_obj[term_id].direct_count ){
	    lowest = result_obj[term_id].direct_count;
	}
    }
    _calc_color_map(highest, lowest);

    GONavi.core.kvetch('_high: ' + highest);
    GONavi.core.kvetch('_low: ' + lowest);

    //
    var features = new Array();
    //var all_term_accs = GONavi.term_list();
    for( var acc in result_obj ){

	if( result_obj[acc].direct_count > 0 ){

	    //var acc = all_term_accs[a];
	    var term_data = GONavi.term_data(acc);

	    var term_lon = term_data.position.lon;
	    var term_lat = term_data.position.lat;
	    
	    //
	    var origin = new OpenLayers.Geometry.Point(term_lon, term_lat);
	    var halo_size = GONavi.map_halo_size();

	    //
	    //GONavi.core.kvetch('_count: ' + result_obj[acc].direct_count);
	    var cstring = _get_color_offset(result_obj[acc].direct_count);
	    //dump('color 1: ' + cstring + "\n");
	    cstring = cstring.toString(16);
	    //dump('color 2: ' + cstring + "\n");
	    if( cstring.length == 1 ){	    
		cstring = '0' + cstring;
	    }
	    cstring = '#ff' + cstring + cstring;
	    //dump('color 3: ' + cstring + "\n");

	    var def = OpenLayers.Feature.Vector.style['default'];

	    // 
	    var style_filling = OpenLayers.Util.extend({}, def);
	    style_filling.strokeColor = cstring;
	    style_filling.fillColor = cstring;
	    //style.fillOpacity = 0.0; 
	    //style.strokeOpacity = 1.0; 


	    var style_ring = OpenLayers.Util.extend({}, def);
	    style_ring.strokeColor = '#ff0000';
	    //style.fillColor = cstring;
	    style_ring.fillOpacity = 0.0; 
	    style_ring.strokeOpacity = 1.0; 

	    var polygon_filling =
		new OpenLayers.Geometry.Polygon.createRegularPolygon(origin,
								     halo_size,
								     25);
	    var polygon_ring =
		new OpenLayers.Geometry.Polygon.createRegularPolygon(origin,
								     halo_size,
								     25);

	    //
	    var f1 = new OpenLayers.Feature.Vector(polygon_filling, null,
						   style_filling);
	    var f2 = new OpenLayers.Feature.Vector(polygon_ring, null,
						   style_ring);
	    features.push(f1);
	    features.push(f2);
	    feature2acc[f1.id] = acc;
	    feature2acc[f2.id] = acc;
	    
	    ///
	    ///
	    ///
	    
// 	    function selected (evt) {
// 		alert(evt.feature.id + " selected on " + this.name);
// 	    }
// 	    vectorLayer.events.register("featureselected",
// 					vectorLayer,
// 					selected);
	    var scontrol = new OpenLayers.Control.SelectFeature(vectorLayer);

	    //map.addControl(control);
	    GONavi.register_control(scontrol);
// 	    scontrol.hover = true;
// 	    scontrol.overFeature = function(f){
// 		alert(f);
// 	    };
// 	    scontrol.outFeature = function(f){
// 		alert(f);
// 	    };
	    scontrol.activate();

	    var popup = null;

	    function onPopupClose(evt) {
		// 'this' is the popup.
		scontrol.unselect(this.feature);
	    }

	    function onFeatureOver(evt) {
		//var feature = evt.feature;
		var feature = vectorLayer.getFeatureFromEvent(evt);
		if( feature && feature2acc[feature.id] ){
		    var facc = feature2acc[feature.id];
		    var td = GONavi.term_data(facc);

		    // Actually create the popup object--coords, etc.
		    var cll = feature.geometry.getBounds().getCenterLonLat();
		    var lat_boost = - (GONavi.map_halo_size() / 2.0);
		    var lon_boost = GONavi.map_halo_size() / 2.0;
		    var popll =
 			new OpenLayers.LonLat(cll.lon + lon_boost,
 					      cll.lat + lat_boost);
		    popup = new OpenLayers.Popup(
			"featurePopup",
			popll,
			new OpenLayers.Size(200,200),
			"<b>" + td.information.name + "</b><br />" +
			    facc + '<br />[' +
			    result_obj[facc].direct_count + '/' +
			    result_obj[facc].indirect_count + ']',
			false,
			onPopupClose);
		    feature.popup = popup;
		    popup.feature = feature;

		    popup.setOpacity(0.85);
		    popup.maxSize = new OpenLayers.Size(300,300);
		    popup.minSize = new OpenLayers.Size(150,117);
		    popup.autoSize = true;
		    //popup.contentDiv.style.overflow = 'auto';
		    //popup.contentDiv.style.border = '1px solid #000000';
		    popup.setBorder('solid 1px');
		    popup.setBackgroundColor('#ffff99');

		    GONavi.register_popup(popup);
		}
	    }

	    // BUG/TODO: make sure that we're actually out, and not
	    // just over the feature...
	    function onFeatureOut(evt) {

		// Get feature and make sure that it is popped.
		var feature = vectorLayer.getFeatureFromEvent(evt);
		if( feature ){

		    // Where is the mouse?
		    var mp = evt.object.events.getMousePosition(evt);
		    var pixel = new OpenLayers.Pixel(mp.x, mp.y);

		    // Is it not still over the feature?
		    if( ! feature.geometry.bounds.containsPixel(pixel) ){
			// Then destroy it my pretties!
			popup.feature = null;
			if( feature.popup ){
			    GONavi.unregister_popup(feature.popup);
			    feature.popup.destroy();
			    feature.popup = null;
			}
		    }
		}
	    }

	    // NOTE: this is reduntant to have this in here every
	    // time, but we need to keep it in here as the two
	    // callbacks would be out of scope otherwise.
	    vectorLayer.events.on({
		'mouseover': onFeatureOver,
		'mouseout': onFeatureOut
	    });
	}
    }

    //     //
    //     vectorLayer.events.on({
    // 	'mouseover': onFeatureOver,
    // 	'mouseout': onFeatureOut
    //     });
    vectorLayer.addFeatures(features);
    GONavi.register_layer(vectorLayer);
}


///
/// Functions for getting offset from the base color.
///

var _base = 255;
var _step = 1;
var _low = 0;
function _calc_color_map(hi, lo){
    _step = _base / (hi - lo);
    _low = lo;
}
function _get_color_offset(n){
    return _base - Math.round((n - _low) * _step);
}
