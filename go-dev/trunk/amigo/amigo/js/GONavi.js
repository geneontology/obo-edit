////
//// Experiments with OpenLayers
////
//// This is a dynamic experiment.
////
//// See: http://openlayers.org/dev/examples/
////
//// NOTE: Done for OpenLayers 2.8.
////
//// BUG: It looks like there is still a problem with the zoom
////      when we cross lon/lat order of magnitude. We could
////      fix this by not having the order zoom jumps but...
////

//
org.bbop.amigo.DEBUG = true;

// Manual namespace.
var GONavi = {};

//
GONavi.Init = function(plugins){

    jQuery.noConflict();
    var core = new org.bbop.amigo.core();
    var widgets = new org.bbop.amigo.ui.widgets();

    ///
    /// Map-centered variables.
    ///

    // This is getting a bit cute, but we're going to assume that the
    // GONavi.Data variable was read from another file before this
    // function is run.
    var map_opts = 
	{
	    controls: []
	};
    // We start with no default opts.
    GONavi.Data['openlayers_map'] = new OpenLayers.Map('go_map', map_opts);
    
    var global_map = GONavi.Data['openlayers_map'];
    var global_world = GONavi.Data['map_world_information'];
    var global_graph_terms = GONavi.Data['map_term_information'];
    var global_graph_edges = GONavi.Data['map_edge_information'];
    var global_maploc = GONavi.Data['map_url'];
    var global_maploc_mini = GONavi.Data['map_mini_url'];
    var global_previous_position = GONavi.Data['map_previous_position'];

    // GO-centered variables.
    var global_go_terms = GONavi.Data['go_term_information'];
    var global_go_term_service = GONavi.Data['go_term_service'];
    var global_amigo_term_details_service =
	GONavi.Data['amigo_term_details_service'];

    // DEBUG.
    core.kvetch("");
    core.kvetch("GONavi.Init: world x: " + global_world['x']);
    core.kvetch("GONavi.Init: world y: " + global_world['y']);
    core.kvetch("GONavi.Init: world lon: " + global_world['lon']);
    core.kvetch("GONavi.Init: world lat: " + global_world['lat']);
    core.kvetch("GONavi.Init: world scale: " + global_world['scale']);
    core.kvetch("GONavi.Init: prev lon: " + global_previous_position['lon']);
    core.kvetch("GONavi.Init: prev lat: " + global_previous_position['lat']);
    core.kvetch("GONavi.Init: prev zoom: " + global_previous_position['zoom']);
    core.kvetch("GONavi.Init: prev focus: "+ global_previous_position['focus']);

    var bounds = new OpenLayers.Bounds();    
    bounds.extend(new OpenLayers.LonLat(global_world.lon,0.0)); // upper-right
    bounds.extend(new OpenLayers.LonLat(0.0,global_world.lat)); // lower-left

    // TODO/BUG: the secret of correct zooming seems to be in these
    // parameters, with a mod to the base zoom-in center point in
    // setCenter.
    // Main ontology layer/resource.
    var img_options = {
	isBaseLayer: true,
	maxResolution: 0.005,
 	//maxResolution: "auto",
 	//minResolution: "auto",
	maxExtent: bounds
	//numZoomLevels: 6
	//units: 'm'
    };
    var size = new OpenLayers.Size(global_world.x, global_world.y);
    var wms = new OpenLayers.Layer.Image('GO Graph',
					 global_maploc,
					 bounds,
					 size,
					 img_options);
    global_map.addLayer(wms);

    // TODO/BUG(?): Without this (and the maxExtent/bounds parameter
    // above), the map gets very confused about zooming and doesn't
    // work properly after an inline refresh until some it has been
    // called one way or another. I'm still not sure if this is a
    // misunderstanding on my part or a bug somewhere else.
    global_map.zoomToMaxExtent();

    // If we have defined the global_previous_position properly, use
    // it to zoom in to our area of interest...
    if( global_previous_position['zoom'] &&
	global_previous_position['lat'] &&
	global_previous_position['lon'] &&
	global_previous_position['focus'] &&
	global_graph_terms[global_previous_position['focus']] ){

	//
	var pan_to_term_id = global_previous_position['focus'];
	var pan_to_term = global_graph_terms[pan_to_term_id];	    
	var pan_to = new OpenLayers.LonLat(pan_to_term.lon,
					   pan_to_term.lat);
	//global_map.panTo(pan_to);
	//global_map.zoomTo(global_previous_position['zoom']);
	global_map.setCenter(pan_to, global_previous_position['zoom']);
    }else{
	global_map.setCenter(new OpenLayers.LonLat(global_world.lon/2.0,
						   global_world.lat/2.0),
			     2);
    }

    global_map.addControl(new OpenLayers.Control.LayerSwitcher());

    ///
    /// Add an overview map. TODO: play with these aux zoom levels some
    /// more.
    ///

//     var bounds_mini = new OpenLayers.Bounds();    
//     bounds_mini.extend(new OpenLayers.LonLat(global_world.lon / 4.0,0.0));
//     bounds_mini.extend(new OpenLayers.LonLat(0.0,global_world.lat / 4.0));
    var size_mini = new OpenLayers.Size(global_world.x / 32,
					global_world.y / 32);
    var img_options_mini = {
	//maxResolution: 0.005,
	//maxExtent: bounds
    };
    var wms_ov = new OpenLayers.Layer.Image('GO Graph',
					    global_maploc_mini,
					    bounds,
					    size_mini,
					    img_options_mini);
    var overview_options = {
	//layers: [wms],
	//size: new OpenLayers.Size(160, 120),
	//numZoomLevels: 6,
	layers: [wms_ov],
	mapOptions: img_options_mini,
	autoPan: true,
	//maxRatio: 32,
	minRatio: 32
    };
    var overview = new OpenLayers.Control.OverviewMap(overview_options);
    global_map.addControl(overview);

    //core.kvetch('_max ratio: ' + overview.maxRatio);
    //core.kvetch('_min ratio: ' + overview.minRatio);

    // Add additional controls.
    //global_map.addControl(new OpenLayers.Control.PanZoomBar());
    //global_map.addControl(new OpenLayers.Control.DragPan());
    global_map.addControl(new OpenLayers.Control.PanZoom());
    global_map.addControl(new OpenLayers.Control.Navigation());
    global_map.addControl(new OpenLayers.Control.KeyboardDefaults());

    // Live coordinate dsiplay no longer needed?
    //     global_map.events.register('click',
    // 			       global_map,
    // 			       function (e) {
    // 				   var x_pos = e.xy.x;
    // 				   var y_pos = e.xy.y;
    // 				   //
    // 				   var pixel =
    // 				       new OpenLayers.Pixel(x_pos, y_pos);
    // 				   var pixll =
    // 				       global_map.getLonLatFromPixel(pixel);
    // 				   //var pixll = global_map.getCenter();
    //			   OpenLayers.Util.getElement('resLoc').innerHTML =
    // 				       "Pixel: (" +x_pos+", "+y_pos+") <br />"+
    // 				       "LL: [" + pixll.lon + ", " + pixll.lat +
    // 				       "]<br />"+"Zoom: "+global_map.getZoom();
    // 			       });

    ///
    /// Take care of resizing the map window at the beginning on future
    /// resizing events.
    ///

    //     core.kvetch("a_" + window);
    //     core.kvetch("b_" + jQuery(window));
    //     core.kvetch("d_" + jQuery(window).innerWidth()); // this one is bad
    var win = jQuery(window);
    function resize_callback(){
	var w_w = win.width() - 100;
	var w_h = win.height() - 100;
	var a_w_w = w_w - 100;
	var a_w_h = w_h - 100;
	core.kvetch('window: width: ' + w_w + ' height: ' + w_h);
	jQuery('#' + 'go_map').css('width', a_w_w + 'px'); 
	jQuery('#' + 'go_map').css('height', a_w_h + 'px'); 
	core.kvetch('window (adjusted): width: ' + a_w_w + ' height: ' + a_w_h);
	global_map.updateSize();
    }
    resize_callback();
    win.resize(resize_callback);


    ///
    /// Map navigation.
    ///

    //
    var navi_callbacks = {};
    function naviHTML(response_obj_acc){

	var response_object = global_go_terms[response_obj_acc];

	// If there are children, make the html area necessary for
	// navigation.
	var minibuf = [];
	var should_do_kids_p = 0;
	for( var kid in response_object.children ){ should_do_kids_p++; break; }
	if( should_do_kids_p ){

	    minibuf.push('<div id="navigation_anchors">');

	    // Make sure that we capture all of the nodes in the
	    // graph.
	    var all_term_ids = new Array();
	    var next_graph_term_ids = new Array();
	    for( var tid in global_graph_terms ){
		all_term_ids.push(tid);
		next_graph_term_ids.push(tid);
	    }

	    // Sort the children alphabetically by name.
	    response_object.children.sort(
		function(a,b){
		    retval = -1;
		    if( a.name.toLowerCase() >= b.name.toLowerCase() ){
			retval = 1;
		    }
		    return retval;
		}
	    );

	    //core.kvetch("-1-: " + response_object + "\n");
	    //core.kvetch("-2-: " + response_object.children + "\n");
	    //core.kvetch("-3-: " + response_object.children.length + "\n");

	    // Generate the necessary html (links, body, and nav
	    // info).
	    minibuf.push(response_object.children.length);
	    if( response_object.children.length == 1 ){
		minibuf.push(' child:<br />');
	    }else{
		minibuf.push(' children:<br />');
	    }

	    var term_string = all_term_ids.join(' ');
	    //for( var kid in response_object.children ){
	    for( var kid = 0; kid < response_object.children.length; kid++ ){

		var kobj = response_object.children[kid];
		all_term_ids.push(kobj.acc);
		
		// Where are we when we navigate?
		var zoom = global_map.getZoom();
		var pixll = global_map.getCenter();

		//core.kvetch("-4-: " + kobj + "\n");
		//core.kvetch("-4-: " + kobj.name + "\n");

		// Generate the callback functions for clicks while
		// we're in here composing the HTML for the navi
		// popup.
		var callback_key =
		    'refresh_on_' + kobj.acc;
		    //'refresh_on';
		function _navi_callback_generate(kacc){
		    return function(){

			// Generate the new JS and image necessary,
			next_graph_term_ids.push(kacc);
			var new_graph_link = 
			    core.api.navi_js_data({
				lon: pixll.lon,
				lat: pixll.lat,
				zoom: zoom,
				focus: kacc,
				terms: next_graph_term_ids
			    });
			core.kvetch('_new navi data link_: ' + new_graph_link);

			// AJAX call with this link, then refresh the
			// map.
			widgets.start_wait('Getting new map...');
			jQuery.ajax({
			    //type: "POST", // it didn't like post...
			    type: "GET",
			    url: new_graph_link, 
			    //dataType: 'json',
			    dataType: 'script',
 			    success: function(data, textStatus){
// 				core.kvetch('_callback executed with data: '
// 					    + data);
				core.kvetch('_callback: refreshing...');
// 				GONavi.Data['openlayers_map'].destroy();
// 				GONavi.Data['openlayers_map'] = null;
				global_map.destroy();
				global_map = null;
				//var foo = GONavi.Init;
				//GONavi = null;
				//GONavi = {};
				//GONavi.Init = foo;
				//GONavi.Replace();
				GONavi.Init(plugins);
				widgets.finish_wait();
			    },
 			    error: function (result, status, error) {
				core.kvetch('Failed server request: '+ status); 
				widgets.finish_wait();
 			    } 
			});  
		    }
		}
		navi_callbacks[callback_key] =
		    _navi_callback_generate(kobj.acc);
		
		//core.kvetch('_anchor id is: ' + callback_key);

 		minibuf.push('<a id="');
 		minibuf.push(callback_key);
		// minibuf.push('" href="#">');
 		minibuf.push('" href="');
		minibuf.push('amigo_exp?mode=layers_graph' +
			     '&lon=' + pixll.lon +
			     '&lat=' + pixll.lat +
			     '&zoom=' + zoom +
			     '&focus=' + kobj.acc +
			     '&terms=' + term_string + ' ' + kobj.acc +
			    "#go_map");
		minibuf.push('">');
		minibuf.push(kobj.name);
		minibuf.push('</a>');
		minibuf.push('<br />');
	    }
	    minibuf.push('</div>');
	}else{
	    //minibuf.push('<p>No children</p>');
	    minibuf.push('No children');
	}

	var popup_html =
	    //'<div style="overflow:auto; overflow-y:scroll;"><p><b>' +
	    //'<div style="overflow:auto"><p><b>' +
	    //'<div style="font-size:75%; overflow:auto;"><p><b>' +
	    '<div style="font-size:75%"><p><b>' +
	    response_object.name +
	    '</b><br /><a href="' +
	    global_amigo_term_details_service +
	    '' +
	    response_obj_acc +
	    '" title="See term details for ' +
	    response_obj_acc +
	    '">'+
	    response_obj_acc +
	    '</a></p><p><div>' +
	    minibuf.join('') +
	    '</p></div>';

	return popup_html;
    }


    //
    function infoHTML(response_object_acc) {

	var nkids = 0;
	if ( global_go_terms[response_object_acc].children ) {
	    nkids = global_go_terms[response_object_acc].children.length;
	}

	var ro = {
	    name: global_go_terms[response_object_acc].name,
	    acc: response_object_acc,
	    nkids: nkids
	}

	var popup_html = '<div style="font-size:75%;"><b>' +
	    ro.name + '</b><br /><small>' +
	    ro.acc + '<br />Children: ' +
	    ro.nkids + '</small></div>';

 	return popup_html;
    }

    ///
    ///
    ///

    //
    var def_style = OpenLayers.Feature.Vector.style['default'];
    var style_blue = OpenLayers.Util.extend({}, def_style);
    //style_blue.strokeColor = "blue"; 
    //style_blue.fillColor = "blue"; 
    //style_blue.fillOpacity = 0.5; 
    //style_blue.strokeOpacity = 0.5; 
    style_blue.fillOpacity = 0.0; 
    style_blue.strokeOpacity = 0.0; 

    var vectorLayer = new OpenLayers.Layer.Vector("[GO World Navigation]");
    vectorLayer.setVisibility(true);

    // Default listeners.
    vectorLayer.events.on({
	'mouseover': onFeatureOver,
	'mouseout': onFeatureOut,
	'click': onFeatureClick
    });
    //var scontrol = new OpenLayers.Control.SelectFeature(vectorLayer);
    //global_map.addControl(scontrol);
    //scontrol.activate();

    var nav_popup = null;
    var info_popup = null;
    var feature2acc = {};

    function onInfoPopupClose(evt) {
	// 'this' is the popup.
	//scontrol.unselect(this.feature);
	// Emptiness.
    }

    function onNaviPopupClose(evt) {

	core.kvetch("GONavi.onNaviPopupClose: closing...");

	// 'this' is the popup.

	// Reenable info (mouseover and mouse off).
	vectorLayer.events.on({
	    'mouseover': onFeatureOver,
	    'mouseout': onFeatureOut
	});

	// Destroy self...TODO/BUG?: don't I need that other stuff too...?
	global_map.removePopup(nav_popup);
	//nav_popup.destroy();

	// 	var feature = vectorLayer.getFeatureFromEvent(evt);
	// 	if( feature ){
	// 	    // Where is the mouse?
	// 	    var mp = evt.object.events.getMousePosition(evt);
	// 	    var pixel = new OpenLayers.Pixel(mp.x, mp.y);
	// 	    // Is it not still over the feature?
	// 	    if( ! feature.geometry.bounds.containsPixel(pixel) ){
	// 		// Then destroy it my pretties!
	// 		navi_popup.feature = null;
	// 		if( feature.popup ){
	// 		    global_map.removePopup(feature.popup);
	// 		    feature.popup.destroy();
	// 		    feature.popup = null;
	// 		}
	// 	    }
	// 	}
    }
    
    //
    function onFeatureClick(evt) {
	//var feature = evt.feature;

	// If we're legit...
	var feature = vectorLayer.getFeatureFromEvent(evt);
	if( feature && feature2acc[feature.id] ){
	    var facc = feature2acc[feature.id];

	    // Quiet the info events.
	    vectorLayer.events.un({
		'mouseover': onFeatureOver,
		'mouseout': onFeatureOut//,
		//'click': onFeatureClick
	    });

	    // Actually create the popup object--coords, etc.
	    var cll = feature.geometry.getBounds().getCenterLonLat();
	    //var lat_boost = - (GONavi.map_halo_size() / 2.0);
	    //var lon_boost = GONavi.map_halo_size() / 2.0;
// 	    var popll =
//  		new OpenLayers.LonLat(cll.lon, //+ lon_boost,
//  				      cll.lat); //+ lat_boost);

	    //alert('clicked on: '+ facc +' at '+ popll.lon +', '+ popll.lat);

	    nav_popup = new OpenLayers.Popup(
		"naviPopup",
		cll,
		new OpenLayers.Size(200,200),
		naviHTML(facc),
		true,
		onNaviPopupClose);
	    feature.popup = nav_popup;
	    nav_popup.feature = feature;

 	    nav_popup.closeOnMove = true;
 	    //nav_popup.panMapIfOutOfView = false;
	    nav_popup.maxSize = new OpenLayers.Size(300,300);
	    nav_popup.minSize = new OpenLayers.Size(250,150);
	    nav_popup.autoSize = true;
	    nav_popup.contentDiv.style.overflow = 'auto';
	    nav_popup.setBorder('solid 1px');
	    nav_popup.setBackgroundColor('#ddddff');
	    // 	global_popups.push(bound_nav_popup);
	    global_map.addPopup(nav_popup, true);

	    // TODO: now that the popup has been added, find it and
	    // attach click events to all of the children navi links.
	    var anchor_kids = jQuery("#navigation_anchors").children();
	    for( var ak = 0; ak < anchor_kids.length; ak++ ){
		var akid = anchor_kids[ak];
		//core.kvetch('_a_: ' + akid.href);
		if( akid.href ){
		    //core.kvetch('_b_: ' + akid.href);
		    jQuery(akid).click(
			function(e){
			    e.preventDefault();
			    core.kvetch('_callback id: ' + this.id);
			    navi_callbacks[this.id].call();
			});
		}
	    }
	}
    }

    function onFeatureOver(evt) {

	//var feature = evt.feature;
	var feature = vectorLayer.getFeatureFromEvent(evt);
	if( feature && feature2acc[feature.id] ){
	    var facc = feature2acc[feature.id];
	    
	    // Make sure that we're clean. This seem to prevent
	    // zombies in some cases.
	    if( info_popup ){
		info_popup.feature = null;
	    }
	    if( feature.popup ){
		global_map.removePopup(feature.popup);
		// feature.popup.destroy(); // TODO: why die 'cause no events?
		feature.popup = null;
	    }

	    // Actually create the popup object--coords, etc.
	    var cll = feature.geometry.getBounds().getCenterLonLat();
	    var lat_boost = - (GONavi.map_halo_size() / 2.0);
	    var lon_boost = GONavi.map_halo_size() / 2.0;
	    var popll =
 		new OpenLayers.LonLat(cll.lon + lon_boost,
 				      cll.lat + lat_boost);
	    info_popup = new OpenLayers.Popup(
		"infoPopup",
		popll,
		new OpenLayers.Size(200,200),
		infoHTML(facc),
		false,
		onInfoPopupClose);
	    feature.popup = info_popup;
	    info_popup.feature = feature;
	    
	    info_popup.setOpacity(0.85);
	    info_popup.maxSize = new OpenLayers.Size(300,300);
	    info_popup.minSize = new OpenLayers.Size(150,50);
	    info_popup.autoSize = true;
	    //info_popup.contentDiv.style.overflow = 'auto';
	    info_popup.setBorder('solid 1px');
	    // info_popup.setBackgroundColor('#ddddff');
 	    // info_popup.setBackgroundColor('#cddcf3');
 	    info_popup.setBackgroundColor('#ddecf3');

	    global_map.addPopup(info_popup);
	}
    }

    //
    function onFeatureOut(evt) {

// 	global_map.removePopup(info_popup);
// 	info_popup.destroy();

	// Get feature and make sure that it is popped.
	var feature = vectorLayer.getFeatureFromEvent(evt);
	if( feature ){
	    
	    // Where is the mouse?
	    var mp = evt.object.events.getMousePosition(evt);
	    var pixel = new OpenLayers.Pixel(mp.x, mp.y);

	    // Is it not still over the feature?
	    if( ! feature.geometry.bounds.containsPixel(pixel) ){
		// Then destroy it my pretties!
		info_popup.feature = null;
		if( feature.popup ){
		    global_map.removePopup(feature.popup);
		    feature.popup.destroy();
		    feature.popup = null;
		}
	    }
	}
    }


    //
    var features = [];
    for( var term_id in global_graph_terms ){

	//
	var term_lon = global_graph_terms[term_id].lon;
	var term_lat = global_graph_terms[term_id].lat;
	
	//var mypopup = null;

	//
	var halo_size = global_world.scale/2.0;
	//var halo_size = global_world.scale/2.0;
	//var halo_size = 0.1;
	var origin = new OpenLayers.Geometry.Point(term_lon, term_lat);
	var polygon =
	    new OpenLayers.Geometry.Polygon.createRegularPolygon(origin,
								 halo_size, 4);

	var f0 = new OpenLayers.Feature.Vector(polygon, null, style_blue);
	features.push(f0);
	feature2acc[f0.id] = term_id;
    }

    //
    vectorLayer.addFeatures(features);
    global_map.addLayer(vectorLayer);

    // Need this for inline refreshes to get correct zoom info(?).
    global_map.updateSize();

    ///
    /// Define API for plugins.
    ///

    //GONavi.term_information = global_go_terms;
    //GONavi.term_position = global_graph_terms;
    GONavi.term_list = function(){
	var ret = new Array();
	for( var term_acc in global_graph_terms ){
	    ret.push(term_acc);
	}
	return ret;
    };
    GONavi.term_data = function(acc){
	return {
	    position: global_graph_terms[acc],
	    information: global_go_terms[acc]
	};
    };
    GONavi.edge_list = function(){
	var ret = new Array();
	for( var edge_acc in global_graph_edges ){
	    ret.push(edge_acc);
	}
	return ret;
    };
    GONavi.edge_data = function(acc){
	return global_graph_edges[acc];
    };
    GONavi.map_halo_size = function(){
	return global_world.scale / 2.0;
    };
    GONavi.register_layer = function(layer){
	global_map.addLayer(layer);
    };
    GONavi.register_control = function(cont){
	global_map.addControl(cont);
    };
    GONavi.register_popup = function(pop){
	global_map.addPopup(pop, true);
    };
    GONavi.unregister_popup = function(pop){
	global_map.removePopup(pop);
    };
    GONavi.current_lonlat = function(){
	return global_map.getCenter();
    };
    GONavi.current_zoom = function(){
	return global_map.getZoom();
    };
    GONavi.previous_focus = function(){
	return global_previous_position['focus'];
    };
    GONavi.lonlat_in_relative_top_p = function(lonlat){
	var mc_ll = global_map.getCenter();
	//var mc_lon = mc_ll.lon;
	var mc_lat = mc_ll.lat;
	var retval = false;
	if( lonlat.lat > mc_lat ){
	    retval = true;
	}
	return retval;
    };
    GONavi.lonlat_in_relative_right_p = function(lonlat){
	var mc_ll = global_map.getCenter();
	var mc_lon = mc_ll.lon;
	//var mc_lat = mc_ll.lat;
	var retval = false;
	if( lonlat.lon > mc_lon ){
	    retval = true;
	}
	return retval;
    };
    GONavi.lonlat_relative_quadrant = function(lonlat){
	var quad = '';
	if( GONavi.lonlat_in_relative_top_p(lonlat) ){
	    quad += 't';
	}else{
	    quad += 'b';
	}
	if( GONavi.lonlat_in_relative_right_p(lonlat) ){
	    quad += 'r';
	}else{
	    quad += 'l';
	}
	return quad;
    };
    GONavi.refresh_map = function(){
	global_map.updateSize();
    };
    GONavi.core = new org.bbop.amigo.core();

    ///
    /// ...
    ///

    // Install/refresh plugins
    if( plugins && plugins.length ){
	for( var pi = 0; pi < plugins.length; pi++ ){
	    plugin = plugins[pi];
	    plugin.call();
	}
    }
};
