////
//// ...
////


// Bring in the AmiGO core and keep a coder handy.
// TODO/BUG: switch DEBUG to false for release.
org.bbop.amigo.DEBUG = true;
var core = new org.bbop.amigo.core();
var gm = new org.bbop.amigo.go_meta();


var Manager = null;

//
function LiveSearchASInit(){

    core.kvetch('');
    core.kvetch('LiveSearchASInit start.');

    mods();

    // Ready the Manager.
    var surl = 'http://accordion.lbl.gov:8080/solr/';
    Manager = new AjaxSolr.Manager({solrUrl: surl}); 
    Manager.init();
    Manager.store.addByValue('q', '*:*');

    // Facets ready.
    var facet_params = {
	facet: true,
	//'facet.field': ['topics', 'organisations', 'exchanges'],
	'facet.field': ['document_category'],
	'facet.limit': 20,
	'facet.mincount': 1,
	'f.topics.facet.limit': 50,
	'json.nl': 'map'
    };
    for (var facet_name in facet_params) {
	Manager.store.addByValue(facet_name, facet_params[facet_name]);
    }

    // TODO: move off.
    var tc_desc = {
	
    };
    AjaxSolr.TagcloudWidget = AjaxSolr.AbstractFacetWidget.extend(tc_desc);

    //var fields = [ 'topics', 'organisations', 'exchanges' ];
    var tag_facet_fields = ['document_category'];
    for( var i = 0, l = tag_facet_fields.length; i < l; i++ ){
	var tag_inst = {
	    id: tag_facet_fields[i],
	    target: '#tag_facet_' + tag_facet_fields[i],
	    field: tag_facet_fields[i],
	    afterRequest: function () {
		if( this.manager.response.facet_counts.facet_fields[this.field]
		    === undefined ){
			$(this.target).html(AjaxSolr.theme('no_items_found'));
			return;
		    }

		var maxCount = 0;
		var objectedItems = [];
		for (var facet in this.manager.response.facet_counts.facet_fields[this.field]) {
		    var count = parseInt(this.manager.response.facet_counts.facet_fields[this.field][facet]);
		    if (count > maxCount) {
			maxCount = count;
		    }
		    objectedItems.push({ facet: facet, count: count });
		}
		objectedItems.sort(function (a, b) {
				       return a.facet < b.facet ? -1 : 1;
				   });
		
		var self = this;
		$(this.target).empty();
		for (var i = 0, l = objectedItems.length; i < l; i++) {
		    var curr_facet = objectedItems[i].facet;
		    $(this.target).append(AjaxSolr.theme('tag', curr_facet, parseInt(objectedItems[i].count / maxCount * 10), self.clickHandler(curr_facet)));
		}
	    }
	};
	Manager.addWidget(new AjaxSolr.TagcloudWidget(tag_inst));
    }

    // TODO: move off.
    AjaxSolr.ResultWidget = AjaxSolr.AbstractWidget.extend({});
    var res_desc = {
	id: 'results',
	target: '#results_div',

	// TODO: Spinner.
	beforeRequest: function () {
	    //$(this.target).html($('<img/>').attr('src', 'ajax-loader.gif'));
	    $(this.target).html('Loading...');
	},

	// How and what to display.
	afterRequest: function () {
	    $(this.target).empty();
	    var res_len = this.manager.response.response.docs.length;
	    for( var i = 0, l = res_len; i < l; i++ ){
		
		// Main doc display.
		var doc = this.manager.response.response.docs[i];
		var d_res = AjaxSolr.theme('result', doc);
		$(this.target).append(d_res);
		
		// ...?
		var items = [];
		// TODO/BUG: same ickiness I was dealing with...
		var pass_dc = doc.document_category; 
		if( typeof doc.document_category == 'string' ){
		    pass_dc = [doc.document_category];
		}
		items = 
		    items.concat(this.facetLinks('document_category', pass_dc));
		core.kvetch('da items: ' + items[0]);

		//items.concat(this.facetLinks(doc.isa_partof_label_closure));
		//items.concat(this.facetLinks(doc.organisations));
		//items.concat(this.facetLinks(doc.exchanges));
		AjaxSolr.theme('list_items',
			       // TODO/BUG: argh!
			       '#links_' + doc.id.replace(/(:|\.)/g,'\\$1'),
			       items, '');
	    }
	},

	// ...?
	facetLinks: function(facet_field, facet_values){
	    var links = [];
	    if( facet_values ){
		for( var i = 0, l = facet_values.length; i < l; i++ ){
		    var f_handler = this.facetHandler(facet_field,
						      facet_values[i]);
		    var f_link = AjaxSolr.theme('facet_link',
						facet_values[i],
						f_handler);
		    core.kvetch('add f_link: ' + f_link);
		    links.push(f_link);
		}
	    }
	    return links;
	},
	
	// ...?
	facetHandler: function(facet_field, facet_value){
	    var self = this;
	    return function (){
		self.manager.store.remove('fq');
		self.manager.store.addByValue('fq',facet_field+':'+facet_value);
		self.manager.doRequest(0);
		return false;
	    };
	}
    };
    Manager.addWidget(new AjaxSolr.ResultWidget(res_desc));


    var pager_desc = {
	id: 'pager',
	target: '#pager',
	prevLabel: '&lt;',
	nextLabel: '&gt;',
	innerWindow: 1,
	renderHeader: function (perPage, offset, total) {
	    $('#pager-header').html($('<span/>').text('displaying ' + Math.min(total,offset + 1) + ' to ' + Math.min(total, offset + perPage) + ' of ' + total));
	}
    };
    Manager.addWidget(new AjaxSolr.PagerWidget(pager_desc));
			  
    // First ping.
    Manager.doRequest();

    core.kvetch('LiveSearchASInit end.');
}


//
function _annotation_line(doc){

    var mbuf = [];
	
    mbuf.push('<div>');
    mbuf.push('<h3>');
    mbuf.push(doc.id);
    mbuf.push('</h3>');
    mbuf.push('<p id="links_');
    mbuf.push(doc.id);
    mbuf.push('">');
    mbuf.push('...</p>');
    mbuf.push('<p>');
    mbuf.push('annotation info');
    mbuf.push('</p>');
    mbuf.push('</div>');
    
    return mbuf.join('');
}


// TODO: model after the above
function _term_line(doc){

    var mbuf = [];
	
    mbuf.push('<div>');
    mbuf.push('<h3>');
    mbuf.push(doc.id);
    mbuf.push('</h3>');
    mbuf.push('<p id="links_');
    mbuf.push(doc.id);
    mbuf.push('">');
    mbuf.push('...</p>');
    mbuf.push('<p>');
    mbuf.push('term info');
    mbuf.push('</p>');
    mbuf.push('</div>');
    
    return mbuf.join('');
}


//
function mods(){
    
    core.kvetch('Adding mods.');

    AjaxSolr.theme.prototype.result = function (doc) {

	var mbuf = [];

	// TODO: document type identification.
	if( doc && doc.document_category ){
	 if( 'annotation' == doc.document_category ){
	     mbuf.push(_annotation_line(doc));
	 }else if( 'ontology_class' == doc.document_category ){
	     mbuf.push(_term_line(doc));
	 // }else if( 'bioentity' == doc.document_category ){
	 //     mbuf.push(_bioentity_line(doc));
	 }else{
	     mbuf.push('Unknown document category: ' + doc.document_category );
	 }
	}else{
	    mbuf.push('WTFBBQ');
	}

	return mbuf.join('');
    };


    AjaxSolr.theme.prototype.tag = function (value, weight, handler) {
	return $('<a href="#" class="tagcloud_item"/>').text(value).addClass('tagcloud_size_' + weight).click(handler);
    };
    
    AjaxSolr.theme.prototype.facet_link = function (value, handler) {
	return $('<a href="#"/>').text(value).click(handler);
    };
    
    AjaxSolr.theme.prototype.no_items_found = function () {
	return 'no items found in current selection';
    };
    
}

