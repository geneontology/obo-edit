////
//// See if we can get a more sensible JS-based ontology browser
//// working.
////

function BrowseInit(){
    
    ///
    /// General setup--resource locations.
    /// Solr server, GOlr config, etc.
    ///

    var sd = new amigo.data.server();
    var gconf = new bbop.golr.conf(amigo.data.golr);

    ///
    /// The info shield.
    ///

    var linker = new amigo.linker();
    var shield = new bbop.widget.term_shield(sd.golr_base(), gconf,
					     {'linker_function': linker });
    shield.set_personality('bbop_ont');

    ///
    /// The tree browser.
    ///

    // Setup the widget with the server info.
    // Launch at a root with a boring callback.
    var b_widget = bbop.widget.browse;
    var b =
	new b_widget(sd.golr_base(), 
		     gconf, 
		     'browser_id',
		     {
			 'base_icon_url': sd.image_base(),
			 'info_icon': 'info',
			 'current_icon': 'current_term',
			 'image_type': 'gif',
			 'info_button_callback':
			 function(term_acc, term_doc){
			     // // Local form.
			     // shield.draw(term_doc);
			     // Remote form (works).
			     shield.draw(term_acc);
			 }
		     });
    b.draw_browser('GO:0008150');

    ///
    /// Ontology selector.
    ///

    jQuery("#graph_radio").buttonset();
    var loop = bbop.core.each;
    loop(['bp', 'cc', 'mf'],
	 function(ont){
	     jQuery('#'+ont).click(function(){
				       var o = jQuery(this).attr('id');
				       if( o == 'bp' ){
					   b.draw_browser('GO:0008150');
				       }else if( o == 'cc' ){
					   b.draw_browser('GO:0005575');
				       }else{
					   b.draw_browser('GO:0003674');
				       }
				   });
	 });

    ///
    /// The autocomplete talking back to the tree browser.
    ///

    jQuery('#' + 'jumper').click(function(){ jQuery(this).val(''); }); // clear
    function jumper(doc){ b.draw_browser(doc['id']); }
    var a_widget = bbop.widget.search_box;
    var auto = new a_widget(sd.golr_base(), gconf, 'jumper',
			    {
				'label_template':
				'{{annotation_class_label}} ({{id}})',
				'value_template': '{{annotation_class}}',
				'list_select_callback': jumper
			    });
    auto.set_personality('bbop_ont'); // profile in gconf
    auto.add_query_filter('document_category', 'ontology_class');
}
