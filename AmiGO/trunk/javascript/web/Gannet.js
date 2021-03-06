////
//// For now, a simple hook into Gannet once live. Just three lines, so
//// will probably leave DEBUG in.
////


//
function GannetInit(){

    // Per-manager logger.
    var logger = new bbop.logger();
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    //ll('');
    ll('GannetInit start...');

    // GOlr: Enter things from pulldown into textarea on change.
    jQuery("#" + "gannet_golr_example_selection").change(
	function(){
	    var semi_solr = jQuery(this).val();
	    //ll('semi_solr: ' + semi_solr);
	    jQuery("#" + "query").val(semi_solr);
	});

    // Make unnecessary things roll up.
    //bbop.core.each(["information", "mirrors", "solr_options"],
    bbop.core.each(["information", "mirrors"],
		   function(eltid){
		       jQuery('#'+eltid).hide();
		       var elt = jQuery('#' + eltid + '_click');
		       elt.click(function(){
				     jQuery('#'+eltid).toggle("blind",{},250);
				     return false;
				 });
		   });
    
    // TODO: scan and add things to the page.
    // Check to see if a results-only id shows up.
    var results_ping = jQuery("#" + "results_generated");
    if( results_ping && results_ping.attr('id') ){
	ll('Looks like a results page.');
    }else{
	ll('Looks like a starting page.');
    }

    ll('GannetInit done.');
}
