////
//// For now, a simple hook into GOOSE once live. Just three lines, so
//// will probably leave DEBUG in.
////


//
function GOOSEInit(){

    // Per-manager logger.
    var logger = new bbop.logger();
    logger.DEBUG = true;
    function ll(str){ logger.kvetch(str); }

    //ll('');
    ll('GOOSEInit start...');

    // Enter things from pulldown into textarea on change.
    jQuery("#" + "goose_example_selection").change(
	function(){
	    var sql = jQuery(this).val();
	    jQuery("#" + "query").val(sql);
	});

    // Check to see if a results-only id shows up.
    var results_ping = jQuery("#" + "results_generated");
    if( results_ping && results_ping.attr('id') ){
	ll('Looks like a results page.');
	// TODO: scan and add things to the page.
    }else{
	ll('Looks like a starting page.');
    }

    ll('GOOSEInit done.');
}
