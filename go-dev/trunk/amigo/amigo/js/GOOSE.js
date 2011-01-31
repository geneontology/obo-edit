////
//// For now, a simple hook into GOOSE once live. Just three lines, so
//// will probably leave DEBUG in.
////


// Bring in the AmiGO core.
org.bbop.amigo.DEBUG = true;
var core = new org.bbop.amigo.core();

//
function GOOSEInit(){

    //core.kvetch('');
    core.kvetch('GOOSEInit start...');

    // Enter things from pulldown into textarea on change.
    jQuery("#" + "goose_example_selection").change(
	function(){
	    var sql = jQuery(this).val();
	    jQuery("#" + "sql").val(sql);
	});

    // Check to see if a results-only id shows up.
    var results_ping = jQuery("#" + "results_generated");
    if( results_ping && results_ping.attr('id') ){
	core.kvetch('Looks like a results page.');
	// TODO: scan and add things to the page.
    }else{
	core.kvetch('Looks like a starting page.');
    }

    core.kvetch('GOOSEInit done.');
}
