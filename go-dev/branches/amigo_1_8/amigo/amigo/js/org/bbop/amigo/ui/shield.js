////////////
////
//// org.bbop.amigo.ui.shield
////
//// Provides an arbitrary shielding popup for the client.
////
//// DEPENDS: org.bbop.amigo
//// DEPENDS: org.bbop.amigo.ui.widgets
////
//// WARNING: Initialization needs to be done after the document is ready.
////
//////////


// Module and namespace checking.
// TODO: Will we need a ui class up there?
if ( typeof org.bbop.amigo.ui == "undefined" ){
    org.bbop.amigo.ui = {};
}
if ( typeof org.bbop.amigo.ui.shield == "undefined" ){
    org.bbop.amigo.ui.shield = {};
}


//
org.bbop.amigo.ui.shield.trigger = function(elt_id, html_string){  

    var widgets = new org.bbop.amigo.ui.widgets();

    // We'll be doing a lot of debugging.
    function ll(str){ core.kvetch(str); }
    ll("");

    ///
    /// Handling.
    ///

    this.bind = function(){

	// TODO: bind actions to things in the action column.
	core.kvetch("adding shield binding to: " + elt_id + "...");

	// Bind to everything that looks like this class.
	var elt = jQuery("#" + elt_id);
	    
	// (Possibly) show menu on click.
	elt.click(function(){

	    var me_id = jQuery(this).attr('id');
	    core.kvetch("_clicked_on_id_: " + me_id);

	    // Create the dialog text.
	    var dialog_id = 'shield_dialog_id_' + core.util.randomness(10);
	    var dialog_text =
		'<div id="' + dialog_id + '">' + html_string + '</div>';
	    jQuery("body").append(jQuery(dialog_text));

	    // Create the dialog.
	    jQuery("#" + dialog_id ).dialog({
		bgiframe: true,
		autoOpen: false,
		height: 300, // TODO/BUG: should be percentage of window
		width: 500, // TODO/BUG: should be percentage of window
		modal: true,
		buttons: {
		    // 
		    'Okay' : $(this).dialog('close')
		},
		close: function() {
		    //allFields.val('').removeClass('ui-state-error');
		}
	    });
	    // Display the dialog.
	    jQuery('#' + dialog_id).dialog('open');
	})
    };
};


//
org.bbop.amigo.ui.shield.set = function(html_string){  

    var widgets = new org.bbop.amigo.ui.widgets();

    // We'll be doing a lot of debugging.
    function ll(str){ core.kvetch(str); }
    ll("");

    ///
    /// Construction.
    ///

    // Create the dialog text.
    var dialog_id = 'shield_dialog_id_' + core.util.randomness(10);
    var dialog_text =
	'<div id="' + dialog_id + '">' + html_string + '</div>';
    jQuery("body").append(jQuery(dialog_text));

    // Create the dialog.
    jQuery("#" + dialog_id ).dialog({
	bgiframe: true,
	autoOpen: false,
	height: 500, // TODO/BUG: should be percentage of window
	width: 700, // TODO/BUG: should be percentage of window
	modal: true,
	buttons: {
	    // 
	    'Okay': function(){
		jQuery(this).dialog('close');
	    }
	},
	close: function() {
	    //allFields.val('').removeClass('ui-state-error');
	}
    });
    // Display the dialog.
    jQuery('#' + dialog_id).dialog('open');

    // Enable external closing of the shield.
    this.close = function(){
	jQuery('#' + dialog_id).dialog('close');
    };
};
