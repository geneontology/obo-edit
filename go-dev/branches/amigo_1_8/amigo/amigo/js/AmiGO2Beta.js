////
//// Initialize the hidden AmiGO 2 forwarding panels if the
//// environment seems right.
////

function AmiGO2BetaInit(){
    
    var tid = '#amigo2-beta-panel';
    
    // Only activate if out environment is right--check for jQuery and
    // the div that will only appear on EOL && AMIGO2_LINK.
    if( jQuery && jQuery(tid) && jQuery(tid).length && jQuery(tid).length > 0 ){
	
	// Remove the class hiding the forwarding panel.
	jQuery('#amigo2-beta-panel').removeClass('hidden');

	// Set to slide out on click.
	var forwarding_tab = {
            speed: 300,
            containerWidth: jQuery('.amigo2-beta-panel').outerWidth(),
            containerHeight: jQuery('.amigo2-beta-panel').outerHeight(),
            tabWidth: jQuery('.amigo2-beta-tab').outerWidth(),
            init: function(){
		jQuery('.amigo2-beta-panel').css(
		    'height',
		    forwarding_tab.containerHeight + 'px');
		jQuery('a.amigo2-beta-tab').click(
		    function(event){
			if( jQuery('.amigo2-beta-panel').hasClass('tab-open') ){
			    // If it's already open...
			    jQuery('.amigo2-beta-panel').animate(
				// {left:'-' + forwarding_tab.containerWidth},
				{right: '-' + forwarding_tab.containerWidth},
				forwarding_tab.speed).removeClass('tab-open');
			    //jQuery('#amigo2-beta-panel-inner').addClass('hidden');
			}else{
			    // If it's closed.
			    //jQuery('#amigo2-beta-panel-inner').removeClass('hidden');
			    jQuery('.amigo2-beta-panel').animate(
				// {left: '0'},
				{right: '0'},
				forwarding_tab.speed).addClass('tab-open');
			}
			event.preventDefault();
		    });
            }
	};
 
	forwarding_tab.init();
    }
}
