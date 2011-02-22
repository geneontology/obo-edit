////
//// 
////


// Bring in the AmiGO core.
org.bbop.amigo.DEBUG = true;
var core = new org.bbop.amigo.core();
//var widgets = null;

//
var global_shop = null;
var global_wait = null;

//
function ScratchClientInit(){

    //
    global_shop = new org.bbop.amigo.ui.shopping();
    global_wait = new org.bbop.amigo.ui.wait("<h4>Your job has been submitted.</h4><p>Your job has been submitted, but it may take some time to get a response. When results are available, you will be taken to a results page. Resubmitting the job will not make things go faster, and in fact may slow things down. Relax and enjoy the your browser\'s spinner. We\'ll install blinken lights in a future version.<\p>");

    // Bind to two examples.
    var nums = [1, 2, 3];
    //jQuery('#foo123').click(function(){alert('foo123');});
    //jQuery('#bar123').click(global_shop.generate_hook('foo_key', 'FOO_NAME'));
    for( var f = 0; f < nums.length; f++ ){
	var n = nums[f];
	core.kvetch('#foo' + n);
	jQuery('#foo' + n).click(global_shop.generate_hook('key_' + n,
							   'name' + n));
    }

    //
    jQuery('#timer-form').click(global_wait.generate_hook());
}
