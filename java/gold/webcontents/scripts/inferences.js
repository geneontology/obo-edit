$(document).ready(function() {
	$(".explanation").hide();
	
	$(".hide-button").click(function(){
		$(this).parents(".explanation").hide("show");
	});
 });

function getExplanation(cls1, cls2, axiom, id){

	$.get('/gold/?servicename=reasoning-service&cls1='+cls1 + "&cls2="+cls2 + "&quantifier="+axiom, function(data) {

		$("#"+id).html(data)
		.parents(".explanation").show("show");
		
		});
	
	/*$("#"+id).html('Hello World')
		.parents(".explanation").show("show");*/
}

function hideExplanation(anchor){
	alert($(anchor).html());
}