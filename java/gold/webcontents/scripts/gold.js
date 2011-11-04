function printVoilations(voilations){

	var voilationsDom = jQuery(".voilations");		
	var cache = new Array();
	jQuery.each(voilations, function(index, voilation){
		var element = cache[voilation.fileId];
		
		if(element == null){
			element= jQuery("#"+voilation.fileId);
			if(element.length >0){
			 	cache[voilation.fileId]=element;			
			}
		}
		if(element.length==0){
			voilationsDom.append("<br /><br /><hr /><div id='"+voilation.fileId+"' style='font-weight:bold;'>"+voilation.fileName+"</div><br />");
			element= jQuery("#"+voilation.fileId);
			//printVoilations(voilations);
		}
		
		var html = "<div class='voilation-body'>"
			+ "<div class='rule-id'>Rule Voilation: "+voilation.ruleId+": </div>"
			+ "<div class='line-number' style='margin-left:15px;'>Voilation at line: "+voilation.lineNumber+"</div>"
			+ "<div class='message' style='color:red;margin-left:15px'>Message: "+voilation.message+"</div>"
			+ "<div class='source' style='margin-left:15px;'>Annotation Row: "+voilation.annotation+"</div>"
			+ "</div><br />";
			
		
		element.append(html);
		
	});
	
}


function printAnnotationInferences(predictions){

	var voilationsDom = jQuery(".voilations");		
	var cache = new Array();
	jQuery.each(predictions, function(index, prediction){
		var element = cache[prediction.fileId];
		
		if(element == null){
			element= jQuery("#"+prediction.fileId);
			if(element.length >0){
			 	cache[prediction.fileId]=element;			
			}
		}
		if(element.length==0){
			voilationsDom.append("<br /><br /><hr /><div id='"+prediction.fileId+"' style='font-weight:bold;'>"+prediction.fileName+"</div><br />");
			element= jQuery("#"+prediction.fileId);
		}
		
		var html = "<div class='prediction-body'>"
			+ "<div class='annotation' style='margin-left:15px'>Message: "+prediction.annotation+"</div>"
			+ "<div class='annotation' style='margin-left:15px'>isRedundantWithExistingAnnotations: "+prediction.isRedundantWithExistingAnnotations+"</div>"
			+ "<div class='annotation' style='margin-left:15px'>isRedundantWithOtherPredictions: "+prediction.isRedundantWithOtherPredictions+"</div>"
			+ "</div><br />";
			
		
		element.append(html);
		
	});
	
	
}

