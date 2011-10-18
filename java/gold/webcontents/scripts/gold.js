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
			printVoilations(voilations);
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
