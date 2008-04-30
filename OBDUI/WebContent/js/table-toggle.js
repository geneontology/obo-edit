function toggleTable(div_id,hostname){

	plus = new Image();
	plus.src = "http://" + hostname + "/OBDUI/images/plus-box.gif";
	minus = new Image();
	minus.src = "http://" + hostname + "/OBDUI/images/min-box.gif";
	
	var detailsDiv = document.getElementById(div_id);
	var toggleImage = document.getElementById((div_id+'_image'));
	
	if (detailsDiv && toggleImage){
					
		if (detailsDiv.style.display == "none"){
			detailsDiv.style.display='table-cell';
			toggleImage.src=minus.src;
		} else {
			detailsDiv.style.display="none";
			toggleImage.src=plus.src;
		}			
	}
}