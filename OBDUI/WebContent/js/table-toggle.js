function toggleTable(div_id){

	var detailsDiv = document.getElementById(div_id);
	if (detailsDiv){			
		if (detailsDiv.style.display == "none"){
			detailsDiv.style.display='table-row';
		} else {
			detailsDiv.style.display="none";
		}			
	}
}