function nodeSearch(contextName,dataSource){

    var searchTerm = document.getElementById("search_term");
    
    if ((searchTerm != null)&&(dataSource != null)){
    	var url = "/" + contextName + "/" + dataSource + "/html/search/contains_all/" + searchTerm.value;    	
    	window.location = url;
    }
    return false;
    
}