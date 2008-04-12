function nodeSearch(contextName){

    var dataSource = document.getElementById("dataSource");
    var searchTerm = document.getElementById("search_term");
    
    if ((searchTerm != null)&&(dataSource != null)){
    	var url = "/" + contextName + "/" + dataSource.value + "/html/search/contains_all/" + searchTerm.value;    	
    	window.location = url;
    }
    return false;
    
}