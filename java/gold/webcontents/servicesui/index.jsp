<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<%@page import="org.geneontology.conf.GoConfigManager"%>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Gene Ontology Web Admin</title>
<script type="text/javascript">
function loadOntology(){
	var yes = confirm("Are you are sure you want to load ontology into database?");

	if(yes){
		window.location="/gold?servicename=gold-db-operations&command=update";
	}
}

function loadTaxonomies(){
	var yes = confirm("Are you are sure you want to load taxonomies into database?");

	if(yes){
		//window.location="gold?servicename=gold-db-operations&command=update";
		document.getElementById("loadtax").submit();
	}
}


function loadGaf(){
	var yes = confirm("Are you are sure you want to load GAF into database?");

	window.location="/gold?servicename=gaf-db-operations&command=update";
}

function getLastUpdateDate(){
	window.location="/gold?servicename=gold-db-operations&command=getlastupdate";
}

function getLastUpdateDateOfGAF(){
	window.location="/gold?servicename=gaf-db-operations&command=getlastupdate";
}


</script>
</head>
<body>

<form id="loadtax" action="/gold">
	<input type="hidden" name="servicename" value="gold-db-operations" />
	<input type="hidden" name="command" value="update" />
	<input type="hidden" name="taxload" value="" />
	
	<%
		for(Object location: GoConfigManager.getInstance().getTaxonomiesLocations()){
	%>
	
			<input type="hidden" name="filelocation" value="<%= location%>" />
	<%
	
		}
	%>

</form>

<h1>Welcome to the Admin site of the Gene Ontology project</h1>

<p>Activities:</p>
<ul>
<li><a href="javascript:loadOntology();">Load ontology into database</a></li>
<li><a href="javascript:loadTaxonomies();">Load Taxonomies into database</a></li>
<li><a href="/gold/phylotools.jsp">Load Phylo trees into database</a></li>
<li><a href="javascript:getLastUpdateDate()">Last Update of Ontololgy database</a></li>
<li><a href="javascript:getLastUpdateDateOfGAF()">Last Update of GAF database</a></li>
<li><a href="/gold?servicename=reasoning-service&command=checkconsistency">Perform Ontology Consistency Check</a></li>
<li><a href="/gold?servicename=reasoning-service&command=find-inferences">Find Ontology Inferences</a></li>
<li><a href="/gold/gaftools.html">Gene Annotation Files Tools</a></li>
</ul>
</body>
</html>