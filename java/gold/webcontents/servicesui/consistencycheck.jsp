<%@page import="owltools.graph.OWLGraphWrapper"%>
<%@page import="java.util.List"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>Reasoning Service</title>
</head>

<body>

	<h1>Consistency Checks</h1>
	

	<%
		boolean  isTaskRunning = (Boolean)request.getAttribute("isTaskRunning");
	
		if(isTaskRunning){
	%>

		<script type='text/javascript'>
			setTimeout("location.reload(true)", 9000);
		</script>
	
		<center><img src="/images/progress-indicator.gif" alt="Request is in Progress" /></center>
		<p align="center">Your Request is in Progress</p>
	
	<%
		}
	%>


	<ul>
		<%
		OWLGraphWrapper graph = (OWLGraphWrapper)request.getAttribute("graph");
		List<String> errors = (List<String>)request.getAttribute("errors");
		int totalErrors = 0;
		if(errors != null){
			totalErrors = errors.size();
			if(errors.isEmpty()){
			%>
				<li>No Error is found during the consistency check</li>
			<%
			}else{
				for(String error: errors){
			%>
				<li><%= error%></li>
			<%
				}
			}
		}
		%>
	</ul>
	
	
	<%
	String taskCompletionTime = (String)request.getAttribute("taskCompletionTime");
	%>
	<hr />
	<h4>The inference computation is completed in <%= taskCompletionTime %> seconds</h4>	
	<h4>Total Inferences found are: <%= totalErrors %></h4>	
	<h4>Ontology is : <%= graph.getOntologyId() %></h4>	
	
</body>


</html>