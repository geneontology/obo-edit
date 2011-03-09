<%@page import="org.obolibrary.obo2owl.Owl2Obo"%>
<%@page import="owltools.graph.OWLGraphWrapper"%>
<%@page import="java.util.List"%>
<%@page import="java.util.Enumeration"%>
<%@page import="owltools.graph.OWLGraphEdge"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>Reasoning Service</title>
</head>

<body>

	<h1>Inferences Computation Results</h1>
	

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


	<table>
		<tr>
		<td>Axiom Type</td><td>Class 1</td><td>Class 2</td>
		</tr>
		<%
		
			OWLGraphWrapper graph = (OWLGraphWrapper)request.getAttribute("graph");
			for(OWLGraphEdge edge: (List<OWLGraphEdge>)request.getAttribute("edges")){
				
				String label1 = Owl2Obo.getIdentifier(edge.getSource()) +"[" + graph.getLabel(edge.getSource()) +"]";
				String label2 = Owl2Obo.getIdentifier(edge.getTarget()) +"["+ graph.getLabel(edge.getTarget()) + "]"; 
				
				String axiomType = edge.getSingleQuantifiedProperty().getQuantifier().toString();
		%>
				
				<tr>
				<td><%= axiomType %></td><td><%= label1 %></td><td><%= label2 %></td>
				</tr>
		<%		
				
			}
			
		%>
		
		
	</table>

	<%
	String taskCompletionTime = (String)request.getAttribute("taskCompletionTime");
	%>
	<hr />
	<h4>The inference computation is completed in <%= taskCompletionTime %> seconds</h4>	

	
</body>
</html>