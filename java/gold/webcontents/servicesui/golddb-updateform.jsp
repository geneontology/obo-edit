<%@page import="org.geneontology.conf.GeneOntologyManager"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Insert title here</title>
</head>
<body>


		<h1>Gene Ontology Admin</h1>
		<form action='.'>
		<input type='hidden' name='servicename' value='<%= request.getAttribute("servicename") %>' />
		<input type='hidden' name='command' value='update' />
		<label>Please select ontology</label><br />
		
		<% 
		for (Object obj : GeneOntologyManager.getInstance()
				.getDefaultOntologyLocations()) {
			%>
			
			<input type='radio' name='ontologylocation' value='<%= obj %>' /> <%= obj %> <br />
		<%
		}
		%>
		<input type='submit' value='update' /><br />
		</form>


</body>
</html>