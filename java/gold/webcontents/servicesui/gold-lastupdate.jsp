<%@page import="java.util.Hashtable"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Gold DB Last Update Date</title>
</head>
<body>
<%
	//String db = (String)request.getAttribute("dbname");
	Hashtable<String, String> dbs = (Hashtable)request.getAttribute("dbs");
%>


<h1>Database Updates status </h1>

<%
	for(String ontology: dbs.keySet()){
		String data = dbs.get(ontology);
		%>
		
		<hr />

		<h4>Document Id : <%= ontology %></h4>
		Last Update Date : <%= data  %> <br />
		
		
		<%
	}
%>

</body>
</html>