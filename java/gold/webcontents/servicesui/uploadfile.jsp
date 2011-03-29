<%@page import="java.util.Hashtable"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Redirecting upload file request</title>

<script type="text/javascript" src="/scripts/jquery-1.5.1.min.js"></script>

<script type="text/javascript">

	$(document).ready(function() {
		$("#redirectupload").submit();
	});

</script>

</head>
<body>
<h4>Re-directing upload file request. Please wait.....</h4>

<%
	Hashtable<String, String> parameters =(Hashtable<String, String>) request.getAttribute("parameters");
	if(parameters != null){
%>

	<form id="redirectupload" action=".">
	
		<%
		for(String key: parameters.keySet()){
			%>
			<input type="hidden" name="<%= key %>" value = "<%= parameters.get(key) %>" />
			<%
		}
		%>
		
	</form>

<%
	}else{
		
	}
%>



</body>
</html>