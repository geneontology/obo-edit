<%@page import="java.io.PrintWriter"%>
<%@page import="org.geneontology.web.Task"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>Initialization Service</title>
</head>

<body>

	<h1>Initializing database....</h1>
	

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

	
		String status = (String)request.getAttribute("status");
		status = status == null ? "" : status;
		
		Throwable ex = null;
		Task task = (Task)request.getAttribute("task");

		if(task != null){
			ex = task.getException();
		}
		
		if(ex != null){
			PrintWriter writer = new PrintWriter(out);
			ex.printStackTrace(writer);
		}
		
	%>

	
</body>


</html>