<%@page import="java.io.PrintWriter"%>
<%@page import="org.geneontology.web.Task"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>Reasoning Service</title>
</head>

<body>

	<h1>Loading Ontology into Solr</h1>
	

	<%
		Task task = (Task)request.getAttribute("task");
		boolean  isTaskRunning = (Boolean)request.getAttribute("isTaskRunning");
	
		Throwable ex = null;
		
		if(task != null){
			ex = task.getException();
		}
		
		
		if(isTaskRunning){
			String id = request.getParameter("id");
			HttpSession s = request.getSession(true);
			if(id == null){
				id = s.getId();
			}
	%>
			<center><img src="/images/progress-indicator.gif" alt="Request is in Progress" /></center>
			<p align="center">Your Request is in Progress</p>

			<form id="reloadform" action="/gold">
				<%
				for(Object param: request.getParameterMap().keySet()){
					%>
					<input type="hidden" name="<%= param %>" value="<%= request.getParameter(param.toString()) %>" />
					<%
				}
				
				%>
				<input type="hidden" name="id" value="<%= id %>" />	
							
			</form>

		<script type='text/javascript'>
			setTimeout('submitForm()', 9000);
			
			function submitForm(){
				document.forms[0].submit();
			}
			
		</script>
	
	
	<%
		}
	%>



	<%
	String taskCompletionTime = (String)request.getAttribute("taskCompletionTime");
	%>
	<hr />
	<h4>The operation is completed in <%= taskCompletionTime %> seconds</h4>	

	<%
	if(ex != null){
		PrintWriter pw = new PrintWriter(out);
		ex.printStackTrace(pw);
		pw.flush();
	}
	
	
	%>
</body>


</html>