<%@page import="java.io.PrintWriter"%>
<%@page import="org.geneontology.gold.rules.AnnotationRuleViolation"%>
<%@page import="java.util.Set"%>
<%@page import="org.geneontology.web.Task"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>Phylo Tree Load Status</title>

</head>
<body>

	<%
		boolean  isTaskRunning = (Boolean)request.getAttribute("isTaskRunning");
	
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

	<h1>Phylo Tree Load Status</h1>

	<%
		Task task = (Task)request.getAttribute("task");
	%>

	<table><tr><th>Operation Name</th><th>Status/Completion Time</th></tr>
	<% 		
		Throwable ex = null;
		if(task != null){
			ex = task.getException();
			String ontology = "";
			for (String opName : task.getCompletedOperations()) {
	
				long stTime = task.getStartTime(opName);
				long endTime = task.getEndTime(opName);
	
				String status = "In progress";
				boolean isCompleted = false;
				if (endTime > 0) {
					status = (float) (endTime - stTime) / 1000 + "";
					isCompleted = true;
				} else if (ex != null) {
					status = "failed";
				}
			%>
				<tr><td> <%= opName %>  </td><td bgcolor='<%= isCompleted ? "green" : "" %>') > <%= status %> </td></tr>
			<% 			
			}
	
		}
		%>	
	</table>

	<%
	if(ex != null){
		PrintWriter pw = new PrintWriter(out);
		ex.printStackTrace(pw);
		pw.flush();
	}
	
	%>
</body>
</html>