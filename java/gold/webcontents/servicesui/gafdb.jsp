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
	<title>Gold Db Operation Status</title>

</head>
<body>

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

	<h1>Status of the execution of the <%= request.getParameter("command") %>' command on the GAF database.</h1>

	<%
		Task task = (Task)request.getAttribute("task");
	%>

	<table><tr><th>Operation Name</th><th>Status/Completion Time</th></tr>
	<% 		
		Exception ex = null;
		if(task != null){
			ex = task.getException();
			String ontology = "";
			for (String opName : task.getCompletedOperations()) {
				String[] st = opName.split("--");
				String opLocalName = opName;
				if (st.length > 1) {
					if (!ontology.equals(st[1])) {
						ontology = st[1];
						%>
						<tr><td colspan='2'><h4>Loading "  <%= ontology %> "</h4></td></tr>
						<%
						
					}
	
					opLocalName = st[0];
				}
	
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
				<tr><td> <%= opLocalName %>  </td><td bgcolor='<%= isCompleted ? "green" : "" %>') > <%= status %> </td></tr>
			<% 			
			}
	
		}
		
		if(ex == null){
			ex = (Exception)request.getAttribute("exception");
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

	<%
	if(request.getParameter("commit") == null){
	
		Set<AnnotationRuleViolation> annotationRuleViolations = (Set<AnnotationRuleViolation>)request.getAttribute("violations");
		if(annotationRuleViolations != null){
		%>
	
			<hr />
			<h3>Annotation Violations</h3>
			<ul>
			<%
			
			for(AnnotationRuleViolation v: annotationRuleViolations){
				%>
				<li> <%= v.getMessage() %> -- <%= v.getSourceAnnotation() %> </li>
				<%			
			}
			
			%>
	
			</ul>
		
		<%
		}
	
		if(!isTaskRunning){
		
			%>
			<hr />		
			<form action=".">
				<input type="hidden" name="commit" />
				<input type="hidden" name="servicename" value="<%= request.getParameter("servicename") %>" />
				<input type="hidden" name="command" value="<%= request.getParameter("command") %>" />
				<input type="submit" value="Commit GAF" />
			</form>
			
			<%
		}
	}
	%>


</body>
</html>