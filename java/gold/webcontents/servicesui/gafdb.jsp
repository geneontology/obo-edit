<%@page import="java.util.ArrayList"%>
<%@page import="java.util.List"%>
<%@page import="java.util.Hashtable"%>
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

	<h1>Status of the execution of the <%= request.getParameter("command") %>' command on the GAF database.</h1>

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
			ex = (Throwable)request.getAttribute("exception");
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
	
		if(!isTaskRunning && ex == null){
		
			%>
			<hr />
			
			
			<div>Select one option from the following options</div>
			
			<br />		
			<form action=".">
				<input type="hidden" name="runrules" />
				<input type="hidden" name="servicename" value="<%= request.getParameter("servicename") %>" />
				<input type="hidden" name="command" value="<%= request.getParameter("command") %>" />
				<input type="hidden" name="id" value="<%= request.getParameter("id") %>" />	
				<input type="submit" value="Run Annotation Rules" />
			</form>
			
			<br />

			<form action=".">
				<input type="hidden" name="commit" />
				<input type="hidden" name="servicename" value="<%= request.getParameter("servicename") %>" />
				<input type="hidden" name="id" value="<%= request.getParameter("id") %>" />	
				<input type="hidden" name="command" value="<%= request.getParameter("command") %>" />
				<input type="submit" value="Save GAF into database" />
			</form>
			
			<br />

			<form action=".">
				<input type="hidden" name="solrload" />
				<input type="hidden" name="commit" />
				<input type="hidden" name="servicename" value="<%= request.getParameter("servicename") %>" />
				<input type="hidden" name="command" value="<%= request.getParameter("command") %>" />
				<input type="hidden" name="id" value="<%= request.getParameter("id") %>" />	
				<input type="submit" value="Load GAF into Solr" />
			</form>

			<%
		}
		
		
		if(annotationRuleViolations != null){
			boolean runRules  = request.getParameter("runrules")== null ? false: true;
			String heading = "Annotation Violations";
			if(!runRules){
				heading = "Annotaiton Parsing Errors";
			}
			%>
		
				<hr />
				<h3><%= heading %></h3>
				<%
				Hashtable<String, List<AnnotationRuleViolation>> table = new Hashtable<String, List<AnnotationRuleViolation>>();
				for(AnnotationRuleViolation v: annotationRuleViolations){
					String ruleId = v.getRuleId() + "";
					List<AnnotationRuleViolation> list = table.get(ruleId);
					
					if(list == null){
						list = new ArrayList<AnnotationRuleViolation>();
						table.put(ruleId, list);
					}
					
					list.add(v);
				
				}
				
				%>
				<ul>

					<%
					for(String rule: table.keySet()){
						List<AnnotationRuleViolation> list = table.get(rule);
						for(AnnotationRuleViolation v: list){
							%>
							<li>
								<div style="font-size: 1.1em;font-weight:bold"><%=v.getRuleId()  %> ---- <%= v.getMessage() %> </div>
								<ul>
									<li>
										<div style="color:red"><%=v.getAnnotationRow()  %> </div>
									</li>
								</ul>
							</li>
							<% 
						}
						
						
					}
					
					%>
		
				</ul>
			
			<%
			}
		
	}
	%>


</body>
</html>