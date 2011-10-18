<%@page import="java.util.List"%>
<%@page import="org.geneontology.web.services.GafDbOperationsService"%>
<%@page import="org.geneontology.gaf.hibernate.GeneAnnotation"%>
<%@page import="java.io.PrintWriter"%>
<%@page import="org.geneontology.web.services.Task"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
	<title>Gold Db Operation Status</title>

	<script type="text/javascript" src="/scripts/jquery-1.5.1.min.js" language="javascript"></script>
	<script type="text/javascript" src="/scripts/gold.js" language="javascript"></script>


</head>
<body>

	<%
		boolean  isTaskRunning = (Boolean)request.getAttribute("isTaskRunning");
//		boolean  isLargeFile = (Boolean)request.getAttribute("isLargeFile");
		boolean runAnnotationRules = "runrules".equals(request.getParameter("command"));
//		boolean commit = "commit".equals(request.getParameter("command"));
		String id = request.getParameter("id");
		HttpSession s = request.getSession(true);
		if(id == null){
			id = s.getId();
		}

		if(isTaskRunning){
	%>
			<center><img class="progress" src="/images/progress-indicator.gif" alt="Request is in Progress" /></center>
			<p class="progress" align="center">Your Request is in Progress</p>

			
			<% if(!runAnnotationRules){ %>
			
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
						setTimeout('submitForm()', 30000);
						
						function submitForm(){
							document.forms[0].submit();
						}
						
					</script>
	
	<%
			}
		}else if(GafDbOperationsService.isDbUpdateInProgress()){
			%>
			
			<h1>DB Update is already in progress. Please the call the update operation later.</h1>
			<%
		}

	%>

	<h1>Status of the execution of the <%= request.getParameter("command") %> command on the GAF database.</h1>

	<%
		Task task = (Task)request.getAttribute("task");
	%>

	<table><tr><th>Operation Name</th><th>Status/Completion Time</th></tr>
	<% 		
		List<Throwable> ex = null;
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
				} else if (ex != null && !ex.isEmpty()) {
					status = "failed";
				}
			%>
				<tr><td> <%= opLocalName %>  </td><td bgcolor='<%= isCompleted ? "green" : "" %>' class="inprogress" > <%= status %> </td></tr>
			<% 			
			}
	
		}
		
		if(ex == null){
			ex = (List<Throwable>)request.getAttribute("exception");
		}
		%>	
	</table>

	<%
	if(ex != null){
		PrintWriter pw = new PrintWriter(out);
		
		for(Throwable t: ex){
			t.printStackTrace(pw);
			pw.println("<br /><hr />");
		}
		pw.flush();
	}
	
	%>

	<%
//	if(!commit){
	
	
		if(runAnnotationRules && ( ex == null || (ex != null && ex.isEmpty()) )){
			%>
			
					<script type='text/javascript'>
						var interval= setInterval('getVoilations()', 20000);
						//var isTaskRunning = true;
						function getVoilations() {
							
						//	alert("before condition");
							//if(isTaskRunning){
								//alert("before condition");
								jQuery.getJSON(
									  window.location.pathname + window.location.search + "&view=annotationchecks&id=<%=id%>",
									  function(data) {


										  if (typeof(data.error) !== "undefined") {
												if(data.error = "task_completed"){
													  clearInterval(interval);
													  jQuery(".progress").hide();
													  jQuery(".commands").show();
													  jQuery(".inprogress").attr('bgcolor', 'green');
												}
										  }else{
											  printVoilations(data);										  
										  }
										//  alert(data);
										 // alert(data.items);
										 /* jQuery.each(data, function(i,item){
												jQuery('#voilations').append('<li>'+item.message+'</li>');
										  });
										  
										jQuery('.totalvoilations').html(jQuery('#voilations').children().size());
											*/
										//  data = jQuery.trim(data) + "";
									//	  alert('Load was performed.: "' + data + '"');
										//  var error = data.indexOf('ServletException');
										  
										 /*if(error == -1){
											  error = data.indexOf('AnnotationRuleCheckException');
										 }*/
										  
										/*  if(data =='NO_DATA$' || error != -1){
											//  isTaskRunning = false;  
											  clearInterval(interval);
											  jQuery(".progress").hide();
											  jQuery(".commands").show();
											  jQuery(".inprogress").attr('bgcolor', error != -1 ? 'red' : 'green')
											  	.html(error != -1 ? 'failed' : 'completed');
											  if(error == -1)
											  	data = "";
											  
											  
										  }*/
									  }/*,
									  
									  
									  error:function (xhr, ajaxOptions, thrownError){
										  clearInterval(interval);
										  jQuery(".progress").hide();
										  jQuery(".commands").show();
										  jQuery(".inprogress").attr('bgcolor', 'red')
										  	.html('failed');

										  jQuery('#voilations').parent().append('<div class="server-error">'+xhr.responseText+'</div>');
						                }*/    									  
									);
							//}
						}
						
					</script>
			<% } %>
			
			<br />

			<div class="voilations">
			</div>
			
			<%
			
			//Set<AnnotationRuleViolation> annotationRuleViolations = (Set<AnnotationRuleViolation>)request.getAttribute("violations");
			//List<AnnotationVoilationForJson> list = new ArrayList<AnnotationVoilationForJson>();
			if(!runAnnotationRules){
				%>
			<script type="text/javascript">
				var annotationVoilations= <jsp:include page="/servicesui/print_annotations_voilations.jsp"/>;
		 		printVoilations(annotationVoilations);
	 		</script>
				
				<%
			}
			
			%>
			
			
			
</body>
</html>