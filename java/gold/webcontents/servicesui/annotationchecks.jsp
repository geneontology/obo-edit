<%@page import="java.util.Hashtable"%>
<%@page import="com.google.gson.Gson"%>
<%@page import="org.geneontology.gold.rules.json.ErrorForJson"%>
<%@page import="org.geneontology.gold.rules.AnnotationRuleViolation"%>
<%@page import="java.util.Set"%>
<%@page import="org.geneontology.web.services.Task"%>
<%@page import="java.io.PrintWriter"%>
<%@page import="java.util.List"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1" trimDirectiveWhitespaces="true"%>

	<%
	
		Task task = (Task)request.getAttribute("task");
		Hashtable<String, Set<AnnotationRuleViolation>> annotationRuleViolations= (Hashtable<String, Set<AnnotationRuleViolation>>)request.getAttribute("violations");

		request.setAttribute("removeAnnotations", new Boolean(true));
		
		List<Throwable> ex = null;
		
		if(task != null){
			
			ex = task.getException();
		
			if(ex != null && !ex.isEmpty()){
				PrintWriter pw = new PrintWriter(out);
				for(Throwable t: ex)
					t.printStackTrace(pw);
			
				pw.flush();
			}else if(annotationRuleViolations !=null && !annotationRuleViolations.isEmpty()){
				request.setAttribute("removeAnnotations", new Boolean(true));
				%>
				<jsp:include page="/servicesui/print_annotations_voilations.jsp"/>
				<%
			}else if(task == null || !task.isRunning()){
				ErrorForJson err = new ErrorForJson("task_completed");
				Gson gson = new Gson();
				out.println(gson.toJson(err));
			}
			
		}
	
%>


