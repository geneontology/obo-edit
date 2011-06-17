<%@page import="org.geneontology.gaf.hibernate.GeneAnnotation"%>
<%@page import="java.io.PrintWriter"%>
<%@page import="org.geneontology.gold.rules.AnnotationRuleViolation"%>
<%@page import="java.util.Set"%>
<%@page import="org.geneontology.web.Task"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1" trimDirectiveWhitespaces="true"%>

	<%
	
		Task task = (Task)request.getAttribute("task");
		Set<AnnotationRuleViolation> annotationRuleViolations = (Set<AnnotationRuleViolation>)request.getAttribute("violations");

		
		
		Throwable ex = null;
		
		if(task != null && annotationRuleViolations != null && annotationRuleViolations.size()>0){
			
			ex = task.getException();
		
			if(ex != null){
				PrintWriter pw = new PrintWriter(out);
				ex.printStackTrace(pw);
				pw.flush();
			}
		//	if(annotationRuleViolations != null){
				for(AnnotationRuleViolation v: annotationRuleViolations){
					String msg = v.getMessage();
					String s = v.getSourceAnnotation() + "";
					String ruleId = v.getRuleId();
					GeneAnnotation ga = v.getSourceAnnotation();
					String lineNr = "";
					if(ga != null)
						lineNr =  ga.getSource() != null ? ga.getSource().getLineNumber() + "" : "";
					%>
                    <li>
                    	<div style="font-size: 1.1em;font-weight:bold">Rule Id: <%=ruleId  %> --- Line Number: <%=lineNr  %> --- <%= msg %> </div>
                    	    <ul>
                    	       <li>
                    				<div style="color:red"><%= s  %> </div>
                    			</li>
                    		</ul>
                    </li>
                    <%
				}
				annotationRuleViolations.clear();
			//}
			
		}else if(task == null || (task != null && !task.isRunning())){
			%>NO_DATA<%
		}
		
%>


