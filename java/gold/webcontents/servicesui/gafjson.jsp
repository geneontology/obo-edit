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
		out.println("{ \"QcChecksOutput\": {");
		
		if(task != null && annotationRuleViolations != null){
		//	PrintWriter pw = new PrintWriter(out);
			while(task.isRunning()){
				try{
					Thread.sleep(1000);
				}catch(Exception e){
					
					out.println("\t{\"error\": \""+ e.getMessage() +"\"},");

					//ex.printStackTrace(pw);
				}
			}
			
			
			ex = task.getException();
		
			if(ex != null){
				out.println("\t{\"error\": \""+ ex.getMessage() +"\"},");
				//	ex.printStackTrace(pw);
				//pw.flush();
			}
			if(annotationRuleViolations != null){
				out.println("\t\"voilations\": {");
				int i=0;
				int Annsize = annotationRuleViolations.size();
				for(AnnotationRuleViolation v: annotationRuleViolations){
					String msg = v.getMessage();
					msg = msg.replaceAll("\t", "\\\\t");
					String s = v.getSourceAnnotation() + "";
					s = s.replaceAll("\t", "\\\\t");
					
					out.println("\t\t\"voilation\": {");
					out.println("\t\t\t\"message\": \"" + msg + "\",");
					out.println("\t\t\t\"annotation\": \"" + s + "\"");
					out.println("\t\t\t\"rule-id\": \"" + v.getRuleId() + "\"");
					out.println("\t\t}");
					i++;
					
					if(i < Annsize){
						out.println("\t\t,");
					}
	
				}
				out.println("\t}");
						
			}
			
		}else{
			out.println("\t{\"error\": \"The server internal error: Please contect to the system administrator.\"}");
		}

		out.println("\t}");
		out.println("}");
		
	
%>


