<%@page import="java.io.PrintWriter"%>
<%@page import="org.geneontology.gold.rules.AnnotationRuleViolation"%>
<%@page import="java.util.Set"%>
<%@page import="org.geneontology.web.Task"%>
<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1" trimDirectiveWhitespaces="true"%>

	<%
		Task task = (Task)request.getAttribute("task");
		Set<AnnotationRuleViolation> annotationRuleViolations = (Set<AnnotationRuleViolation>)request.getAttribute("violations");
		String sessionId = request.getParameter("id");
		Throwable ex = null;
		
		
	///	out.println("{ \"QcChecksOutput\": {");
		
		if(annotationRuleViolations == null){
			out.println("\t{\"error\": \"The server internal error: Please contect to the system administrator.\"}");
		}else if(sessionId != null && task != null){		
		
			if(annotationRuleViolations != null){
			//	PrintWriter pw = new PrintWriter(out);
		///		out.println("\t\"voilations\": {");
		
			//	while(task.isRunning()){
					/*	try{
							Thread.sleep(120000);
						}catch(Exception e){
							
							out.println("\t{\"error\": \""+ e.getMessage() +"\"},");
		
							//ex.printStackTrace(pw);
						}*/
					
					
					
						ex = task.getException();
					
						if(ex != null){
							out.println("\t{\"error\": \""+ ex.getMessage() +"\"},");
							//	ex.printStackTrace(pw);
							//pw.flush();
						}else{
						//if(annotationRuleViolations != null){
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
							
							System.out.println("......................Clearing the annotations: " + annotationRuleViolations.size());
							annotationRuleViolations.clear();
						}
						
						
					//	}
					
					
				//	}
					
				///out.println("\t}");
				
				
				
				
				
				
			}
		}else  if(task == null || (task != null && !task.isRunning()))
		   out.println("NO_DATA");
		else if(sessionId == null){
			sessionId = request.getSession(true).getId();
			
			out.println("session:"+sessionId);
		}
	   
	
%>


